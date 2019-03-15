source("/home/guilherme/GitLab/src-website/content/private_scripts/mtps_monetdb.R" , echo = FALSE )

library(data.table)
library(fst)

library(doParallel)
library(plyr)

nodes <- detectCores()
cl <- makeCluster(nodes)
registerDoParallel(cl)

# stopCluster(cl)


# output_dir <- "/Volumes/Trabalho/MTPS"
output_dir <- "~/Bases/MTPS"
# unlink( output_dir , recursive = TRUE )
# datavault_dir <- "/Volumes/DataVault/TESTE"
# unlink( datavault_dir , recursive = TRUE )
datavault_dir <- "~/DataVault/MTPS"

catalog_init <- catalog_mtps( output_dir = output_dir )
catalog_dv <- datavault_mtps( catalog = catalog_init , datavault_dir = datavault_dir )

stopCluster(cl)

catalog <- catalog_dv

batch.size = 100000

### build dataset

tf <- tempfile()
td <- tempdir()

for ( i in seq_len( nrow( catalog ) ) ){
  
  if ( !file.exists( catalog[i , "output_filename" ] ) ) {
    
    # download the file
    if ( is.null( catalog[ i , "datavault_file" ] ) ) {
      download.file( catalog[ i , "full_url" ] , tf , quiet = TRUE , mode = "wb" )
    } else if ( is.na( catalog[ i , "datavault_file" ] ) ) {
      download.file( catalog[ i , "full_url" ] , tf , quiet = TRUE , mode = "wb" ) 
    } else {
      file.copy( catalog[ i , "datavault_file" ] , tf )
    }
    
    # extract file
    archive::archive_extract( normalizePath( tf ) , dir = file.path( td , "unzipped" ) )
    
    # list files files
    this_data_file <- list.files( file.path( tempdir() , "unzipped" ) , full.names = TRUE )
    this_data_file <- grep( "\\.csv|\\.txt$", this_data_file, value = TRUE, ignore.case = TRUE )
    
    # faster read file
    x <- tryCatch( suppressWarnings( data.table::fread( this_data_file , sep = ";" , encoding = "Latin-1" , dec = "," , na.strings = "-1" , showProgress = FALSE , data.table = TRUE , stringsAsFactors = FALSE , fill = TRUE , strip.white = TRUE , colClasses = "character" ) ) , 
                   error = function(e) utils::read.csv( this_data_file , sep = ";" , dec = "," , header = TRUE , fileEncoding = "latin1" , as.is = TRUE , colClasses = "character" ) )
    
    # coerce to data.table
    x <- data.table::as.data.table( x ) ; gc()
    
    # convert all column names to lowercase
    names( x ) <- tolower( names( x ) )
    
    # remove special characters
    names( x ) <- remove_special_character( names( x ) )
    
    # remove trailing spaces
    names( x ) <- trimws( names( x ) , which = "both" )
    
    # remove special characters
    names( x ) <- gsub( "\\.$|\\(|\\)" , "" , names( x ) )
    
    # change dots and spaces for underscore
    names( x ) <- gsub( "\\.| |\\/|\\-" , "_" , names( x ) )
    
    # make duplicated colnames unique
    new_cols <- make.unique( colnames(x) , sep = "_" )
    names(x) <- new_cols
    
    # figure out which columns really ought to be numeric
    if (catalog[i,"type"] %in% c( "caged" , "caged_ajustes" ) ) {
      this_pattern <- "cnae|cbo|regiao|municipio|tipo|regioes|ibge|uf|faixa|grau|ind_|sexo|bairro|distrito|raca_cor|competencia|admitidos|saldo"
      these_cols <- names( x )[ !grepl( this_pattern , names(x) ) ]
    } else {
      this_pattern <- "^(qtd|vl|tempo|rem)"
      these_cols <- names( x )[ grepl( this_pattern , names(x) ) ]
    }
    gc()
    x[ , (these_cols) := lapply( .SD , string_to_num_with_commas ) , .SDcols = these_cols ] ; gc()
    
    # force uf to numeric
    suppressWarnings( x[ , uf := as.numeric( substr( municipio ,1,2) ) ] )
    
    # store case count
    catalog[ i , 'case_count' ] <- nrow( x )
    
    # save file
    if ( !dir.exists( dirname( catalog[ i , 'output_filename' ] ) ) ) dir.create( dirname( catalog[ i , 'output_filename' ] ) , recursive = TRUE )
    write.fst( x , path = catalog[ i , 'output_filename' ] , compress = 100 )
    
    # remove object and clear RAM
    rm( x ) ; gc()
    
    # remove temporary files
    unlink( file.path( tempdir() , "unzipped" ) , recursive = TRUE )
    file.remove( tf )
    
    # save catalog tracker
    saveRDS( catalog , file = file.path( output_dir , "mtps.Rds" ) , compress = TRUE )
    
  }
  
  # process tracker
  cat( paste0( "mtps catalog entry " , i , " of " , nrow( catalog ) , " stored at '" , catalog[ i , 'output_filename' ] , "'\r" ) )
  
  # if ( catalog$type[i] %in% "rais" & catalog$subtype[i] %in% "vinc" ) { next() }
  
  # if this is the final catalog entry for the unique db_tablename, then write them all to the database
  if ( !is.na( catalog$db_tablename[[i]] ) & ( i == max( which( catalog$db_tablename == catalog[ i , 'db_tablename' ] ) ) ) ) {
    
    cat( "\n" )
    
    # open the connection to the monetdblite database
    db <- DBI::dbConnect( MonetDBLite::MonetDBLite() , catalog[ i , 'dbfolder' ] )
    
    # if table exists, remove it from database
    if ( catalog$db_tablename[ i ] %in% DBI::dbListTables( db ) ) { DBI::dbRemoveTable( db , catalog$db_tablename[ i ] ) }
    
    # list all files for this database
    these_files <- catalog[ catalog$db_tablename %in% catalog[ i , 'db_tablename' ] , 'output_filename' ]
    
    # loop through all files and collect column names and formats
    these_dts <- plyr::llply( these_files , function(this_file) {
      
      # skip unexisting files
      if ( !file.exists( this_file ) ) return( NULL )
      
      # read file
      x <- read_fst( this_file , as.data.table = TRUE , from = 1 , to = 1 ) 
      
      # collect columns
      these_cols <- colnames( x )
      
      # collect formats
      these_formats <- sapply( x , typeof )
      
      # set up data.table
      data.table( filename = this_file , column_name = these_cols , column_format = these_formats )
      
    } )
    
    # stack files
    data_structure <- rbindlist( these_dts , use.names = TRUE )
    
    # reshape data structure
    data_structure <- dcast( data_structure , column_name ~ column_format , fill = "double" , drop = FALSE , value.var = "column_format" , fun.aggregate = unique )
    
    # define final col_format
    data_structure <- as.data.frame( data_structure )
    data_structure$col_format <- apply( data_structure[ , -1 ] , 1 , function( y ) { ifelse( any( y %in% c( "character", "date" ) ) , "character" ,"numeric" ) } )
    
    # build final structure
    data_structure <- data_structure[ , c( "column_name" , "col_format" ) ]
    
    # read file, format columns and write it to table
    for ( this_file in these_files ) {
      
      filemeta <- metadata_fst( this_file )
      max.row <- filemeta[2][[1]]
      
      # store case count
      catalog[ i , 'case_count' ] <- max.row
      
      # read data in chunks
      j = 1
      start.row = 1
      while ( j <= ceiling( max.row/batch.size ) ) {
        
        # process files in batches
        end.row <- ifelse( start.row + batch.size - 1 <= max.row , start.row + batch.size - 1 , max.row )
        x <- read_fst( this_file , as.data.table = TRUE , from = start.row , to = end.row )
        start.row = start.row + batch.size
        
        # create missing columns
        missing_cols <- data_structure$column_name [ !is.element( data_structure$column_name , colnames(x) ) ]
        if ( length( missing_cols ) > 0 )  x[ , (missing_cols) := NA ]
        
        # fix formats
        these_num_cols <- data_structure$column_name[ data_structure$col_format == "numeric" ]
        x[ , (these_num_cols) := lapply( .SD , as.numeric ) , .SDcols = these_num_cols ]
        these_char_cols <- data_structure$column_name[ data_structure$col_format == "character" ]
        x[ , (these_char_cols) := lapply( .SD , as.character ) , .SDcols = these_char_cols ]
        
        # reorder column names
        length(data_structure$column_name)
        length(colnames(x))
        setcolorder( x , data_structure$column_name )
        
        # write table to database
        DBI::dbWriteTable( db , catalog[ i, "db_tablename" ] , x , append = TRUE )
        
        # remove object and clear RAM
        rm( x ) ; gc()
        
        # iterattion
        # cat( j , "out of", ceiling( max.row/batch.size ) , "\r" )
        j=j+1
        
      }
      
      # process tracker
      cat( basename( this_file ) , "stored at" , catalog[ i, "db_tablename" ] , "\r" )
      
    }
    
    cat( "\n" )
    
  }
  
}

# disconnect from the current monet database
DBI::dbDisconnect( db , shutdown = TRUE )

# return catalog
catalog

# save catalog
saveRDS( catalog , file = file.path( output_dir , "mtps.Rds" ) , compress = TRUE )