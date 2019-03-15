source("/home/guilherme/GitLab/src-website/content/private_scripts/datasus_monetdb.R" , echo = FALSE )

library(data.table)
library(read.dbc)
library(fst)

library(doParallel)
library(plyr)

nodes <- detectCores()
cl <- makeCluster(nodes)
registerDoParallel(cl)

# stopCluster(cl)


output_dir <- "/home/guilherme/Bases/DataSUS"
# output_dir <- "/Volumes/DataVault/TESTE"
# unlink( output_dir , recursive = TRUE )
# datavault_dir <- "/Volumes/DataVault/TESTE"
# unlink( datavault_dir , recursive = TRUE )
datavault_dir <- "/Volumes/DataVault/DataSUS"

catalog_init <- catalog_datasus( output_dir = output_dir )
catalog_dv <- datavault_datasus( catalog = catalog_init , datavault_dir = datavault_dir )

stopCluster(cl)

catalog <- catalog_dv
# catalog <- catalog[ catalog$db_tablename == catalog$db_tablename[[1]] , ]
# catalog <- catalog[ catalog$year < 1996 , ]


### build dataset

tf <- tempfile()

for ( i in seq_len( nrow( catalog ) ) ){
  
  # download the file
  if ( is.null( catalog[ i , "datavault_file" ] ) ) {
    download.file( catalog[ i , "full_url" ] , tf , quiet = TRUE , mode = "wb" )
  } else if ( is.na( catalog[ i , "datavault_file" ] ) ) {
    download.file( catalog[ i , "full_url" ] , tf , quiet = TRUE , mode = "wb" ) 
  } else {
    file.copy( catalog[ i , "datavault_file" ] , tf , overwrite = TRUE )
  }
  
  # read dbc file
  x <- read.dbc( tf , as.is = TRUE )
  
  # convert to data.table
  x <- data.table( x )
  
  # convert all column names to lowercase
  names( x ) <- tolower( names( x ) )
  
  # add underscores after monetdb illegal names
  for ( j in names( x )[ toupper( names( x ) ) %in% getFromNamespace( "reserved_monetdb_keywords" , "MonetDBLite" ) ] ) names( x )[ names( x ) == j ] <- paste0( j , "_" )
  
  # remove trailing spaces
  names( x ) <- trimws( names( x ) , which = "both" )
  
  # force code variables to character
  code_vars <- names( x )[ grepl( "^(cod|causabas|linha|ocup|dt|numero|idade|sexo)" , names( x ) ) ]
  x[ , (code_vars) := lapply( .SD , as.character ) , .SDcols = code_vars ]
  
  # fix 2014 format
  if ( "sexo" %in% names(x) ) {
    x[ !( sexo %in% c("1","2","M","F") ) , sexo := NA ]
    x[ sexo == "M" , sexo := 1 ] ; x[ sexo == "F" , sexo := 2 ] 
  }
  
  # figure out which columns really ought to be numeric
  for( this_col in names( x )[ !grepl( "^(cod|causabas|linha|ocup|dt|numero|idade|sexo)" , names( x ) ) ] ){
    
    # if the column can be coerced without a warning, coerce it to numeric
    this_result <- tryCatch( as.numeric( x[[this_col]] ) , warning = function(c) NULL , error = function(c) NULL )
    
    if( !is.null( this_result ) ) x[ , (this_col) := lapply( .SD , as.numeric ) , .SDcols = this_col ]
    
  }
  
  # save case count
  catalog[ i , 'case_count' ] <- nrow( x )
  
  # write file to disk
  if ( !dir.exists( dirname( catalog[ i , 'output_filename' ] ) ) ) dir.create( dirname( catalog[ i , 'output_filename' ] ) , recursive = TRUE )
  fst::write_fst( x , path = catalog[ i , 'output_filename' ] , compress = 100 )
  
  # process tracker
  cat( paste0( "datasus catalog entry " , i , " of " , nrow( catalog ) , " stored at '" , catalog[ i , 'output_filename' ] , "'\r" ) )
  
  # if this is the final catalog entry for the unique db_tablename, then write them all to the database
  if ( i == max( which( catalog$db_tablename == catalog[ i , 'db_tablename' ] ) ) ) {
    
    cat( "\n" )
    
    # open the connection to the monetdblite database
    db <- DBI::dbConnect( MonetDBLite::MonetDBLite() , catalog[ i , 'dbfolder' ] )
    
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
      
      # read file
      x <- read_fst( this_file , as.data.table = TRUE ) 
      
      # create missing columns
      missing_cols <- data_structure$column_name [ !( data_structure$column_name %in% colnames(x) ) ]
      x[ , (missing_cols) := NA ]
      
      # fix formats
      these_num_cols <- data_structure$column_name[ data_structure$col_format == "numeric" ]
      x[ , (these_num_cols) := lapply( .SD , as.numeric ) , .SDcols = these_num_cols ]
      these_char_cols <- data_structure$column_name[ data_structure$col_format == "character" ]
      x[ , (these_char_cols) := lapply( .SD , as.character ) , .SDcols = these_char_cols ]
      
      # reorder column names
      setcolorder(x , data_structure$column_name )
      
      # write table to database
      DBI::dbWriteTable( db , catalog[ i, "db_tablename" ] , x , append = TRUE )
      rm( x ) ; gc()
      
      # process tracker
      cat( basename( this_file ) , "stored at" , catalog[ i, "db_tablename" ] , "\r" )
      
    }
    cat( "\n" )
    
    # disconnect from the current monet database
    DBI::dbDisconnect( db , shutdown = TRUE )
    
  }
  
  # delete the temporary files
  suppressWarnings( file.remove( tf ) )
  
}

catalog