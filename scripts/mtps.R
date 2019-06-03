string_to_num_with_commas <- function( string ) {
  tryCatch( as.numeric( gsub( ",", "." , string ) ) , error=function(e) string )
}

get_common_dir <- function(paths, delim = "/") {
  path_chunks <- strsplit(paths, delim)
  
  i <- 1
  repeat({
    current_chunk <- sapply(path_chunks, function(x) x[i])
    if(any(current_chunk != current_chunk[1])) break
    i <- i + 1
  })
  paste(path_chunks[[1]][seq_len(i - 1)], collapse = delim)
  
}

# recursive ftp scrape
link_scrape <- function( x ) {
  library(RCurl)
  custom_recodes <- c( "í" = "%ED" , "á" = "%E1" , "ã" = "%E3" , " " = "%20" )
  if ( !is.null( x ) ){
    Sys.sleep(runif(1,.2,1))
    h <- getCurlHandle()
    this_text <- getURL( x , curl = h , .encoding = 'ISO-8859-1' , .opts = list( timeout = 240 , dirlistonly = TRUE , ftplistonly = TRUE , ftp.use.epsv = FALSE ) )
    reset(h)
    this_text <- unlist( strsplit( this_text , ifelse( Sys.info()["sysname"] == "Windows" , "\r\n" , "\n" ) ) )
    this_text <- curlPercentEncode( this_text , codes = custom_recodes )
    paste0( x , this_text )
  } else { 
    NULL 
  }
}

getlisting <- function( these_urls ) {
  library(future.apply)
  these_urls <- ifelse( grepl( "\\/$" , these_urls ) , these_urls , paste0( these_urls , "/" ) )
  res <- future_lapply( these_urls , link_scrape )
  res <- unlist( res )
  res <- res[ !grepl( "\\/EEC" , res ) ] # drop EEC folder
  is.file <- grepl( "\\." , basename( res ) , ignore.case = TRUE )
  res[ !is.file ] <- ifelse( grepl( "\\/$" , res[ !is.file ] ) , res[ !is.file ] , paste0( res[ !is.file ] , "/" ) )
  list( dirs = if ( any(!is.file) ) { res[ !is.file ] } else { NULL } , 
        files = if ( any(is.file) ) { res[ is.file ] } else { NULL } )
}

recursive_ftp_scrape <- function( main_url , max.iter = Inf ) {
  
  final_files <- NULL
  
  directories <- main_url
  
  i=0
  while ( length(directories) > 0 ) {
    i=i+1
    if ( i > max.iter ) { break() }
    listing <- getlisting(directories)
    directories <- listing[[1]]
    files <- listing[[2]]
    final_files <- c(final_files, files )
    if ( length( directories) > 0 ) cat( length(directories) , "new directories found.\n" ) else cat( "done!\n" )
  }
  
  return( final_files )
  
}

remove_special_character <- function(x) {
  stripped_x <- iconv( x , to = "ascii//translit" , sub = "_" )
  stripped_x <- gsub( "('|~|\\^)" , "" , stripped_x , ignore.case = TRUE )
  stripped_x
}


catalog_mtps <-
  function( output_dir , ... ){
    
    # fix path
    output_dir <- ifelse( !grepl( "/$" , output_dir ) , paste0( output_dir , "/" ) , output_dir )
    
    # set main ftp directory
    url_path <- "ftp://ftp.mtps.gov.br/pdet/microdados/"
    
    # scrape files
    mtps_files <- recursive_ftp_scrape( url_path )
    
    # # drop "caged_ajustes"
    # mtps_files <- mtps_files[ !grepl( "ajustes" , dirname( mtps_files ) , ignore.case = TRUE ) ]
    
    # keep data files only
    mtps_files <- mtps_files[ grepl( "\\.(7z|zip)$" , basename( mtps_files ) , ignore.case = TRUE ) ]
    
    # create catalog
    catalog <-
      data.frame(
        full_url = mtps_files ,
        stringsAsFactors = FALSE
      )
    
    # define types
    catalog$type <-
      ifelse( grepl( "layout|\\.(xls|pdf)" , catalog$full_url , ignore.case = TRUE ) , "docs" ,
              ifelse( grepl( "RAIS/" , catalog$full_url ) , "rais" ,
                      ifelse( grepl( "CAGED/" , catalog$full_url ) , "caged" , 
                              ifelse( grepl( "CAGED_AJUSTES/" , catalog$full_url ) , "caged_ajustes" , NA ) ) )
      )
    
    # define subtypes
    catalog$subtype[ catalog$type %in% c( "caged" , "caged_ajustes" ) ] <- NA
    catalog$subtype[ catalog$type == "rais" ] <- 
      ifelse( grepl( "estb" , basename( catalog$full_url ) , ignore.case = TRUE ) , "estb" , "vinc" )[ catalog$type == "rais" ]
    
    # define output filenames
    catalog$output_filename <- gsub( "\\.(7z|zip)$" , ".fst" ,
                                     gsub( url_path , output_dir , tolower( catalog$full_url ) , ignore.case = TRUE ) ,
                                     ignore.case = TRUE )
    catalog$output_filename <- sapply( catalog$output_filename , URLdecode )
    
    # define years and months
    these_years <- gsub( "[A-Z]{1-20}|\\..*|_" , "" , basename( catalog[ , "full_url" ] ) ) [ catalog$type %in% c( "caged" , "caged_ajustes" ) ]
    these_years <- ifelse( nchar( these_years ) > 4 , substr( these_years , 3, 6 ) , these_years )
    catalog$year [ catalog$type %in% c( "caged" , "caged_ajustes" ) ] <- as.numeric( these_years )
    
    these_months <- gsub( "[A-Z]{1-20}|\\..*|_" , "" , basename( catalog[ , "full_url" ] ) ) [ catalog$type %in% c( "caged" , "caged_ajustes" ) ]
    these_months <- ifelse( nchar( these_months ) > 4 , substr( these_months , 1, 2 ) , NA )
    catalog$month [ catalog$type %in% c( "caged" , "caged_ajustes" ) ] <- as.numeric( these_months )
    
    these_years <- gsub( "[A-Z]{1-20}|\\..*|_" , "" , basename( catalog[ , "full_url" ] ) , ignore.case = TRUE ) [ catalog$type %in% "rais" ]
    catalog$year [ catalog$type %in% "rais" ] <- as.numeric( these_years )
    
    # define tablenames
    catalog$db_tablename[ catalog$type %in% c( "caged" , "caged_ajustes" ) ] <- paste( catalog$type , catalog$year , sep = "_" ) [ catalog$type %in% c( "caged" , "caged_ajustes" ) ]
    catalog$db_tablename[ catalog$type %in% "rais" ] <- paste( "rais" , catalog$subtype , catalog$year , sep = "_" )[ catalog$type %in% "rais" ]
    
    # define database folder
    catalog$dbfolder <- ifelse( is.na( catalog$db_tablename ) , NA , paste0( output_dir , "MonetDB/" ) )
    
    catalog
    
  }

# build datavault
datavault_mtps <- function( catalog , datavault_dir , skipExist = TRUE ) {
  
  # get common directory
  url_path <- get_common_dir( tolower( catalog$full_url ) )
  
  # create datavault links
  catalog$datavault_file <- gsub( url_path , paste0( datavault_dir , "/" ), tolower( catalog$full_url ) , ignore.case = TRUE )
  catalog$datavault_file <- gsub( "//" , "/" , catalog$datavault_file , ignore.case = TRUE )
  
  # check for existing files
  existing_files <- file.exists( catalog$datavault_file )
  
  # if there isn't any non-downloaded, run download procedure:
  if ( any( !existing_files ) ) {
    
    # print message
    cat( sum( 1*!existing_files ) , "missing files detected.\nDownloading missing files only.\n")
    
    # download files to datavault directory
    for ( i in seq_len( nrow(catalog) )[ !existing_files ] ) {
      
      # skip existing file
      if( skipExist & existing_files[ i ] ) next()
      
      # download file
      # create directories
      dir.create( dirname( catalog$datavault_file[ i ] ) , recursive = TRUE , showWarnings = FALSE ) 
      download.file( catalog$full_url[ i ] , catalog$datavault_file[ i ] , quiet = FALSE )
      
      # process tracker
      cat( "file" , i , "out of" , nrow( catalog ) , "downloaded to" , datavault_dir , "\r")
      
    }
    
  }
  
  # print message
  cat( "\nmtps datavault was built at" , datavault_dir , "\n" )
  
  # return catalog
  catalog
  
}

# build database
build_mtps <-
  function( catalog ){
    
    # load libraries
    library(archive)
    library(data.table)
    library(fst)
    library(DBI)
    library(MonetDBLite)
    
    # check single database
    if ( length( unique( catalog[ !is.na( catalog[ , "dbfolder" ] ) , "dbfolder" ] ) ) > 1 ) { stop( "Non-unique dbfolder." ) }
    
    # create temporary items
    tf <- tempfile()
    td <- tempdir()
    
    # loop through catalog entries
    for ( i in seq_len( nrow( catalog ) ) ){
      
      # download the file
      if ( is.null( catalog[ i , "datavault_file" ] ) ) {
        download.file( catalog[ i , "full_url" ] , tf , quiet = TRUE , mode = "wb" )
      } else if ( is.na( catalog[ i , "datavault_file" ] ) ) {
        download.file( catalog[ i , "full_url" ] , tf , quiet = TRUE , mode = "wb" ) 
      } else {
        file.copy( catalog[ i , "datavault_file" ] , tf )
      }
      
      if ( !grepl( ".7z$|.zip$" , catalog[ i , 'full_url' ] , ignore.case = TRUE ) ) {
        
        file.copy( tf , catalog[ i , 'output_filename' ] )
        
      } else {
        
        if ( !file.exists( catalog[i , "output_filename" ] ) ) {
          
          archive_extract( normalizePath( tf ) , dir = file.path( td , "unzipped" ) )
          
          this_data_file <- list.files( file.path( tempdir() , "unzipped" ) , full.names = TRUE )
          
          this_data_file <- grep( "\\.csv|\\.txt$", this_data_file, value = TRUE, ignore.case = TRUE )
          
          # faster read file
          x <- tryCatch( suppressWarnings( fread( this_data_file , sep = ";" , encoding = "Latin-1" , dec = "," , na.strings = "-1" , showProgress = FALSE , data.table = FALSE , stringsAsFactors = FALSE , fill = TRUE , strip.white = TRUE , colClasses = "character" ) ) , 
                         error = function(e) read.csv( this_data_file , sep = ";" , dec = "," , header = TRUE , fileEncoding = "latin1" , as.is = TRUE , colClasses = "character" ) )
          
          # coerce to data.table
          x <- as.data.table( x )
          
          # remove files and delete temporary unzips folder
          file.remove( tf )
          suppressWarnings( unlink( file.path( td , "unzipped" ) , recursive = TRUE ) )
          
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
          
          # # fix NAs
          # x[ , ] <- apply( x[ , ] , 2 , function( this_vec ) { this_vec [ this_vec == -1 ] <- NA ; return( this_vec ) } )
          
          # figure out which columns really ought to be numeric
          # figure out which columns really ought to be numeric
          if (catalog[i,"type"] %in% c( "caged" , "caged_ajustes" ) ) {
            this_pattern <- "cnae|cbo|regiao|municipio|tipo|regioes|ibge|uf|faixa|grau|ind_|sexo|bairro|distrito|raca_cor|competencia|admitidos|saldo"
            these_cols <- names( x )[ !grepl( this_pattern , names(x) ) ]
          } else {
            this_pattern <- "^(qtd|vl|tempo|rem)"
            these_cols <- names( x )[ grepl( this_pattern , names(x) ) ]
          }
          for( this_col in these_cols ){
            
            # if the column can be coerced without a warning, coerce it to numeric
            this_result <- tryCatch( as.numeric( gsub( "," , "." , x[ , this_col , with = F ][[1]] ) ), warning = function(c) NULL )
            
            if( !is.null( this_result ) ) x[ , (this_col) := lapply( .SD , function(y) as.numeric( gsub( ",", "." , y ) ) ) , .SDcols = this_col  ]
            
          }
          
          # force uf to numeric
          suppressWarnings( x[ , uf := as.numeric( uf ) ] )
          
          # store case count
          catalog[ i , 'case_count' ] <- nrow( x )
          
          # save file
          dir.create( dirname( catalog[ i , 'output_filename' ] ) , recursive = TRUE , showWarnings = FALSE )
          write.fst( x , path = catalog[ i , 'output_filename' ] , compress = 100 )
          
        } else {
          x <- read_fst( path = catalog[ i , 'output_filename' ] , as.data.table = TRUE )
          catalog[ i , 'case_count' ] <- nrow( x )
        }
        
        these_cols <- sapply( x , class )
        
        these_cols <- data.frame( col_name = names( these_cols ) , col_type = these_cols , stringsAsFactors = FALSE )
        
        if( exists( catalog[ i , 'db_tablename' ] ) ){
          
          same_table_cols <- get( catalog[ i , 'db_tablename' ] )
          same_table_cols <- unique( rbind( these_cols , same_table_cols ) )
          
        } else same_table_cols <- these_cols
        
        dupe_cols <- same_table_cols$col_name[ duplicated( same_table_cols$col_name ) ]
        
        # if there's a duplicate, remove the numeric typed column
        same_table_cols <- same_table_cols[ !( same_table_cols$col_type == 'numeric' & same_table_cols$col_name %in% dupe_cols ) , ]
        
        assign( catalog[ i , 'db_tablename' ] , same_table_cols )
        
        # process tracker
        cat( paste0( "mtps catalog entry " , i , " of " , nrow( catalog ) , " stored at '" , catalog[ i , 'output_filename' ] , "'\n" ) )
        rm( x ) ; gc()
        
        # if this is the final catalog entry for the unique db_tablename, then write them all to the database
        if( i == max( which( catalog$db_tablename == catalog[ i , 'db_tablename' ] ) ) ){
          
          correct_columns <- get( catalog[ i , 'db_tablename' ] )
          
          # open the connection to the monetdb database
          dir.create( catalog[ i , "dbfolder" ] , recursive = TRUE , showWarnings = FALSE ) 
          db <- dbConnect( MonetDBLite() , catalog[ i , 'dbfolder' ] )
          
          # loop through all tables that match the current db_tablename
          for( this_file in catalog[ catalog$db_tablename %in% catalog[ i , 'db_tablename' ] , 'output_filename' ] ){
            
            x <- read_fst( this_file , as.data.table = TRUE )
            
            for( this_col in setdiff( correct_columns$col_name , names( x ) ) ) x[ , (this_col) := NA ]
            
            # get final table types
            same_table_cols <- get( catalog[ i , 'db_tablename' ] )
            
            for( this_row in seq( nrow( same_table_cols ) ) ){
              
              if( same_table_cols[ this_row , 'col_type' ] != sapply( x , class , USE.NAMES = FALSE )[ same_table_cols[ this_row , 'col_name' ] ] ) {
                
                if( same_table_cols[ this_row , 'col_type' ] == 'numeric' ) x[ , ( same_table_cols[ this_row , 'col_name' ] ) := lapply( .SD , as.numeric ) , .SDcols = same_table_cols[ this_row , 'col_name' ] ]
                if( same_table_cols[ this_row , 'col_type' ] == 'character' ) x[ , ( same_table_cols[ this_row , 'col_name' ] ) := lapply( .SD , as.character ) , .SDcols = same_table_cols[ this_row , 'col_name' ] ]
                
              }
              
            }
            
            # put the columns of x in alphabetical order so they're always the same
            setcolorder( x , sort( colnames( x ) ) )
            
            # re-save the file
            write.fst( x , path = this_file , compress = 100 )
            
            # append the file to the database
            dbWriteTable( db , catalog[ i , 'db_tablename' ] , x , append = TRUE , row.names = FALSE )
            
            # process tracker
            file_index <- seq_along( catalog[ ( catalog[ i , 'db_tablename' ] == catalog$db_tablename ) & ! is.na( catalog$db_tablename ) , 'output_filename' ] ) [ this_file == catalog[ ( catalog[ i , 'db_tablename' ] == catalog$db_tablename ) & !is.na( catalog$db_tablename ) , 'output_filename' ] ]
            cat( "\r", paste0( "mtps entry " , file_index , " of " , nrow( catalog[ catalog$db_tablename == catalog[ i , 'db_tablename' ] , ] ) , " stored at '" , catalog[ i , 'db_tablename' ] , "'\r" ) )
            
          }
          
        }
        
      }
      
    }
    
    # disconnect from the current database
    dbDisconnect( db , shutdown = TRUE )
    
    catalog
    
  }

