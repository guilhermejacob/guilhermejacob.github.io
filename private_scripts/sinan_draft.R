# auxilliary functions
# create local data catalog:
catalog_sinan <- function( output_dir ){
  
  if ( !grepl( "\\/$" , output_dir ) ) output_dir <- paste0( output_dir , "/" )
  
  sus_htmls <- "ftp://ftp.datasus.gov.br/dissemin/publicos/uploads/fernanda/"
  
  # sus_htmls <- datasus_getlisting( sus_htmls )$dirs
  # sus_htmls <- sus_htmls[ !grepl( "parcial|base%20tabnet" , sus_htmls , ignore.case = TRUE ) ]
  
  these_links <- datasus_ftp_scrape( datasus_getlisting( sus_htmls )$dirs )
  these_links <- these_links[ !grepl( "dengn" , these_links , ignore.case = TRUE ) ]
  these_links <- these_links[ !grepl( "BASE%20TABNET%20JAN%202018" , these_links , ignore.case = TRUE ) ]
  these_links_dec <- Hmisc::sedit( these_links , as.character( datasus_codes ) , names( datasus_codes ) )
  
  catalog <-
    data.frame(
      full_url = these_links ,
      stringsAsFactors = FALSE
    )
  
  # catalog$type <- "sinan"
  
  catalog$output_filename <- gsub( sus_htmls , output_dir , tolower( these_links_dec ) , ignore.case = TRUE )
  catalog$output_filename <- gsub( "\\/\\/" , "/" , catalog$output_filename )
  catalog$output_filename <- gsub( "\\.(dbf|dbc)$" , ".Rds" , catalog$output_filename , ignore.case = TRUE )
  
  # year_lines <- gsub( "[^0-9]" , "" , basename( catalog$output_filename ) )
  
  # these_tablenames <- ifelse( paste0( dirname( catalog$output_filename ) , "/" ) == output_dir , NA , dirname( catalog$output_filename ) )
  # these_tablenames <- gsub( paste0( output_dir , "|\\..*" ) , "" , these_tablenames )
  # these_tablenames <- gsub( "\\-|\\/| " , "_" , these_tablenames )
  # these_tablenames <- iconv( these_tablenames , "" , "ascii//translit" , sub = "" )
  # these_tablenames <- gsub( "\\^|'|~" , "" , these_tablenames )
  
  # these_tablenames <- basename( catalog$output_filename )
  # these_tablenames <- iconv( these_tablenames , "" , "ascii//translit" , sub = "" )
  # these_tablenames <- gsub( "\\^|'|~" , "" , these_tablenames )
  # these_tablenames <- gsub( "_|[0-9]{2-4}.*" , "" , these_tablenames )
  # these_tablenames <- ifelse( grepl( "\\.(dbf|dbc)$" , basename( these_links ) , ignore.case = TRUE ) , these_tablenames , NA )
  # these_tablenames <- gsub( "sinan" , "violencia" , these_tablenames )
  
  these_tablenames <- basename( dirname( these_links_dec ) )
  these_tablenames <- tolower( these_tablenames )
  these_tablenames <- iconv( these_tablenames , "" , "ascii//translit" , sub = "" )
  these_tablenames <- gsub( "\\^|'|~" , "" , these_tablenames )
  these_tablenames <- gsub( "\\-| " , "_" , these_tablenames )
  these_tablenames <- gsub( "acidentes_por_animais_peconhentos" , "animpn" , these_tablenames )
  these_tablenames <- ifelse( grepl( "\\.(dbf|dbc)$" , basename( these_links ) , ignore.case = TRUE ) , these_tablenames , NA )
  
  catalog$db_tablename <- these_tablenames
    
  catalog$dbfolder <- ifelse( is.na( catalog$db_tablename ) , NA , paste0( output_dir , "MonetDB" ) )
  
  catalog
  
}

# datavault
datavault_sinan <- function( catalog , datavault_dir , skipExist = TRUE ) {
  
  if ( !grepl( "\\/$" , datavault_dir ) ) datavault_dir <- paste0( datavault_dir , "/" )
  
  catalog[ , "datavault_file" ] <- gsub( "ftp://ftp.datasus.gov.br/dissemin/publicos/uploads/fernanda" , datavault_dir , catalog$full_url )
  catalog[ , "datavault_file" ] <- gsub( "//" , "/" , catalog[ , "datavault_file" ] , ignore.case = TRUE )
  
  # check for existing files
  existing_files <- file.exists( catalog[ , "datavault_file" ] )
  
  for (i in seq_along( catalog$full_url ) ) {
    
    # skip existing file
    if( skipExist & existing_files[ i ] ){ cat( "file" , i , "out of" , nrow( catalog ) , "downloaded to" , datavault_dir , "\r") ; next() }
    
    if ( !dir.exists( dirname( catalog[ i , "datavault_file" ] ) ) ) { dir.create( dirname( catalog[ i , "datavault_file" ] ) , recursive = TRUE ) }
    
    # download file
    download.file( catalog[ i , "full_url" ], catalog[ i , "datavault_file" ] , mode = "wb" , quiet = FALSE )
    
    # process tracker
    cat( "file" , i , "out of" , nrow( catalog ) , "downloaded to" , datavault_dir , "\n")
    
  }
  
  # print message
  cat( "\ndatasus datavault was built at" , datavault_dir , "\n" )
  
  # return catalog
  catalog
  
}

# build dataset from catalog:
build_sinan <- function( catalog ) {
  
  # create all folders:
  for ( this_dir in unique( dirname( catalog$output_filename ) ) ) { dir.create( this_dir , recursive = TRUE )  }
  
  if ( !requireNamespace( "read.dbc" , quietly = TRUE ) ) stop( "read.dbc needed for this function to work. to install it, type `install.packages( 'read.dbc' )`" , call. = FALSE )
  
  tf <- tempfile()
  
  for ( i in seq_len( nrow( catalog ) ) ){
    
    # download the file
    if ( is.null( catalog[ i , "datavault_file" ] ) ) {
      download.file( catalog[ i , "full_url" ] , tf , quiet = TRUE , mode = "wb" )
    } else if ( is.na( catalog[ i , "datavault_file" ] ) ) {
      download.file( catalog[ i , "full_url" ] , tf , quiet = TRUE , mode = "wb" ) 
    } else {
      file.copy( catalog[ i , "datavault_file" ] , tf )
    }
    
    if ( !grepl( "dbf$" , catalog[ i , 'full_url' ] , ignore.case = TRUE ) ) {
      
      if ( !dir.exists( dirname( catalog[ i , 'output_filename' ] ) ) ) dir.create( dirname( catalog[ i , 'output_filename' ] ) , showWarnings = FALSE , recursive = TRUE )
      if ( !file.exists( catalog[ i , 'output_filename' ] ) ) file.create( catalog[ i , 'output_filename' ] , showWarnings = FALSE )
      file.copy( tf , catalog[ i , 'output_filename' ] , overwrite = TRUE )
      
    } else {
      # read dbc file
      # x <- foreign::read.dbf( tf , as.is = TRUE )
      x <- foreign::read.dbf( tf , as.is = TRUE )
      
      # convert all column names to lowercase
      names( x ) <- tolower( names( x ) )
      
      # add underscores after monetdb illegal names
      for ( j in names( x )[ toupper( names( x ) ) %in% getFromNamespace( "reserved_monetdb_keywords" , "MonetDBLite" ) ] ) names( x )[ names( x ) == j ] <- paste0( j , "_" )
      
      # remove trailing spaces
      names( x ) <- trimws( names( x ) , which = "both" )
      
      # coerce factor columns to character
      x[ sapply( x , class ) == "factor" ] <- sapply( x[ sapply( x , class ) == "factor" ] , as.character )
      
      # figure out which columns really ought to be numeric
      for( this_col in names( x ) ){
        
        # if the column can be coerced without a warning, coerce it to numeric
        this_result <- tryCatch( as.numeric( x[ , this_col ] ) , warning = function(c) NULL , error = function(c) NULL )
        
        if( !is.null( this_result ) ) x[ , this_col ] <- as.numeric( x[ , this_col ] )
        
      }
      
      # force dates to numeric:
      # x[ , grep( "^dt|^data" , colnames(x) ) ] <- apply( x[ , grep( "^dt|^data" , colnames(x) ) ] , 2 , as.numeric )
      
      catalog[ i , 'case_count' ] <- nrow( x )
      
      if ( !dir.exists( dirname( catalog[ i , 'output_filename' ] ) ) ) dir.create( dirname( catalog[ i , 'output_filename' ] ) , recursive = TRUE )
      saveRDS( x , file = catalog[ i , 'output_filename' ] )
      
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
      cat( paste0( "datasus catalog entry " , i , " of " , nrow( catalog ) , " stored at '" , catalog[ i , 'output_filename' ] , "'\r" ) )
      
      
      # if this is the final catalog entry for the unique db_tablename, then write them all to the database
      if ( i == max( which( catalog$db_tablename == catalog[ i , 'db_tablename' ] ) ) ) {
        
        correct_columns <- get( catalog[ i , 'db_tablename' ] )
        
        # open the connection to the monetdblite database
        db <- DBI::dbConnect( MonetDBLite::MonetDBLite() , catalog[ i , 'dbfolder' ] )
        
        # loop through all tables that match the current db_tablename
        for( this_file in catalog[ catalog$db_tablename %in% catalog[ i , 'db_tablename' ] , 'output_filename' ] ){
          
          x <- readRDS( file = this_file )
          
          for( this_col in setdiff( correct_columns$col_name , names( x ) ) ) x[ , this_col ] <- NA
          
          # get final table types
          same_table_cols <- get( catalog[ i , 'db_tablename' ] )
          
          for( this_row in seq( nrow( same_table_cols ) ) ){
            
            if( same_table_cols[ this_row , 'col_type' ] != class( x[ , same_table_cols[ this_row , 'col_name' ] ] ) ){
              
              if( same_table_cols[ this_row , 'col_type' ] == 'numeric' ) x[ , same_table_cols[ this_row , 'col_name' ] ] <- as.numeric( x[ , same_table_cols[ this_row , 'col_name' ] ] )
              if( same_table_cols[ this_row , 'col_type' ] == 'character' ) x[ , same_table_cols[ this_row , 'col_name' ] ] <- as.character( x[ , same_table_cols[ this_row , 'col_name' ] ] )
              
            }
            
          }
          
          # put the columns of x in alphabetical order so they're always the same
          x <- x[ sort( names( x ) ) ]
          
          # re-save the file
          saveRDS( x , file = this_file )
          
          # append the file to the database
          DBI::dbWriteTable( db , catalog[ i , 'db_tablename' ] , x , append = TRUE , row.names = FALSE )
          
          file_index <- seq_along( catalog[ ( catalog[ i , 'db_tablename' ] == catalog$db_tablename ) & ! is.na( catalog$db_tablename ) , 'output_filename' ] ) [ this_file == catalog[ ( catalog[ i , 'db_tablename' ] == catalog$db_tablename ) & ! is.na( catalog$db_tablename ) , 'output_filename' ] ]
          cat( paste0( "sinan entry " , file_index , " of " , nrow( catalog[ catalog$db_tablename == catalog[ i , 'db_tablename' ] , ] ) , " stored at '" , catalog[ i , 'db_tablename' ] , "'\r" ) )
          
        }
        
        # disconnect from the current monet database
        DBI::dbDisconnect( db , shutdown = TRUE )
        
      }
      
      
    }
    
    
    # delete the temporary files
    suppressWarnings( file.remove( tf ) )
    
  }
  
  catalog
  
}

# recursive ftp scrape
datasus_getlisting <- function( these_urls ) {
  these_urls <- ifelse( grepl( "\\/$" , these_urls ) , these_urls , paste0( these_urls , "/" ) )
  res <- lapply( these_urls , function( x ) { if ( !is.null( x ) ) paste0( x , RCurl::curlPercentEncode( unlist( strsplit( RCurl::getURL( x, dirlistonly = TRUE ) , "\n" ) ) , codes = datasus_codes , post.amp = FALSE ) ) else NULL } )
  res <- unlist( res )
  is.file <- unlist( lapply( res , function(x) grepl( "\\." , basename( x ) ) ) )
  res[ !is.file ] <- ifelse( grepl( "\\/$" , res[ !is.file ] ) , res[ !is.file ] , paste0( res[ !is.file ] , "/" ) )
  list( dirs = if ( any(!is.file) ) { res[ !is.file ] } else { NULL } , 
        files = if ( any(is.file) ) { res[ is.file ] } else { NULL } )
}


# datasus_ftp_scrape <- function( main_url ) {
#   
#   final_files <- NULL
#   
#   directories <- datasus_getlisting(main_url)[[1]]
#   
#   while ( length(directories) > 0) {
#     listing <- datasus_getlisting(directories)
#     directories <- listing[[1]]
#     files <- listing[[2]]
#     final_files <- c(final_files, files)
#   }
#   
#   final_files
#   
# }


datasus_ftp_scrape <- function( main_url ) {
  
  final_files <- NULL
  
  directories <- main_url
  # directories <- directories[ 1:5 ]
  
  while ( length(directories) > 0) {
    listing <- datasus_getlisting(directories)
    directories <- listing[[1]]
    files <- listing[[2]]
    final_files <- c(final_files, files )
  }
  
  final_files
  
}

datasus_codes <- c( RCurl:::PercentCodes , "à" =	"%E0" ,	
                    "á" =	"%E1" , "â" =	"%E2" , "ã" =	"%E3" , "å" =	"%E5" , "ã" = "%E3" , "é"	= "%E9" , "ê" =	"%EA" , "ç" =	"%E7" ,
                    "é" =	"%E9" , "ê" =	"%EA" , "í"	= "%ED" , "ó" =	"%F3" , "ô" =	"%F4" , "õ" =	"%F5" , "ú" =	"%FA"	, "û" =	"%FB"	,
                    "ü" =	"%FC" )

