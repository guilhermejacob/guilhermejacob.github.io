downloader::source_url( "https://raw.githubusercontent.com/guilhermejacob/guilhermejacob.github.io/master/scripts/install_packages.R" , quiet = TRUE , prompt = TRUE )

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

files_in_ftp_dir <- function( main_url ) {
  main_list <- gsub( "//" , "/" , file.path( main_url , unlist( strsplit( RCurl::getURL( main_url , ftp.use.epsv = TRUE , dirlistonly = TRUE ) , "\n" ) ) ) ) 
  main_list <- unlist( main_list )
  main_list <- ifelse( !grepl( "\\." , basename( main_list ) ) , paste0( main_list , "/" ) , main_list )
  gsub( "vínculos" , "v%EDnculos" , main_list )
  # unlist( sapply( main_list, URLencode , USE.NAMES = FALSE , simplify = TRUE ) )
}

remove_special_character <- function(x) {
  stripped_x <- iconv( x , to = "ascii//translit" , sub = "_" )
  stripped_x <- gsub( "('|~|\\^)" , "" , stripped_x , ignore.case = TRUE )
  stripped_x
}


catalog_mtps <-
  function( output_dir , ... ){
    
    output_dir <- ifelse( !grepl( "/$" , output_dir ) , paste0( output_dir , "/" ) , output_dir )
    
    url_path <- "ftp://ftp.mtps.gov.br/pdet/microdados/"
    # url_path <- "ftp:/ftp.mtps.gov.br/pdet/microdados/RAIS/"
    
    file_list <- files_in_ftp_dir( url_path )
    file_list <- unlist( sapply( unique( file_list ) , function( this_url ) {
      if ( grepl( "\\." , basename( this_url ) ) ) { return( this_url ) } else { this_url <- paste0( this_url , "/" ) }
      gsub( "//" , "/" , files_in_ftp_dir( this_url ) )
    } , USE.NAMES = FALSE , simplify = TRUE ) )
    file_list <- unlist( sapply( unique( file_list ) , function( this_url ) {
      if ( grepl( "\\." , basename( this_url ) ) ) { return( this_url ) } else { this_url <- paste0( this_url , "/" ) }
      gsub( "//" , "/" , files_in_ftp_dir( this_url ) )
    } , USE.NAMES = FALSE , simplify = TRUE ) )
    file_list <- unlist( sapply( unique( file_list ) , function( this_url ) {
      if ( grepl( "\\." , basename( this_url ) ) ) { return( this_url ) } else { this_url <- paste0( this_url , "/" ) }
      gsub( "//" , "/" , files_in_ftp_dir( this_url ) )
    } , USE.NAMES = FALSE , simplify = TRUE ) )
    
    # keep files only
    mtps_files <- file_list[ grepl( "\\." , basename( file_list ) ) ]
    
    # drop "caged_ajustes"
    mtps_files <- mtps_files[ !grepl( "ajustes" , dirname( file_list ) , ignore.case = TRUE ) ]
    
    catalog <-
      data.frame(
        full_url = mtps_files ,
        stringsAsFactors = FALSE
      )
    
    catalog$type <-
      ifelse( grepl( "layout|xls$|pdf$" , catalog$full_url , ignore.case = TRUE ) , "docs" ,
              ifelse( grepl( "RAIS" , catalog$full_url ) , "rais" ,
                      ifelse( grepl( "CAGED" , catalog$full_url ) , "caged" , NA ) )
      )
    
    catalog$subtype[ catalog$type == "caged" ] <- NA
    catalog$subtype[ catalog$type == "rais" ] <-
      ifelse( grepl( "estb" , catalog$full_url[ catalog$type == "rais" ] , ignore.case = TRUE ) , "estabelecimento" , "vinculo" )
    
    catalog$output_filename <- gsub( "7z$|zip$" , "rds" ,
                                     gsub( gsub( "ftp://" , "ftp:/" , url_path ) , output_dir , tolower( catalog$full_url ) , ignore.case = TRUE ) ,
                                     ignore.case = TRUE )
    catalog$output_filename <- sapply( catalog$output_filename , utils::URLdecode )
    
    catalog$year <- NULL
    catalog$year [ catalog$type %in% c( "caged" , "rais" ) ] <- as.numeric( gsub( ".*/" , "" , dirname( catalog$full_url ) )[ catalog$type %in% c( "caged" , "rais" ) ] )
    
    catalog$month <- NULL
    catalog$month [ catalog$type %in% c( "caged" ) ] <- substr( gsub( ".*_|\\..*", "", basename( catalog$full_url ) ) , 1 , 2 ) [ catalog$type %in% c( "caged" ) ]
    catalog$month <- as.numeric( catalog$month )
    
    catalog$db_tablename <-
      ifelse( catalog$type %in% c("caged" , "rais") ,
              paste0(
                ifelse( catalog$type == "caged" , "caged" , "rais_" ) ,
                ifelse( !is.na( catalog$subtype ) , paste0( catalog$subtype, "_" ) , "" ) ,
                ifelse( catalog$type == "rais" , catalog$year , "" ) ) , NA )
    
    # catalog$dbfolder <- ifelse( is.na( catalog$db_tablename ) , NA , paste0( output_dir , "/MonetDB" ) )
    catalog$dbfile <- ifelse( is.na( catalog$db_tablename ) , NA , paste0( output_dir , "/mtps.sqlite" ) )
    catalog$dbfile <- gsub( "//" , "/" , catalog$dbfile )
    
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
  
  # create directories
  lapply( unique( dirname( catalog$datavault_file ) ), function( this_dir ){ if ( !dir.exists( this_dir ) ) dir.create( this_dir , recursive = TRUE ) } )
  
  # if there isn't any non-downloaded, run download procedure:
  if ( any( !existing_files ) ) {
    
    # print message
    cat( sum( 1*!existing_files ) , "missing files detected.\nDownloading missing files only.\n")
    
    # download files to datavault directory
    for ( i in seq_len( nrow(catalog) )[ !existing_files ] ) {
      
      # skip existing file
      if( skipExist & existing_files[ i ] ) next()
      
      # download file
      download.file( catalog$full_url[ i ] , catalog$datavault_file[ i ] , quiet = TRUE )
      
      # process tracker
      cat( "file" , i , "out of" , nrow( catalog ) , "downloaded to" , datavault_dir , "\r")
      
    }
    
  }
  
  # print message
  cat( "\nmtps datavault was built at" , datavault_dir , "\n" )
  
  # return catalog
  catalog
  
}

build_mtps <-
  function( catalog , ... ){
    
    tf <- tempfile()
    
    # create directory structure
    for ( i in seq_len( nrow( catalog ) ) )  if ( !dir.exists( dirname( catalog[ i , 'output_filename' ] ) ) ) { dir.create( dirname( catalog[ i , 'output_filename' ] ) , recursive = TRUE ) }
    for ( i in seq_len( nrow( catalog ) ) )  if ( !dir.exists( dirname( catalog[ i , "dbfile" ] ) ) ) { dir.create( dirname( catalog[ i , "dbfile" ] ) , recursive = TRUE ) }
    
    for ( i in seq_len( nrow( catalog ) ) ){
      
      # download the file
      if ( is.null( catalog[ i , "datavault_file" ] ) ) {
        download.file( catalog[ i , "full_url" ] , tf , quiet = TRUE , mode = "wb" )
      } else if ( is.na( catalog[ i , "datavault_file" ] ) ) {
        download.file( catalog[ i , "full_url" ] , tf , quiet = TRUE , mode = "wb" ) 
      } else {
        file.copy( catalog[ i , "datavault_file" ] , tf )
      }
      
      if( !grepl( ".7z$|.zip$" , catalog[ i , 'full_url' ] , ignore.case = TRUE ) ){
        
        file.copy( tf , catalog[ i , 'output_filename' ] )
        
      } else {
        
        archive::archive_extract( normalizePath( tf ) , dir = file.path( tempdir() , "unzipped" ) )
        
        this_data_file <- list.files( file.path( tempdir() , "unzipped" ) , full.names = TRUE )
        
        this_data_file <- grep( "\\.csv|\\.txt$", this_data_file, value = TRUE, ignore.case = TRUE )
        
        x <- utils::read.csv( this_data_file , sep = ";" , dec = "," , header = TRUE , fileEncoding = "latin1" , as.is = TRUE )
        
        suppressWarnings( unlink( file.path( tempdir() , "unzipped" ) , recursive = TRUE ) )
        
        # convert all column names to lowercase
        names( x ) <- tolower( names( x ) )
        
        # remove special characters
        names( x ) <- remove_special_character( names( x ) )
        
        # remove trailing spaces
        names( x ) <- trimws( names( x ) , which = "both" )
        
        # change dots for underscore
        names( x ) <- gsub( "\\.$" , "" , names( x ) )
        names( x ) <- gsub( "\\.|\\.\\." , "_" , names( x ) )
        
        # fix NAs
        x[ , ] <- apply( x[ , ] , 2 , function( this_vec ) { this_vec [ this_vec == -1 ] <- NA ; return( this_vec ) } )
        
        # figure out which columns really ought to be numeric
        for( this_col in names( x ) ){
          
          # if the column can be coerced without a warning, coerce it to numeric
          this_result <- tryCatch( as.numeric( x[ , this_col ] ) , warning = function(c) NULL )
          
          if( !is.null( this_result ) ) x[ , this_col ] <- as.numeric( x[ , this_col ] )
          
        }
        
        catalog[ i , 'case_count' ] <- nrow( x )
        
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
        cat( paste0( "mtps catalog entry " , i , " of " , nrow( catalog ) , " stored at '" , catalog[ i , 'output_filename' ] , "'\r\n\n" ) )
        
        # if this is the final catalog entry for the unique db_tablename, then write them all to the database
        if( i == max( which( catalog$db_tablename == catalog[ i , 'db_tablename' ] ) ) ){
          
          correct_columns <- get( catalog[ i , 'db_tablename' ] )
          
          # open the connection to the sqlite database
          db <- DBI::dbConnect( RSQLite::SQLite() , catalog[ i , 'dbfile' ] )
          
          # loop through all tables that match the current db_tablename
          for( this_file in catalog[ catalog$db_tablename %in% catalog[ i , 'db_tablename' ] , 'output_filename' ] ){
            
            x <- readRDS( this_file )
            
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
            cat( "\r", paste0( "mtps entry " , file_index , " of " , nrow( catalog[ catalog$db_tablename == catalog[ i , 'db_tablename' ] , ] ) , " stored at '" , catalog[ i , 'db_tablename' ] , "'\r" ) )
            
          }
          
          # disconnect from the current database
          DBI::dbDisconnect( db )
          
        }
        
        
      }
      
      
      # delete the temporary files
      suppressWarnings( file.remove( tf ) )
      
    }
    
    catalog
    
  }

