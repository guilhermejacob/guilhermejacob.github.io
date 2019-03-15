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


catalog_obmigra <-
  function( output_dir , ... ){
    
    output_dir <- ifelse( !grepl( "/$" , output_dir ) , paste0( output_dir , "/" ) , output_dir )
    
    url_path <- "ftp://ftp.mtps.gov.br/obmigra/dados/microdados/"
    
    file_list <- files_in_ftp_dir( url_path )
    file_list <- unlist( sapply( unique( file_list ) , function( this_url ) {
      if ( grepl( "\\." , basename( this_url ) ) ) { return( this_url ) } else { this_url <- paste0( this_url , "/" ) }
      gsub( "//" , "/" , files_in_ftp_dir( this_url ) )
    } , USE.NAMES = FALSE , simplify = TRUE ) )
    file_list <- unlist( sapply( unique( file_list ) , function( this_url ) {
      if ( grepl( "\\." , basename( this_url ) ) ) { return( this_url ) } else { this_url <- paste0( this_url , "/" ) }
      gsub( "//" , "/" , files_in_ftp_dir( this_url ) )
    } , USE.NAMES = FALSE , simplify = TRUE ) )
    
    # fix beggining
    file_list <- gsub( "^ftp:/" , "ftp://" , file_list )
    
    # keep files only
    obmigra_files <- file_list[ grepl( "\\." , basename( file_list ) ) ]
    
    
    catalog <-
      data.frame(
        full_url = obmigra_files ,
        stringsAsFactors = FALSE
      )
    
    catalog$type <-
      ifelse( grepl( "layout|xls$|pdf$" , catalog$full_url , ignore.case = TRUE ) , "docs" ,
              ifelse( grepl( "rais" , catalog$full_url ) , "rais" ,
                      ifelse( grepl( "cnig" , catalog$full_url ) , "cnig" , 
                              ifelse( grepl( "sincre" , catalog$full_url ) , "sincre" , NA ) ) )
      )
    
    catalog$subtype[ catalog$type == "cnig" ] <- 
      gsub( ".*(CNIg\\.|CNIg\\_)|(\\.|_).*" , "" , basename( catalog$full_url[ catalog$type == "cnig" ] ) )
    
    # fix names
    catalog$subtype <- ifelse( grepl( "^ind" , catalog$subtype ) , "indeferido" ,
                               ifelse( grepl( "^pror" , catalog$subtype ) , "prorrogado" ,
                                       ifelse( grepl( "^def" , catalog$subtype ) , "deferido" , 
                                               ifelse( grepl( "^canc" , catalog$subtype ) , "cancelado" , NA ) ) ) )
    
    catalog$year <- NULL
    catalog$year <- ifelse( catalog$type == "rais" , gsub( ".*\\-|\\..*" , "" , basename( catalog$full_url ) ) ,
                            ifelse( catalog$type == "sincre" , gsub( "SINCRE\\.|\\..*" , "" , basename( catalog$full_url ) ) ,
                                    ifelse( catalog$type == "cnig" , gsub( ".*\\/" , "" , dirname( catalog$full_url ) ) , NA ) ) )
    
    catalog$output_filename <- NULL
    catalog$output_filename <- gsub( "\\.csv$" , ".rds" , gsub( url_path , output_dir , tolower( catalog$full_url ) , ignore.case = TRUE ) , ignore.case = TRUE )
    catalog$output_filename[ !( catalog$type == "docs" ) ] <-
      file.path( dirname( catalog$output_filename[ !( catalog$type == "docs" ) ] ) , 
                 paste0( catalog$type , ifelse( !is.na( catalog$subtype ) , paste0( "_" , catalog$subtype ) , "" ), "_" , catalog$year , ".rds" )[ !( catalog$type == "docs" ) ] )
    
    # catalog$db_tablename[ catalog$type %in% c( "cnig" , "rais" , "sincre" ) ] <-
    #   paste0( catalog$type[ catalog$type %in% c( "cnig" , "rais" , "sincre" ) ] , 
    #           ifelse( !is.na( catalog$subtype[ catalog$type %in% c( "cnig" , "rais" , "sincre" ) ] ) , 
    #                   paste0( "_" , catalog$subtype[ catalog$type %in% c( "cnig" , "rais" , "sincre" ) ] ) , "" ) )
    
    # stack by data type
    catalog$db_tablename[ catalog$type %in% c( "cnig" , "rais" , "sincre" ) ] <- catalog$type[ catalog$type %in% c( "cnig" , "rais" , "sincre" ) ]
    
    # divide rais in before and after 2015
    catalog$db_tablename[ catalog$type %in% "rais" ] <- 
      ifelse( catalog$year[ catalog$type %in% "rais" ] >= 2015 , "rais0" , "rais1" )
    
    catalog$dbfile <- ifelse( is.na( catalog$db_tablename ) , NA , paste0( output_dir , "/obmigra.sqlite" ) )
    catalog$dbfile <- gsub( "//" , "/" , catalog$dbfile )
    
    catalog$dateformat <- 
      ifelse( catalog$type == "cnig" , "%d/%m/%Y" ,
              ifelse( catalog$type == "rais" , ifelse( catalog$year == 2000 , "%Y%m%d" , "%d%m%Y" ) ,
                      ifelse( catalog$type == "sincre" , ifelse( catalog$year == 2000 , "%d/%m/%Y" , "%Y-%m-%d" ) , NA ) ) )
    
    # drop RAIS
    catalog <- catalog[ !grepl( "rais" , catalog$full_url , ignore.case = TRUE ) , ]
    
    catalog
    
  }

# build datavault
datavault_obmigra <- function( catalog , datavault_dir , skipExist = TRUE ) {
  
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
      download.file( catalog$full_url[ i ] , catalog$datavault_file[ i ] , quiet = FALSE , mode = "wb" )
      
      # process tracker
      cat( "file" , i , "out of" , nrow( catalog ) , "downloaded to" , datavault_dir , "\r")
      
    }
    
  }
  
  # print message
  cat( "\nobmigra datavault was built at" , datavault_dir , "\n" )
  
  # return catalog
  catalog
  
}

build_obmigra <-
  function( catalog , ... ){
    
    tf <- tempfile()
    
    # create directory structure
    for ( i in seq_len( nrow( catalog ) ) )  if ( !dir.exists( dirname( catalog[ i , 'output_filename' ] ) ) ) { dir.create( dirname( catalog[ i , 'output_filename' ] ) , recursive = TRUE ) }
    for ( i in seq_len( nrow( catalog ) ) )  if ( !dir.exists( dirname( catalog[ i , "dbfile" ] ) ) ) { dir.create( dirname( catalog[ i , "dbfile" ] ) , recursive = TRUE ) }
    
    for ( i in seq_len( nrow( catalog ) ) ) {
      
      # download the file
      if ( is.null( catalog[ i , "datavault_file" ] ) ) {
        download.file( catalog[ i , "full_url" ] , tf , quiet = TRUE , mode = "wb" )
      } else if ( is.na( catalog[ i , "datavault_file" ] ) ) {
        download.file( catalog[ i , "full_url" ] , tf , quiet = TRUE , mode = "wb" ) 
      } else {
        file.copy( catalog[ i , "datavault_file" ] , tf )
      }
      
      if ( catalog[ i , 'type' ] == "docs" ) {
        
        file.copy( tf , catalog[ i , 'output_filename' ] )
        
        # process tracker
        cat( paste0( "obmigra catalog entry " , i , " of " , nrow( catalog ) , " stored at '" , catalog[ i , 'output_filename' ] , "'\r\n\n" ) )
        
      } else {
        
        # read data
        if ( catalog[ i , "type" ] == "rais" & catalog[ i , "year" ] == 2015 ) {
          x <- suppressWarnings( readxl::read_xlsx( tf , sheet = 1 ) )
          x <- as.data.frame( x )
          x$ano <- catalog[ i , "year" ]
        } else {
          x <- utils::read.csv( tf , sep = ";" , dec = "," , header = TRUE , fileEncoding = "latin1" , as.is = TRUE )
        }
        
        # convert all column names to lowercase
        names( x ) <- tolower( names( x ) )
        
        # remove special characters
        names( x ) <- remove_special_character( names( x ) )
        
        # remove trailing spaces
        names( x ) <- trimws( names( x ) , which = "both" )
        
        # change dots for underscore
        names( x ) <- gsub( "\\.$" , "" , names( x ) )
        names( x ) <- gsub( "\\.|\\.\\." , "_" , names( x ) )
        
        # change space for underscore
        names( x ) <- gsub( " " , "_" , names( x ) )
        
        if ( catalog[ i , "type" ] == "sincre" & catalog[ i , "year" ] == 2000 ) x$ano_reg <- 2000
        
        # change dates
        for( this_col in grep( "^data_" , names( x ) , value = TRUE ) ) {
          if ( catalog[ i , "type" ] %in% "rais" ) x[ , this_col ] <- stringr::str_pad( x[ , this_col ] , width = "8" , pad = "0" , side = "left" )
          this_result <- as.character( as.Date( x[ , this_col ] , format = catalog[ i , "dateformat" ] ) )
          # x[ , paste0( "rec_" , this_col ) ] <- this_result
          x[ , this_col ] <- this_result
        }
        
        # figure out which columns really ought to be numeric
        for( this_col in names( x ) ){
          
          # if the column can be coerced without a warning, coerce it to numeric
          this_result <- tryCatch( as.numeric( x[ , this_col ] ) , warning = function(c) NULL , error = function(c) NULL )
          
          if( !is.null( this_result ) ) x[ , this_col ] <- as.numeric( x[ , this_col ] )
          
        }
        
        catalog[ i , 'case_count' ] <- nrow( x )
        
        saveRDS( x , file = catalog[ i , 'output_filename' ] )
        
        these_cols <- sapply( x , class )
        
        these_cols <- data.frame( col_name = names( these_cols ) , col_type = these_cols , stringsAsFactors = FALSE )
        
        if ( exists( catalog[ i , 'db_tablename' ] ) ) {
          
          same_table_cols <- get( catalog[ i , 'db_tablename' ] )
          same_table_cols <- unique( rbind( these_cols , same_table_cols ) )
          
        } else same_table_cols <- these_cols
        
        dupe_cols <- same_table_cols$col_name[ duplicated( same_table_cols$col_name ) ]
        
        # if there's a duplicate, remove the numeric typed column
        same_table_cols <- same_table_cols[ !( same_table_cols$col_type == 'numeric' & same_table_cols$col_name %in% dupe_cols ) , ]
        
        assign( catalog[ i , 'db_tablename' ] , same_table_cols )
        
        # process tracker
        cat( paste0( "obmigra catalog entry " , i , " of " , nrow( catalog ) , " stored at '" , catalog[ i , 'output_filename' ] , "'\r\n\n" ) )
        
        # if this is the final catalog entry for the unique db_tablename, then write them all to the database
        if( i == max( which( catalog$db_tablename == catalog[ i , 'db_tablename' ] ) ) ) {
          
          correct_columns <- get( catalog[ i , 'db_tablename' ] )
          
          # open the connection to the sqlite database
          db <- DBI::dbConnect( RSQLite::SQLite() , catalog[ i , 'dbfile' ] )
          
          # loop through all tables that match the current db_tablename
          for( this_file in catalog[ catalog$db_tablename %in% catalog[ i , 'db_tablename' ] , 'output_filename' ] ) {
            
            x <- readRDS( this_file )
            
            for( this_col in setdiff( correct_columns$col_name , names( x ) ) ) x[ , this_col ] <- NA
            
            # get final table types
            same_table_cols <- get( catalog[ i , 'db_tablename' ] )
            
            for( this_row in seq( nrow( same_table_cols ) ) ){
              
              if( same_table_cols[ this_row , 'col_type' ] != class( x[ , same_table_cols[ this_row , 'col_name' ] ] ) ) {
                
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
            cat( "\r", paste0( "obmigra entry " , file_index , " of " , nrow( catalog[ !is.na( catalog$db_tablename ) & catalog$db_tablename == catalog[ i , 'db_tablename' ] , ] ) , " stored at '" , catalog[ i , 'db_tablename' ] , "'\r" ) )
            
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

