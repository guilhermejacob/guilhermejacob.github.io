downloader::source_url( "https://raw.githubusercontent.com/guilhermejacob/guilhermejacob.github.io/master/scripts/install_packages.R" , quiet = TRUE , prompt = FALSE )

# catalog function
catalog_censo_superior <- function( output_dir ) {
  
  inep_portal <- "http://portal.inep.gov.br/web/guest/microdados"
  
  inep_html <- xml2::read_html(inep_portal)
  w <- rvest::html_attr( rvest::html_nodes( inep_html , "a" ) , "href" )
  
  these_links <- w[ grepl( "superior(.*)\\.zip" , w) ]
  
  these_years <- as.numeric( gsub( ".*superior|.*superior_|\\..*" , "" , basename( these_links ) ) )
  
  catalog <- data.frame(
    year =  these_years ,
    full_url = these_links ,
    output_folder = file.path( output_dir , these_years ) ,
    dbfolder = file.path( output_dir , "MonetDB" ) ,
    stringsAsFactors = FALSE )
  
  catalog <- catalog[ order(catalog$year) , ]
  
  # subset for 2009 onwards
  catalog <- catalog[ catalog$year >= 2009 , ]
  
  catalog
  
}

# datavault
datavault_censo_superior <- function( catalog , datavault_dir , skipExist = TRUE ) {
  
  # get common directory
  url_path <- get_common_dir( tolower( catalog$full_url ) )
  
  # create datavault links
  catalog[ , "datavault_file" ] <- gsub( url_path , paste0( datavault_dir , "/" ), tolower( catalog$full_url ) , ignore.case = TRUE )
  catalog[ , "datavault_file" ] <- gsub( "//" , "/" , catalog[ , "datavault_file" ] , ignore.case = TRUE )
  
  # check for existing files
  existing_files <- file.exists( catalog[ , "datavault_file" ] )
  
  # create directories
  lapply( unique( dirname( catalog[ , "datavault_file" ] ) ), function( this_dir ){ if ( !dir.exists( this_dir ) ) dir.create( this_dir , recursive = TRUE ) } )
  
  # if there isn't any non-downloaded, run download procedure:
  if ( any( !existing_files ) ) {
    
    # print message
    cat( sum( 1*!existing_files ) , "missing files detected.\nDownloading missing files only.\n")
    
    # download files to datavault directory
    for ( i in seq_len( nrow(catalog) )[ !existing_files ] ) {
      
      # skip existing file
      if( skipExist & existing_files[ i ] ) next()
      
      # download file
      download.file( catalog[ i , "full_url" ] , catalog[ i , "datavault_file" ] , quiet = FALSE )
      
      # process tracker
      cat( "file" , i , "out of" , nrow( catalog ) , "downloaded to" , datavault_dir , "\r")
      
    }
    
  }
  
  # print message
  cat( "\ncenso_superior datavault was built at" , datavault_dir , "\n" )
  
  # return catalog
  catalog
  
}

# build censo superior
build_censo_superior <- function( catalog ) {
  
  tf <- tempfile()
  td <- file.path( tempdir() , "unzip" ) 
  
  for ( i in seq_len( nrow( catalog ) ) ) {
    
    # create folders
    if ( !dir.exists( dirname( catalog[ i , "dbfolder" ] ) ) ) dir.create( dirname( catalog[ i , "dbfolder" ] ) , recursive = TRUE )
    # if ( !dir.exists( catalog[ i , "output_folder" ] ) ) dir.create( catalog[ i , "output_folder" ] , recursive = TRUE )
    
    # opens connection to database
    db <- DBI::dbConnect( MonetDBLite::MonetDBLite() , catalog[ i , "dbfolder" ] )
    
    # download the file
    if ( is.null( catalog[ i , "datavault_file" ] ) ) {
      download.file( catalog[ i , "full_url" ] , tf , quiet = TRUE , mode = "wb" )
    } else if ( is.na( catalog[ i , "datavault_file" ] ) ) {
      download.file( catalog[ i , "full_url" ] , tf , quiet = TRUE , mode = "wb" ) 
    } else {
      file.copy( catalog[ i , "datavault_file" ] , tf )
    }
    
    # extract files
    unzip( tf , exdir = td )
    
    # remove temporary file
    file.remove( tf )
    
    # get filenames
    all_files_wrong<- list.files( td , full.names = FALSE , recursive = TRUE )
    
    # fix file names
    all_files_fixed <- sapply( all_files_wrong, function( this_file ) iconv( URLdecode( this_file ) , from = "IBM850" ) , USE.NAMES = FALSE )
    
    # create correct folders
    for ( this_folder in unique( dirname( file.path( catalog[ i , "output_folder" ] , all_files_fixed ) ) ) ) {
      dir.create( this_folder , recursive = TRUE , showWarnings = FALSE )
    }
    
    # copy correct structure
    for ( j in seq_along( all_files_wrong ) ) {
      file.copy( file.path( td , all_files_wrong[ j ] ) , file.path( catalog[ i , "output_folder" ] , all_files_fixed[ j ] ) , overwrite = TRUE )
    }
    
    # remove temporary folder
    # file.remove( file.path( td ) )
    unlink( td , recursive = TRUE , force = TRUE )
    
    # apply year-specific procedures
    if ( catalog[ i , "year" ] >= 2009 ) {
      
      # get data files
      datafiles <- grep( "\\.(rar|zip|csv)$" , list.files( catalog[ i , "output_folder" ] , full.names = TRUE , recursive = TRUE ) , ignore.case = TRUE , value = TRUE )
      
      # extract if any
      if ( any( grepl( "\\.rar$" , datafiles , ignore.case = TRUE ) ) ) { 
        for ( this_rar_file in datafiles [ grepl( "\\.rar$" , datafiles , ignore.case = TRUE ) ] ) {
          archive::archive_extract( this_rar_file , dir = dirname( this_rar_file ) )
          file.remove( this_rar_file )
        }
      }
      if ( any( grepl( "\\.zip$" , datafiles , ignore.case = TRUE ) ) ) { 
        for ( this_zip_file in datafiles [ grepl( "\\.rar$" , datafiles , ignore.case = TRUE ) ] ) {
          unzip( this_zip_file , exdir = dirname( this_zip_file ) )
          file.remove( this_zip_file )
        }
      }
      
      # get data files, again
      datafiles <- grep( "\\.csv$" , list.files( catalog[ i , "output_folder" ] , full.names = TRUE , recursive = TRUE ) , ignore.case = TRUE , value = TRUE )
      
      # tablesizes vector
      tablesize <- NULL
      
      for ( this_datafile in datafiles ) {
        
        # define tablename
        this_tablename <- paste0( gsub( "dm_|\\..*" , "" , tolower( basename(this_datafile) ) ) , "_" , catalog[ i , "year" ] )
        
        # read data
        # x <- suppressMessages( readr::read_delim( this_datafile , delim = "|" ) )
        x <- data.table::fread( this_datafile , sep = "|" , data.table = TRUE )
        
        # fix column names
        colnames(x) <- tolower( colnames(x) ) # lowercase
        colnames(x) <- gsub( "~|'|^" , "" , iconv( colnames(x) , to = "ascii//translit" ) ) # strip special characters
        
        # decode fields
        these_cols <- colnames(x)[ sapply( x , class ) %in% "character" ]
        x[ , (these_cols) := lapply( .SD , function( this_vector ) { iconv( this_vector , from = "windows-1252" ) } ) , .SDcols = these_cols ]
        
        # write data to database
        DBI::dbWriteTable( db , this_tablename , x )
        
        # drop data and datafile
        rm( x ) ; file.remove( this_datafile ) ; gc()
        
        # process tracker
        cat( basename( this_datafile ) , "stored at" , this_tablename , "\n" )
        
        # collect table sizes
        k <- which( this_datafile %in% datafiles )
        tablesize[ k ] <- DBI::dbGetQuery( db , paste0( "SELECT COUNT(*) FROM ", this_tablename ) )[ 1 , 1 ]
        
      }
      
    } else {
      
      # get data files
      datafiles <- grep( "dados/.*\\.(rar|zip|txt)$" , list.files( catalog[ i , "output_folder" ] , full.names = FALSE , recursive = TRUE ) , ignore.case = TRUE , value = TRUE )
      datafiles <- file.path( catalog[ i , "output_folder" ] , datafiles )
      
      # extract if any
      if ( any( grepl( "\\.rar$" , datafiles , ignore.case = TRUE ) ) ) { 
        for ( this_rar_file in datafiles [ grepl( "\\.rar$" , datafiles , ignore.case = TRUE ) ] ) {
          archive::archive_extract( this_rar_file , dir = dirname( this_rar_file ) )
          file.remove( this_rar_file )
        }
      }
      if ( any( grepl( "\\.zip$" , datafiles , ignore.case = TRUE ) ) ) { 
        for ( this_zip_file in datafiles [ grepl( "\\.rar$" , datafiles , ignore.case = TRUE ) ] ) {
          unzip( this_zip_file , exdir = dirname( this_zip_file ) )
          file.remove( this_zip_file )
        }
      }
      
      # get data files, again
      datafiles <- grep( "dados/.*\\.txt$" , list.files( catalog[ i , "output_folder" ] , full.names = FALSE , recursive = TRUE ) , ignore.case = TRUE , value = TRUE )
      datafiles <- file.path( catalog[ i , "output_folder" ] , datafiles )
      
      # get input files
      inputfiles <- grep( "input.*\\.sas$" , list.files( catalog[ i , "output_folder" ] , full.names = FALSE , recursive = TRUE ) , ignore.case = TRUE , value = TRUE )
      inputfiles <- file.path( catalog[ i , "output_folder" ] , inputfiles )
      inputfiles <- 
        sapply( basename( datafiles ) , 
                function( this_data_file ) { 
                  this_pattern <- tolower( this_data_file )
                  this_pattern <- gsub( "_sup_.*|_[1-9].*|\\..*" , "" , this_pattern , ignore.case = TRUE )
                  this_pattern <- gsub( "presencial" , "pres" , this_pattern , ignore.case = TRUE )
                  this_pattern <- gsub( "distancia" , "dis" , this_pattern , ignore.case = TRUE )
                  this_pattern <- gsub( "formacao_esp_" , "forme_" , this_pattern , ignore.case = TRUE )
                  if ( catalog[ i , "year" ] >= 2006 ) this_pattern <- gsub( "instituicao" , "instituicoes" , this_pattern , ignore.case = TRUE )
                  if ( catalog[ i , "year" ] >= 2006 ) this_pattern <- gsub( "secomple_" , "sequenciais_comple_" , this_pattern , ignore.case = TRUE )
                  inputfiles [ grepl( this_pattern , basename( inputfiles ) , ignore.case = TRUE ) ]
                } , USE.NAMES = FALSE )
      inputfiles <- unlist( inputfiles )
      
      # tablesizes vector
      tablesize <- NULL
      
      tf2 <- tempfile()
      
      for ( j in seq_along(datafiles) ) {
        
        this_datafile <- datafiles[[j]]
        this_inputfile <- inputfiles[[j]]
        this_tablename <- paste0( gsub( "_sup.*|_[1-9].*" , "" , gsub( "\\..*" , "" , tolower( basename( this_datafile ) ) ) ), "_" , catalog[ i , "year" ] )
        
        # read input code
        this_sas <- file( this_inputfile , 'r' , encoding = 'windows-1252' )
        w <- remove_special_character( readLines( this_sas ) )
        close( this_sas )
        
        # revise length of file
        w <- w[ seq_along( w ) >= grep( "input" , w , ignore.case = TRUE )[[1]] ]
        
        # remove all tab characters
        w <- gsub( '\t' , ' ' , w )
        
        # fix inputs
        w <- gsub( '@ ' , '@' , w , fixed = TRUE )
        w <- gsub( "@1 MASCARA  " , "@1 MASCARA $8" , w , fixed = TRUE )
        w <- gsub( "@25 CD_CURSO 10." , "@25 CD_CURSO 8." , w , fixed = TRUE )
        # w <- gsub( "@33 CODMUNIC $12." , "@35 CODMUNIC $10." , w , fixed = TRUE )
        w <- gsub( "@248 DT_INICIO $8." , "@248 DT_INICIO $9." , w , fixed = TRUE )
        w <- gsub( "@248 DT_INICIO DATE8." , "@248 DT_INICIO $9." , w , fixed = TRUE )
        w <- trimws( w )
        # w <- gsub( '\\$' , '' , w )
        # w <- gsub( '\\.' , '' , w )
        w <- gsub( "quit.*" , "" , w , ignore.case = TRUE )
        
        
        # write code to a temporary file
        writeLines( w , tf2 )
        
        # read data, finally
        readLines( this_datafile , n = 5 )
        x <- suppressWarnings( lodown:::read_SAScii( this_datafile , tf2 ) )
        file.remove( tf2 )
        file.remove( this_datafile )
        
        # convert column names to lowercase
        names( x ) <- tolower( names( x ) )
        
        # remove special characters
        names( x ) <- remove_special_character( names( x ) )
        
        # write data to table in database
        DBI::dbWriteTable( db , this_tablename , x )
        
        rm( x )
        
        cat( tolower( gsub( "\\..*" , "" , basename( this_datafile ) ) ) , "stored at" , this_tablename , "\r\n\n" )
        
        catalog[ i , 'case_count' ] <- max( catalog[ i , 'case_count' ] , DBI::dbGetQuery( db , paste( "SELECT COUNT(*) FROM" , this_tablename ) )[ 1 , 1 ] , na.rm = TRUE )
        
      }
      
    }
    
    # close database connetion
    DBI::dbDisconnect( db , shutdown = TRUE )
    
    # add number of data files read
    # catalog[ i , "dfs_read" ] <- length( datafiles )
    # catalog[ i , "max_size" ] <- length( max( tablesize ) )
    # catalog[ i , "total_obs" ] <- length( sum( tablesize ) )
    
    # process tracker
    cat( paste0( "censo superior" , " catalog entry " , i , " of " , nrow( catalog ) , " stored in '" , catalog[ i , 'dbfolder' ] , "'\r\n\n" ) )
    
  }
  
  
  catalog
  
}

# auxilliary functions
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

remove_special_character <- function(x) {
  stripped_x <- iconv( x , to = "ascii//translit" , sub = "_" )
  stripped_x <- gsub( "('|~|\\^)" , "" , stripped_x , ignore.case = TRUE )
  stripped_x
}