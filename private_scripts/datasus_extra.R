downloader::source_url( "https://raw.githubusercontent.com/guilhermejacob/guilhermejacob.github.io/master/scripts/install_packages.R" , quiet = TRUE , prompt = TRUE )


# auxilliary functions
# create local data catalog:
catalog_datasus_extra <- function( output_dir ){
  
  # sus_htmls <- "ftp://ftp.datasus.gov.br/dissemin/publicos/"
  sus_htmls <- paste0( "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/" , c( "Dados" , "doc" ) , "/" )
  # sus_htmls <- sus_htmls[1:2]
  
  these_links <- datasus_ftp_scrape( sus_htmls )
  # these_links <- these_links[ grepl( "\\.(dbc|dbf)$" , basename( these_links ) , ignore.case = TRUE ) ]
  
  catalog <-
    data.frame(
      full_url = these_links ,
      stringsAsFactors = FALSE
    )
  
  datafiles <- grepl( "\\.(dbc|dbf)$" , these_links , ignore.case = TRUE )
  
  catalog$type[ datafiles ] <- ifelse( grepl( "\\.(dbc|dbf)$" , these_links , ignore.case = TRUE ) , substr( basename( these_links ) , 1 , 2 ) , NA )[ datafiles ]
  
  catalog$output_filename <-
    gsub( "200508_/" , "" ,
          gsub( "dados/" , "" ,
                gsub( "dbc$" , "fst" ,
                      gsub( "ftp://ftp.datasus.gov.br/dissemin/publicos" , output_dir , tolower( these_links ) ) ,
                      ignore.case = TRUE )
          )
    )
  
  year_lines <- paste0( "20", substr( gsub( "[^0-9]" , "" , basename( these_links ) ) , 1, 2 ) )
  catalog$year[datafiles] <- as.numeric( year_lines )[datafiles]
  
  month_lines <- paste0( substr( gsub( "[^0-9]" , "" , basename( these_links ) ) , 3, 4 ) )
  catalog$month[datafiles] <- as.numeric( month_lines )[datafiles]
    
  catalog
  
}

# datavault
datavault_datasus_extra <- function( catalog , datavault_dir , skipExist = TRUE ) {
  
  catalog[ , "datavault_file" ] <- gsub( "ftp://ftp.datasus.gov.br/dissemin/publicos" , datavault_dir , catalog$full_url )
  catalog[ , "datavault_file" ] <- gsub( "//" , "/" , catalog[ , "datavault_file" ] , ignore.case = TRUE )
  
  # check for existing files
  existing_files <- file.exists( catalog[ , "datavault_file" ] )
  
  # remove existing
  catalog <- catalog[ !existing_files & skipExist , ]
  
  # parallel
  plyr::llply( seq_along( catalog$full_url ) , function( i ) {
    
    # skip existing file
    if( skipExist & existing_files[ i ] ){ cat( "file" , i , "out of" , nrow( catalog ) , "downloaded to" , datavault_dir , "\r") ; next() }
    
    if ( !dir.exists( dirname( catalog[ i , "datavault_file" ] ) ) ) { dir.create( dirname( catalog[ i , "datavault_file" ] ) , recursive = TRUE ) }
    
    # download file
    download.file( catalog[ i , "full_url" ], catalog[ i , "datavault_file" ] , mode = "wb" , quiet = TRUE )
    
    # process tracker
    cat( "file" , i , "out of" , nrow( catalog ) , "downloaded to" , datavault_dir , "\r")
    
  } , .parallel = TRUE )
  
  
  # print message
  cat( "\ndatasus datavault was built at" , datavault_dir , "\n" )
  
  # return catalog
  catalog
  
}

# build dataset from catalog:
build_datasus_extra <- function( catalog ) {
  
  # create all folders:
  # for ( this_dir in unique( dirname( catalog$output_filename ) ) ) { dir.create( this_dir , recursive = TRUE )  }
  
  if ( !requireNamespace( "read.dbc" , quietly = TRUE ) ) stop( "read.dbc needed for this function to work. to install it, type `install.packages( 'read.dbc' )`" , call. = FALSE )
  
  tf <- tempfile()
  
  for ( i in seq_len( nrow( catalog ) ) ) {
    
    # download the file
    if ( is.null( catalog[ i , "datavault_file" ] ) ) {
      download.file( catalog[ i , "full_url" ] , tf , quiet = TRUE , mode = "wb" )
    } else if ( is.na( catalog[ i , "datavault_file" ] ) ) {
      download.file( catalog[ i , "full_url" ] , tf , quiet = TRUE , mode = "wb" ) 
    } else {
      file.copy( catalog[ i , "datavault_file" ] , tf )
    }
    
    if ( !grepl( "dbc$" , catalog[ i , 'full_url' ] , ignore.case = TRUE ) ) {
      
      if ( !dir.exists( dirname( catalog[ i , 'output_filename' ] ) ) ) dir.create( dirname( catalog[ i , 'output_filename' ] ) , showWarnings = FALSE , recursive = TRUE )
      if ( !file.exists( catalog[ i , 'output_filename' ] ) ) file.create( catalog[ i , 'output_filename' ] , showWarnings = FALSE )
      file.copy( tf , catalog[ i , 'output_filename' ] , overwrite = TRUE )
      
    } else {
      
      # read dbc file
      x <- read.dbc::read.dbc( tf , as.is = TRUE )
      
      # force data.table
      x <- data.table::as.data.table( x )
      
      # convert all column names to lowercase
      names( x ) <- tolower( names( x ) )
      
      # add underscores after monetdb illegal names
      for ( j in names( x )[ toupper( names( x ) ) %in% getFromNamespace( "reserved_monetdb_keywords" , "MonetDBLite" ) ] ) names( x )[ names( x ) == j ] <- paste0( j , "_" )
      
      # remove trailing spaces
      names( x ) <- trimws( names( x ) , which = "both" )
      
      catalog[ i , 'case_count' ] <- nrow( x )
      
      # save file
      if ( !dir.exists( dirname( catalog[ i , 'output_filename' ] ) ) ) dir.create( dirname( catalog[ i , 'output_filename' ] ) , recursive = TRUE )
      fst::write_fst( x , path = catalog[ i , 'output_filename' ] , compress = 100 )
      
      # process tracker
      cat( paste0( "datasus catalog entry " , i , " of " , nrow( catalog ) , " stored at '" , catalog[ i , 'output_filename' ] , "'\r" ) )
      
    }
    
    # delete the temporary files
    suppressWarnings( file.remove( tf ) )
    
  }
  
  catalog
  
}

# recursive ftp scrape
datasus_getlisting <- function( these_urls ) {
  these_urls <- ifelse( grepl( "\\/$" , these_urls ) , these_urls , paste0( these_urls , "/" ) )
  # res <- lapply( these_urls , function( x ) { if ( !is.null( x ) ) paste0( x , unlist( strsplit( RCurl::getURL( x, dirlistonly = TRUE ) , "\n" ) ) ) else NULL } )
  res <- plyr::llply( these_urls , function( x ) { if ( !is.null( x ) ) paste0( x , unlist( strsplit( RCurl::getURL( x, dirlistonly = TRUE ) , "\n" ) ) ) else NULL } , .progress = "text" )
  # res <- plyr::llply( these_urls , function( x ) { if ( !is.null( x ) ) paste0( x , unlist( strsplit( RCurl::getURL( x, dirlistonly = TRUE ) , "\n" ) ) ) else NULL } , .parallel = TRUE )
  res <- unlist( res )
  res <- res[ !grepl( "SISPRENATAL\\/201201_\\/Doc$", res ) ] # folder not loading in server
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
  
  while ( length(directories) > 0) {
    listing <- datasus_getlisting(directories)
    directories <- listing[[1]]
    files <- listing[[2]]
    final_files <- c(final_files, files)
  }
  
  final_files
  
}