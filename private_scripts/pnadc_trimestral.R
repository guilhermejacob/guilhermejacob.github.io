downloader::source_url( "https://raw.githubusercontent.com/guilhermejacob/guilhermejacob.github.io/master/scripts/install_packages.R" , quiet = TRUE , prompt = TRUE )

# output_dir <- "/Volumes/Trabalho/Teste"

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
getlisting <- function( these_urls ) {
  these_urls <- ifelse( grepl( "\\/$" , these_urls ) , these_urls , paste0( these_urls , "/" ) )
  res <- lapply( these_urls , function( x ) { if ( !is.null( x ) ) paste0( x , RCurl::curlPercentEncode( unlist( strsplit( RCurl::getURL( x, dirlistonly = TRUE ) , "\n" ) ) ) ) else NULL } )
  res <- unlist( res )
  is.file <- grepl( "\\.[a-z]{3}$" , basename( res ) , ignore.case = TRUE )
  res[ !is.file ] <- ifelse( grepl( "\\/$" , res[ !is.file ] ) , res[ !is.file ] , paste0( res[ !is.file ] , "/" ) )
  list( dirs = if ( any(!is.file) ) { res[ !is.file ] } else { NULL } , 
        files = if ( any(is.file) ) { res[ is.file ] } else { NULL } )
}

recursive_ftp_scrape <- function( main_url ) {
  
  final_files <- NULL
  
  directories <- main_url
  # directories <- directories[ 1:5 ]
  
  while ( length(directories) > 0) {
    listing <- getlisting(directories)
    directories <- listing[[1]]
    files <- listing[[2]]
    final_files <- c(final_files, files )
  }
  
  final_files
  
}

remove_special_character <- function(x) {
  stripped_x <- iconv( x , to = "ascii//translit" , sub = "_" )
  stripped_x <- gsub( "('|~|\\^)" , "" , stripped_x , ignore.case = TRUE )
  stripped_x
}

catalog_pnadc <-
  function( output_dir , ... ){
    
    output_dir <- ifelse( !grepl( "/$" , output_dir ) , paste0( output_dir , "/" ) , output_dir )
    
    # define main url path
    url_path <- "ftp://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados/"
    
    # scrape website
    file_list <- recursive_ftp_scrape( url_path )
    
    # drop suplementos
    file_list <- file_list[ !grepl( "Suplementos" , file_list , ignore.case = TRUE ) ]
    
    # keep .zip only
    pnadc_data_files <- file_list[ grepl( "^PNADC_.*\\.zip$" , basename( file_list ) , ignore.case = TRUE ) ]
    
    # build catalog
    catalog <-
      data.frame(
        full_url = pnadc_data_files ,
        quarter = substr( gsub( "PNADC_|_.*" , "" , basename( pnadc_data_files ) ) , 1 , 2 ) ,
        year = substr( gsub( "PNADC_|_.*" , "" , basename( pnadc_data_files ) ) , 3, 6 ),
        stringsAsFactors = FALSE
      )
    
    # define input files
    catalog[ , "input_file" ] <- file_list[ grepl( "Dicionario" , file_list , ignore.case = TRUE ) ] 
    
    # define filename
    catalog[ , "output_filename" ] <- file.path( output_dir , paste0( "pnadc " , catalog[ , "year" ] , " " , catalog[ , "quarter" ] , ".fst" ) )
    
    # reorder
    catalog <- catalog[ order( catalog[, "year"] , catalog[, "quarter"] , decreasing = FALSE ) , ]
    
    # return catalog
    catalog
    
  }

# build datavault
datavault_pnadc <- function( catalog , datavault_dir , skipExist = TRUE ) {
  
  # get common directory
  url_path <- get_common_dir( tolower( catalog$full_url ) )
  
  # create datavault links
  catalog[ , "datavault_file" ] <- gsub( url_path , paste0( datavault_dir , "/" ), tolower( catalog$full_url ) , ignore.case = TRUE )
  catalog[ , "datavault_file" ] <- gsub( "//" , "/" , catalog[ , "datavault_file" ] , ignore.case = TRUE )
  
  # check for existing files
  existing_files <- file.exists( catalog[ , "datavault_file" ] )
  
# if there isn't any non-downloaded, run download procedure:
  if ( any( !existing_files ) ) {
    
    # print message
    cat( sum( 1*!existing_files ) , "missing files detected.\nDownloading missing files only.\n")
    
    # download files to datavault directory
    for ( i in seq_len( nrow(catalog) )[ !existing_files ] ) {
      
      # skip existing file
      if( skipExist & existing_files[ i ] ) next()
      
      # download file
      if ( !dir.exists( dirname( catalog[ i , "datavault_file" ] ) ) ) dir.create( dirname( catalog[ i , "datavault_file" ] ) , recursive = TRUE )
      download.file( catalog[ i , "full_url" ] , catalog[ i , "datavault_file" ] , quiet = FALSE )
      
      # download input file
      this_local_input <- file.path( datavault_dir , "Docs", basename( catalog[ i , "input_file" ] ) )
      if ( !file.exists( this_local_input ) ) {
        dir.create( dirname( this_local_input ) , recursive = TRUE , showWarnings = FALSE )
        # file.create( this_local_input , showWarnings = FALSE )
        download.file( catalog[ i , "input_file" ] , this_local_input , quiet = TRUE )
      }
      catalog[ i , "input_file" ] <- this_local_input
      
      # process tracker
      cat( "file" , i , "out of" , nrow( catalog ) , "downloaded to" , datavault_dir , "\r")
      
    }
    
  }
  
  # print message
  cat( "\npnadc datavault was built at" , datavault_dir , "\n" )
  
  # return catalog
  catalog
  
}


build_pnadc <-
  function( catalog , ... ){
    
    # define temporary files and folder
    tf <- tempfile()
    tf2 <- tempfile()
    td <- file.path( tempdir() , "unzips" )
    
    for ( i in seq_len( nrow( catalog ) ) ){
      
      # download the file
      if ( is.null( catalog[ i , "datavault_file" ] ) ) {
        download.file( catalog[ i , "full_url" ] , tf , quiet = TRUE , mode = "wb" )
      } else if ( is.na( catalog[ i , "datavault_file" ] ) ) {
        download.file( catalog[ i , "full_url" ] , tf , quiet = TRUE , mode = "wb" ) 
      } else {
        file.copy( catalog[ i , "datavault_file" ] , tf )
      }
      
      # download input the file
      if ( grepl( "^ftp://" , catalog[ i , "input_file" ] ) ) {
        download.file( catalog[ i , "input_file" ] , tf2 , quiet = TRUE , mode = "wb" )
      } else {
        file.copy( catalog[ i , "input_file" ] , tf2 )
      }
      
      # unzip downloaded files
      unzip( tf , exdir = td )
      unzip( tf2 , exdir = td )
      
      # list unzipped files
      unzipped_files <- list.files( td , full.names = TRUE )
      
      # detect data file
      datafile <- unzipped_files [ grepl( "^PNADC_" , basename( unzipped_files ) , ignore.case = TRUE ) ]
      
      # detect SAS input file
      sas_input <- unzipped_files [ grepl( "^Input_" , basename( unzipped_files ) , ignore.case = TRUE ) ]
      
      # ..and read that text file directly into an R data.frame
      # using the sas importation script downloaded before this big fat loop
      x <- suppressWarnings( lodown:::read_SAScii( datafile , sas_input , sas_encoding = "latin1" ) ) 
      
      # immediately make every field numeric
      for( j in names( x ) ) x[ , j ] <- as.numeric( as.character( x[ , j ] ) )
      
      # convert all column names to lowercase
      names( x ) <- tolower( names( x ) )
      
      # create directory structure
      if ( !dir.exists( dirname( catalog[ i , 'output_filename' ] ) ) ) { dir.create( dirname( catalog[ i , 'output_filename' ] ) , recursive = TRUE ) }
      
      # save file
      fst::write_fst( x , path = catalog[ i , 'output_filename' ] , compress = 100 )
      
      # store case count
      catalog[ i , 'case_count' ] <- nrow( x )
      
      # delete the temporary files
      file.remove( tf , unzipped_files , tf2 )
      
      # delete temporary directory
      unlink( td , recursive = TRUE )
      
      # process tracker
      cat( paste0( "pnadc catalog entry " , i , " of " , nrow( catalog ) , " stored at '" , catalog[ i , 'output_filename' ] , "'\r\n\n" ) )
      
    }
    
    # return catalog
    catalog
    
  }

