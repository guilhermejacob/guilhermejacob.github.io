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

catalog_pnadc_anual <-
  function( output_dir , ... ){
    
    output_dir <- ifelse( !grepl( "/$" , output_dir ) , paste0( output_dir , "/" ) , output_dir )
    
    url_path <- "ftp://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/"
    # url_path <- "ftp:/ftp.pnadc_anual.gov.br/pdet/microdados/RAIS/"
    
    file_list <- recursive_ftp_scrape( url_path )
    
    # keep .zip, .xls, and .doc only
    # pnadc_anual_files <- pnadc_anual_files[ grepl( "\\.(zip|xls|doc)$" , basename( pnadc_anual_files ) , ignore.case = TRUE ) ]
    
    # keep .zip only
    pnadc_data_files <- file_list[ grepl( "\\.zip$" , basename( file_list ) , ignore.case = TRUE ) ]
    
    # non supplementary
    pnadc_data_files <- pnadc_data_files[ grepl( "visita", basename(pnadc_data_files))]
    
    # build catalog
    catalog <-
      data.frame(
        full_url = pnadc_data_files ,
        year = gsub( "PNADC_|_.*" , "" , basename( pnadc_data_files ) ),
        visit = as.numeric( gsub( ".*visita|(\\.|_).*" , "" , basename( pnadc_data_files ) ) ) ,
        stringsAsFactors = FALSE
      )
    
    # define input files
    pnadc_input_files <- file_list[ grepl( "Input_.*\\.txt$" , basename( file_list ) ) ]
    catalog[ , "input_file" ] <- NULL
    for( i in seq_along( pnadc_data_files ) ) {
      pattern0 <- paste0( "_" , ifelse( catalog[ i , "year" ] <= 2014 , 2012 , catalog[ i , "year" ] ) )
      pattern1 <- paste0( catalog[ i , "visit" ] , "_visita" )
      this_input <- pnadc_input_files [ grepl( paste0( pattern1 , pattern0 ) , basename( pnadc_input_files ) ) ]
      catalog[ i , "input_file" ] <- this_input
    }
    
    # define dictionary files
    pnadc_dict_files <- file_list[ grepl( "dicionario_.*\\.xls$" , basename( file_list ) ) ]
    catalog[ , "dict_file" ] <- NULL
    for( i in seq_along( pnadc_data_files ) ) {
      pattern0 <- paste0( "_" , ifelse( catalog[ i , "year" ] <= 2014 , 2012 , catalog[ i , "year" ] ) )
      pattern1 <- paste0( catalog[ i , "visit" ] , "_visita" )
      this_dict <- pnadc_dict_files [ grepl( paste0( pattern1 , pattern0 ) , basename( pnadc_dict_files ) ) ]
      catalog[ i , "dict_file" ] <- this_dict
    }
    
    # define file date
    these_dates <- gsub( ".*_visita[1-9]|_|\\..*" , "" , basename( pnadc_data_files ) , ignore.case = TRUE )
    these_dates <- ifelse( nchar( these_dates ) == 8 , these_dates , NA )
    catalog[ , "file_date" ] <- these_dates
    
    # define filename
    catalog[ , "output_filename" ] <- paste0( output_dir , paste( catalog[ , "year" ] , catalog[ , "visit" ] , "pnadc anual.fst" ) )
    
    # reorder
    catalog <- catalog[ order( catalog[, "year"] , catalog[, "visit"] , decreasing = FALSE ) , ]
    
    # return catalog
    catalog
    
  }

# build datavault
datavault_pnadc_anual <- function( catalog , datavault_dir , skipExist = TRUE ) {
  
  # get common directory
  url_path <- get_common_dir( tolower( catalog$full_url ) )
  
  # create datavault links
  catalog[ , "datavault_file" ] <- gsub( url_path , paste0( datavault_dir , "/" ), tolower( catalog$full_url ) , ignore.case = TRUE )
  catalog[ , "datavault_file" ] <- gsub( "//" , "/" , catalog[ , "datavault_file" ] , ignore.case = TRUE )
  
  # check for existing files
  existing_files <- file.exists( catalog[ , "datavault_file" ] )
  
  # create directories
  # lapply( unique( dirname( catalog[ , "datavault_file" ] ) ), function( this_dir ){ if ( !dir.exists( this_dir ) ) dir.create( this_dir , recursive = TRUE ) } )
  
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
      
      # download dictionary file
      this_local_dict <- file.path( datavault_dir , "Docs", basename( catalog[ i , "dict_file" ] ) )
      if ( !file.exists( this_local_dict ) ) {
        dir.create( dirname( this_local_dict ) , recursive = TRUE , showWarnings = FALSE )
        # file.create( this_local_dict , showWarnings = FALSE )
        download.file( catalog[ i , "dict_file" ] , this_local_dict , quiet = TRUE , mode = "wb" )
      }
      catalog[ i , "dict_file" ] <- this_local_dict
      
      # process tracker
      cat( "file" , i , "out of" , nrow( catalog ) , "downloaded to" , datavault_dir , "\r")
      
    }
    
  }
  
  # print message
  cat( "\npnadc_anual datavault was built at" , datavault_dir , "\n" )
  
  # return catalog
  catalog
  
}

build_pnadc_anual <-
  function( catalog , ... ){
    
    # load libraries
    library(fst)
    library(lodown)
    
    # create temporary files
    tf <- tempfile()
    tf2 <- tempfile()
    
    # loop through entries
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
      
      # extract file
      unzip( tf , exdir = paste0( tempdir() , "/unzips" ) )
      
      unzipped_files <- list.files( paste0( tempdir() , "/unzips" ) , full.names = TRUE )
      
      txt_file <- grep( "\\.txt$" , unzipped_files , ignore.case = TRUE , value = TRUE )
      
      if( length( txt_file ) != 1 ) stop( 'only expecting one txt file within each zip' )
      
      # ..and read that text file directly into an R data.frame
      # using the sas importation script downloaded before this big fat loop
      x <- suppressWarnings( lodown:::read_SAScii( txt_file , tf2 , sas_encoding = "latin1" ) ) 
      
      # immediately make every field numeric
      for( j in names( x ) ) x[ , j ] <- as.numeric( as.character( x[ , j ] ) )
      # x[ , ] <- apply( x[ , ] , 2 , function(this_col) as.numeric( as.character( this_col ) ) )
      
      # convert all column names to lowercase
      names( x ) <- tolower( names( x ) )
      
      # create output folder
      dir.create( dirname( catalog[ i , 'output_filename' ] ) , showW )
      
      # save data
      write.fst( x , path = catalog[ i , 'output_filename' ] , compress = 100 )
      
      # store case count
      catalog[ i , 'case_count' ] <- nrow( x )
      
      # delete the temporary files
      file.remove( tf , unzipped_files , tf2 )
      
      cat( paste0( "pnadc anual catalog entry " , i , " of " , nrow( catalog ) , " stored at '" , catalog[ i , 'output_filename' ] , "'\r\n\n" ) )
      
    }
    
    catalog
    
  }

