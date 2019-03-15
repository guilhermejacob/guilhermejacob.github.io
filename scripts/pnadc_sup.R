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

catalog_pnadc_sup <-
  function( output_dir , ... ){

    output_dir <- ifelse( !grepl( "/$" , output_dir ) , paste0( output_dir , "/" ) , output_dir )

    url_path <- "ftp://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados/"
    # url_path <- "ftp:/ftp.pnadc_sup.gov.br/pdet/microdados/RAIS/"

    file_list <- recursive_ftp_scrape( url_path )

    # keep suplementos only
    file_list <- file_list[ grepl( "Suplementos" , file_list , ignore.case = TRUE ) ]

    # keep .zip only
    pnadc_data_files <- file_list[ grepl( "^PNADC_.*\\.zip$" , basename( file_list ) , ignore.case = TRUE ) ]

    # build catalog
    catalog <-
      data.frame(
        full_url = pnadc_data_files ,
        quarter = substr( gsub( "PNADC_|_.*" , "" , basename( pnadc_data_files ) ) , 1 , 2 ) ,
        year = substr( gsub( "PNADC_|_.*" , "" , basename( pnadc_data_files ) ) , 3, 6 ),
        topic = gsub( "PNADC_[0-9]{6}_|_.*|\\..*" , "" , basename( pnadc_data_files ) ) ,
        stringsAsFactors = FALSE
      )

    # define input files
    pnadc_input_files <- file_list[ grepl( "Input_.*\\.txt$" , basename( file_list ) ) ]
    catalog[ , "input_file" ] <- NULL
    for( i in seq_along( pnadc_data_files ) ) {
      pattern_folder <- paste0( "\\/" , catalog[ i , "year" ] , "\\/" )
      pattern_file <- paste0( "_" , catalog[ i , "topic" ] )
      this_input <- pnadc_input_files [ grepl( pattern_file , basename( pnadc_input_files ) , ignore.case = TRUE ) & grepl( pattern_folder , pnadc_input_files , ignore.case = TRUE ) ]
      catalog[ i , "input_file" ] <- this_input
    }

    # define dictionary files
    pnadc_dict_files <- file_list[ grepl( "dicionario_.*\\.xls$" , basename( file_list ) ) ]
    catalog[ , "dict_file" ] <- NULL
    for( i in seq_along( pnadc_data_files ) ) {
      pattern_folder <- paste0( "\\/" , catalog[ i , "year" ] , "\\/" )
      pattern_file <- paste0( "_" , catalog[ i , "topic" ] )
      this_dict <- pnadc_dict_files [ grepl( pattern_file , basename( pnadc_dict_files ) , ignore.case = TRUE ) & grepl( pattern_folder , pnadc_dict_files , ignore.case = TRUE ) ]
      catalog[ i , "dict_file" ] <- this_dict
    }

    # define filename
    catalog[ , "output_filename" ] <- paste0( output_dir , paste( "pnadc" , catalog[ , "topic" ] , catalog[ , "quarter" ] , catalog[ , "year" ] ) , ".Rds" )

    # reorder
    catalog <- catalog[ order( catalog[, "year"] , catalog[, "quarter"] , decreasing = FALSE ) , ]

    catalog

  }


build_pnadc_sup <-
  function( catalog , ... ){
    
    tf <- tempfile()
    tf2 <- tempfile()
    
    # create directory structure
    for ( i in seq_len( nrow( catalog ) ) )  if ( !dir.exists( dirname( catalog[ i , 'output_filename' ] ) ) ) { dir.create( dirname( catalog[ i , 'output_filename' ] ) , recursive = TRUE ) }
    
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
      
      saveRDS( x , file = catalog[ i , 'output_filename' ] )
      
      catalog[ i , 'case_count' ] <- nrow( x )
      
      # delete the temporary files
      file.remove( tf , unzipped_files , tf2 )
      
      cat( paste0( "pnadc anual catalog entry " , i , " of " , nrow( catalog ) , " stored at '" , catalog[ i , 'output_filename' ] , "'\r\n\n" ) )
      
    }
    
    catalog
    
  }

