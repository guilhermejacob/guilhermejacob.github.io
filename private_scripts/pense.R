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

catalog_pense <-
  function( output_dir , ... ){
    
    output_dir <- ifelse( !grepl( "/$" , output_dir ) , paste0( output_dir , "/" ) , output_dir )
    
    url_path <- "ftp://ftp.ibge.gov.br/pense"
    # url_path <- "ftp:/ftp.pense.gov.br/pdet/microdados/RAIS/"
    
    file_list <- recursive_ftp_scrape( url_path )
    
    # keep microdata only
    file_list <- file_list[ grepl( "microdados" , file_list , ignore.case = TRUE ) ]
    
    # drop .txt files
    # file_list <- file_list[ !grepl( "\\.txt$" , basename( file_list ) , ignore.case = TRUE ) ]
    
    # keep .zip files only
    file_list <- file_list[ grepl( "\\.zip$" , basename( file_list ) , ignore.case = TRUE ) ]
    
    # build catalog
    catalog <-
      data.frame(
        full_url = file_list ,
        year = gsub( ".*pense\\/|\\/.*" , "" , dirname(file_list) ) ,
        stringsAsFactors = FALSE
      )
    
    # set output folder
    catalog[ grepl( "\\.zip$" , basename( file_list ) , ignore.case = TRUE ) , "doc_folder" ] <- 
      paste0( output_dir , catalog[ , "year" ] , "/Documentos" )
    
    # set output files
    catalog[ grepl( "\\.zip$" , basename( file_list ) , ignore.case = TRUE ) , "output_folder" ] <- 
      paste0( output_dir , catalog[ , "year" ] , "/Dados" )
    
    subset( catalog , year > 2009 )
    
  }


build_pense <-
  function( catalog , ... ){
    
    tf <- tempfile()
    tf2 <- tempfile()
    td <- tempdir()
    
    for ( i in seq_len( nrow( catalog ) ) ){
      
      # download the file
      if ( is.null( catalog[ i , "datavault_file" ] ) ) {
        download.file( catalog[ i , "full_url" ] , tf , quiet = TRUE , mode = "wb" )
      } else if ( is.na( catalog[ i , "datavault_file" ] ) ) {
        download.file( catalog[ i , "full_url" ] , tf , quiet = TRUE , mode = "wb" ) 
      } else {
        file.copy( catalog[ i , "datavault_file" ] , tf )
      }
      
      unzip( tf , exdir = paste0( td , "/unzips" ) )
      
      if ( catalog[ i , "year" ] == 2012 ) {
        
        unzipped_files <- list.files( paste0( td , "/unzips" ) , full.names = FALSE , recursive = TRUE )
        # sapply( iconvlist() , function(x)  iconv( sapply( unzipped_files , URLdecode )[[2]] , from = x ) )
        # unzipped_files <- iconv( sapply( unzipped_files , URLdecode , USE.NAMES = FALSE ) , from = "850" )
        unzipped_files <- paste0( td , "/unzips/" , unzipped_files )
        
        # read escolas
        this_file <- unzipped_files[ grepl( "^escolas.xlsx" , basename( unzipped_files ) ) ]
        x <- suppressWarnings( readxl::read_xlsx( this_file ) )
        
        names(x) <- tolower( names(x) )
        
        this_output_file <- paste0( catalog[ i , "output_folder" ] , "/pense 2012 escolas.Rds" )
        if ( !dir.exists( dirname(this_output_file) ) ) dir.create( dirname(this_output_file) , recursive = TRUE )
        saveRDS( x , file = this_output_file )
        
        # read estudantes
        this_file <- unzipped_files[ grepl( "^estudantes.xlsx" , basename( unzipped_files ) ) ]
        x <- suppressWarnings( readxl::read_xlsx( this_file ) )
        
        names(x) <- tolower( names(x) )
        
        this_output_file <- paste0( catalog[ i , "output_folder" ] , "/pense 2012 estudantes.Rds" )
        if ( !dir.exists( dirname(this_output_file) ) ) dir.create( dirname(this_output_file) , recursive = TRUE )
        saveRDS( x , file = this_output_file )
        
        # read estudantes e escolas
        this_file <- unzipped_files[ grepl( "^estudantesescolas.xlsx" , basename( unzipped_files ) ) ]
        x <- suppressWarnings( readxl::read_xlsx( this_file ) )
        
        names(x) <- tolower( names(x) )
        
        this_output_file <- paste0( catalog[ i , "output_folder" ] , "/pense 2012 estudantes e escolas.Rds" )
        if ( !dir.exists( dirname(this_output_file) ) ) dir.create( dirname(this_output_file) , recursive = TRUE )
        saveRDS( x , file = this_output_file )
        
        
        catalog[ i , 'case_count' ] <- nrow( x )
        # delete the temporary files
        file.remove( tf , unzipped_files , tf2 )
        unlink( paste0( td , "/unzips/" ) , recursive = TRUE )
        
        cat( paste0( "pense catalog entry " , i , " of " , nrow( catalog ) ) )
        
      } else if ( catalog[ i , "year" ] == 2015 ) {
        
        unzipped_files <- list.files( paste0( td , "/unzips" ) , full.names = FALSE , recursive = TRUE )
        # sapply( iconvlist() , function(x)  iconv( sapply( unzipped_files , URLdecode )[[2]] , from = x ) )
        # unzipped_files <- iconv( sapply( unzipped_files , URLdecode , USE.NAMES = FALSE ) , from = "850" )
        unzipped_files <- paste0( td , "/unzips/" , unzipped_files )
        
        # read escolas
        this_file <- unzipped_files[ grepl( "^PENSE_.*_ESCOLA.CSV" , basename( unzipped_files ) ) ]
        x <- suppressWarnings( read.csv( this_file , sep = ";" ) )
        
        names(x) <- tolower( names(x) )
        
        tipo <- ifelse( grepl( "AMOSTRA1" , basename( this_file ) ) ,  " amostra1" , " amostra2" )
        this_output_file <- paste0( catalog[ i , "output_folder" ] , "/pense 2015" , tipo , " escolas.Rds" )
        if ( !dir.exists( dirname(this_output_file) ) ) dir.create( dirname(this_output_file) , recursive = TRUE )
        saveRDS( x , file = this_output_file )
        
        # read estudantes 
        this_file <- unzipped_files[ grepl( "^PENSE_.*_ALUNO.CSV" , basename( unzipped_files ) ) ]
        x <- suppressWarnings( read.csv( this_file , sep = ";" ) )
        
        names(x) <- tolower( names(x) )
        
        tipo <- ifelse( grepl( "AMOSTRA1" , basename( this_file ) ) ,  " amostra1" , " amostra2" )
        this_output_file <- paste0( catalog[ i , "output_folder" ] , "/pense 2015" , tipo , " estudantes.Rds" )
        if ( !dir.exists( dirname(this_output_file) ) ) dir.create( dirname(this_output_file) , recursive = TRUE )
        saveRDS( x , file = this_output_file )
        
        # read estudantes e escolas
        this_file <- unzipped_files[ grepl( "^PENSE_.*_ALUNOESCOLA.CSV" , basename( unzipped_files ) ) ]
        x <- suppressWarnings( read.csv( this_file , sep = ";" ) )
        
        names(x) <- tolower( names(x) )
        
        tipo <- ifelse( grepl( "AMOSTRA1" , basename( this_file ) ) ,  " amostra1" , " amostra2" )
        this_output_file <- paste0( catalog[ i , "output_folder" ] , "/pense 2015" , tipo , " estudantes.Rds" )
        if ( !dir.exists( dirname(this_output_file) ) ) dir.create( dirname(this_output_file) , recursive = TRUE )
        saveRDS( x , file = this_output_file )
        
        
        catalog[ i , 'case_count' ] <- nrow( x )
        
        for ( this_doc_file in unzipped_files[ grepl( "dicio" , basename( unzipped_files ) , ignore.case = TRUE ) ] ) {
          this_output_file <- paste0( catalog[ i , "doc_folder" ] , "/" , tolower( basename( this_doc_file ) ) )
          if ( !dir.exists( dirname(this_output_file) ) ) dir.create( dirname(this_output_file) , recursive = TRUE )
          file.copy( this_doc_file , this_output_file )
        }
        
        # delete the temporary files
        file.remove( tf , unzipped_files , tf2 )
        unlink( paste0( td , "/unzips/" ) , recursive = TRUE )
        
        cat( paste0( "pense catalog entry " , i , " of " , nrow( catalog ) ) )
        
      }
      
    }
    
    catalog
    
  }

# build datavault
datavault_pense <- function( catalog , datavault_dir , skipExist = TRUE ) {
  
  # get common directory
  # url_path <- get_common_dir( tolower( catalog$full_url ) )
  url_path <- get_common_dir( tolower( catalog$full_url ) )
  
  # create datavault links
  catalog$datavault_file <- gsub( url_path , paste0( datavault_dir , "/rais/" ), tolower( catalog$full_url ) , ignore.case = TRUE )
  catalog$datavault_file <- gsub( "//" , "/" , catalog$datavault_file , ignore.case = TRUE )
  
  # check for existing files
  existing_files <- file.exists( catalog$datavault_file )
  
  # create directories
  # lapply( unique( dirname( catalog$datavault_file ) ), function( this_dir ){ if ( !dir.exists( this_dir ) ) dir.create( this_dir , recursive = TRUE ) } )
  
  # if there isn't any non-downloaded, run download procedure:
  if ( any( !existing_files ) ) {
    
    # print message
    cat( sum( 1*!existing_files ) , "missing files detected.\nDownloading missing files only.\n")
    
    # download files to datavault directory
    for ( i in seq_len( nrow(catalog) )[ !existing_files ] ) {
      
      # skip existing file
      if( skipExist & existing_files[ i ] ) next()
      
      # download file
      if ( !dir.exists( dirname( catalog$datavault_file[ i ] ) ) ) { dir.create( dirname( catalog$datavault_file[ i ] ) , recursive = TRUE ) }
      download.file( catalog$full_url[ i ] , catalog$datavault_file[ i ] , quiet = FALSE )
      
      # process tracker
      cat( "file" , i , "out of" , nrow( catalog ) , "downloaded to" , datavault_dir , "\r")
      
    }
    
  }
  
  # print message
  cat( "\npense datavault was built at" , datavault_dir , "\n" )
  
  # return catalog
  catalog
  
}