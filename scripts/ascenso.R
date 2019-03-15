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

catalog_ascenso <-
  function( output_dir , ... ){
    
    output_dir <- ifelse( !grepl( "/$" , output_dir ) , paste0( output_dir , "/" ) , output_dir )
    
    url_path <- "ftp://ftp.ibge.gov.br/Censos/Censo_Demografico_2010/Resultados_do_Universo/Agregados_por_Setores_Censitarios/"
    # url_path <- "ftp:/ftp.pnadc_anual.gov.br/pdet/microdados/RAIS/"
    
    file_list <- files_in_ftp_dir( url_path )
    
    # fix double bars
    file_list <- gsub( "ftp:/" , "ftp://" , file_list )
    
    # keep zip files only
    file_list <- file_list[ grepl( "\\.zip$" , basename( file_list ) , ignore.case = TRUE ) ]
    
    # keep documentation
    doc_file  <- file_list[ grepl( "documentacao" , basename( file_list ) , ignore.case = TRUE ) ]
    file_list <- file_list[ !grepl( "documentacao" , basename( file_list ) , ignore.case = TRUE ) ]
    
    # build catalog
    catalog2010 <-
      data.frame(
        full_url = file_list ,
        year = 2010 ,
        area_code = gsub( "_2017.*" , "" , basename( file_list ) ),
        documentation = doc_file ,
        stringsAsFactors = FALSE
      )
    
    # 2007
    url_path <- "ftp://ftp.ibge.gov.br/Contagem_da_Populacao_2007/Agregado_por_Setores_Censitarios_2007/"
    
    file_list <- files_in_ftp_dir( url_path )
    
    # fix double bars
    file_list <- gsub( "ftp:/" , "ftp://" , file_list )
    
    # keep documentation
    doc_file  <- file_list[ grepl( "\\.doc" , basename( file_list ) , ignore.case = TRUE ) ]
    file_list <- file_list[ !grepl( "\\.doc" , basename( file_list ) , ignore.case = TRUE ) ]
    
    # keep zip files only
    file_list <- file_list[ grepl( "\\.zip$" , basename( file_list ) , ignore.case = TRUE ) ]
    
    # build catalog
    catalog2007 <-
      data.frame(
        full_url = file_list ,
        year = 2007 ,
        area_code = gsub( ".*_2007_|\\..*" , "" , basename( file_list ) ) ,
        documentation = NA ,
        stringsAsFactors = FALSE
      )
    
    # 2000
    url_path <- "ftp://ftp.ibge.gov.br/Censos/Censo_Demografico_2000/Dados_do_Universo/Agregado_por_Setores_Censitarios/"
    
    file_list <- files_in_ftp_dir( url_path )
    
    # fix double bars
    file_list <- gsub( "ftp:/" , "ftp://" , file_list )
    
    # keep documentation
    doc_file  <- file_list[ grepl( "\\.doc" , basename( file_list ) , ignore.case = TRUE ) ]
    file_list <- file_list[ !grepl( "\\.doc" , basename( file_list ) , ignore.case = TRUE ) ]
    
    # keep zip files only
    file_list <- file_list[ grepl( "\\.zip$" , basename( file_list ) , ignore.case = TRUE ) ]
    
    # build catalog
    catalog2000 <-
      data.frame(
        full_url = file_list ,
        year = 2000 ,
        area_code = gsub( ".*_2000_|\\..*" , "" , basename( file_list ) ),
        documentation = doc_file ,
        stringsAsFactors = FALSE
      )
    
    # stack data
    catalog <- do.call( rbind , list( catalog2010 , catalog2007 , catalog2000 ) )
    
    # define database file
    catalog[ , "dbfile" ] <- paste0( output_dir , "ascenso.sqlite" )
    
    # return full catalog
    catalog
    
  }

# build datavault
datavault_ascenso <- function( catalog , datavault_dir , skipExist = TRUE ) {
  
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
      
      # download dictionary file
      if ( !is.na( catalog[ i , "documentation" ] ) ) {
        this_local_doc <- file.path( datavault_dir , basename( catalog[ i , "documentation" ] ) )
        if ( !file.exists( this_local_doc ) ) {
          dir.create( dirname( this_local_doc ) , recursive = TRUE , showWarnings = FALSE )
          download.file( catalog[ i , "documentation" ] , this_local_doc , quiet = TRUE , mode = "wb" )
        }
        catalog[ i , "documentation" ] <- this_local_doc
      }
      
      # process tracker
      cat( "file" , i , "out of" , nrow( catalog ) , "downloaded to" , datavault_dir , "\r")
      
    }
    
  }
  
  # print message
  cat( "\nascenso datavault was built at" , datavault_dir , "\n" )
  
  # return catalog
  catalog
  
}

build_ascenso <-
  function( catalog , ... ){
    
    tf <- tempfile()
    tf2 <- tempfile()
    
    # create directory structure
    for ( this_dir in unique( dirname( catalog[ , "dbfile" ] ) ) ) { dir.create( this_dir , recursive = TRUE , showWarnings = FALSE ) }
    
    for ( i in seq_len( nrow( catalog ) ) ){
      
      db <- DBI::dbConnect( RSQLite::SQLite() , catalog[ i , "dbfile" ] )
      
      # download the file
      if ( is.null( catalog[ i , "datavault_file" ] ) ) {
        download.file( catalog[ i , "full_url" ] , tf , quiet = TRUE , mode = "wb" )
      } else if ( is.na( catalog[ i , "datavault_file" ] ) ) {
        download.file( catalog[ i , "full_url" ] , tf , quiet = TRUE , mode = "wb" ) 
      } else {
        file.copy( catalog[ i , "datavault_file" ] , tf )
      }
      
      unzip( tf , exdir = paste0( tempdir() , "/unzips" ) )
      
      unzipped_files <- list.files( paste0( tempdir() , "/unzips" ) , full.names = TRUE , recursive = TRUE )
      
      data_files <- grep( "\\.csv$" , unzipped_files , ignore.case = TRUE , value = TRUE )
      
      case_count <- NULL
      for ( this_data_file in data_files ) {
        
        # this_tablename <- gsub( "_.*" , "" , tolower( basename( this_data_file ) ) , ignore.case = TRUE )
        this_tablename <- gsub( "\\..*" , "" , tolower( basename( this_data_file ) ) , ignore.case = TRUE )
        this_tablename <- paste0( this_tablename , "_" , catalog[ i , "year" ] )
        x <- read.csv( this_data_file , sep = ";" , as.is = TRUE , encoding = "latin1" )
        colnames( x ) <- tolower( colnames( x ) )
        x <- x[ , !grepl( "x" , colnames( x ) ) ]
      
        if ( sum( grepl( "^(cod|nome)" , colnames( x ) ) ) <= 1 ) {
          
        }
        x[ , grepl( "^(cod|nome)" , colnames( x ) ) ] <- apply( x[ , grepl( "^(cod|nome)" , colnames( x ) ) ] , 2 , function(y) suppressWarnings( as.character(y) ) )
        x[ , !grepl( "^(cod|nome)" , colnames( x ) ) ] <- apply( x[ , !grepl( "^(cod|nome)" , colnames( x ) ) ] , 2 , function(y) as.numeric( gsub( "," , "." , y) ) )
        x[ , !grepl( "^(cod|nome)" , colnames( x ) ) ] <- apply( x[ , !grepl( "^(cod|nome)" , colnames( x ) ) ] , 2 , function(y) suppressWarnings( as.numeric(y) ) )
        
        DBI::dbWriteTable( db , this_tablename , x , overwrite = TRUE )
        case_count <- c( case_count , nrow(x) )
        rm( x )
        
      }
      
      
      catalog[ i , 'case_count' ] <- nrow( x )
      
      # delete the temporary files
      file.remove( tf , unzipped_files , tf2 )
      
      cat( paste0( "pnadc anual catalog entry " , i , " of " , nrow( catalog ) , " stored at '" , catalog[ i , 'output_filename' ] , "'\r\n\n" ) )
      
    }
    
    catalog
    
  }

