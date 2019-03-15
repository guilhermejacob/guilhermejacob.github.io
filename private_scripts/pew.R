downloader::source_url( "https://raw.githubusercontent.com/guilhermejacob/guilhermejacob.github.io/master/scripts/install_packages.R" , quiet = TRUE , prompt = TRUE )

get_catalog_pew <-
  function( output_dir , ... ){
    
    catalog <- NULL
    
    dl_homepage <- "http://www.pewresearch.org/data/download-datasets/"
    
    # figure out research areas #
    research_areas <- xml2::read_html( dl_homepage )
    
    ra_link_refs <- rvest::html_attr( rvest::html_nodes( research_areas , "a" ) , "href" )
    
    ra_link_text <- rvest::html_text( rvest::html_nodes( research_areas , "a" ) )
    
    ra_link_text <- ra_link_text[ grep( "/datasets/" , ra_link_refs ) ]
    
    ra_link_refs <- ra_link_refs[ grep( "/datasets/" , ra_link_refs ) ]
    
    
    for( topic_num in seq_along( ra_link_text ) ){
      
      # figure out years #
      topic_page <- xml2::read_html( ra_link_refs[ topic_num ] )
      
      to_link_refs <- rvest::html_attr( rvest::html_nodes( topic_page , "a" ) , "href" )
      
      to_link_text <- rvest::html_text( rvest::html_nodes( topic_page , "a" ) )
      
      year_link_refs <- to_link_refs[ grep( "^[0-9][0-9][0-9][0-9]$" , to_link_text ) ]
      
      year_link_text <- to_link_text[ grep( "^[0-9][0-9][0-9][0-9]$" , to_link_text ) ]
      
      # each topic should have something
      stopifnot( length( year_link_text ) > 0 )
      
      for( year_num in seq_along( year_link_text ) ){
        
        # figure out pages #
        year_page <- xml2::read_html( year_link_refs[ year_num ] )
        
        data_link_refs <- rvest::html_attr( rvest::html_nodes( year_page , "a" ) , "href" )
        
        data_link_text <- rvest::html_text( rvest::html_nodes( year_page , "a" ) )
        
        page_list <- as.numeric( unique( gsub( "(.*)/page/([0-9]+)/$" , "\\2" , grep( "/page/[0-9]+/$" , data_link_refs , value = TRUE ) ) ) )
        
        if( length( page_list ) == 0 ) all_pages <- 1 else all_pages <- seq( max( page_list ) )
        
        
        for( page_num in all_pages ){
          
          # figure out datasets #
          these_data_webpage <- paste0( year_link_refs[ year_num ] , "/page/" , page_num , "/" )
          
          these_data_page <- xml2::read_html( these_data_webpage )
          
          these_data_link_link <- rvest::html_attr( rvest::html_nodes( these_data_page , "a" ) , "dataset-dl-link" )
          
          these_data_link_refs <- rvest::html_attr( rvest::html_nodes( these_data_page , "a" ) , "href" )
          
          these_data_link_title <- rvest::html_attr( rvest::html_nodes( these_data_page , "div" ) , "dataset-title" )
          
          these_data_link_text <- rvest::html_text( rvest::html_nodes( these_data_page , "a" ) )
          
          these_data_link_text <- these_data_link_text[ !is.na( these_data_link_link ) ]
          
          these_data_link_title <- these_data_link_title[ !is.na( these_data_link_title ) ]
          
          these_data_link_refs <- these_data_link_refs[ !is.na( these_data_link_link ) ]
          
          these_data_link_link <- these_data_link_link[ !is.na( these_data_link_link ) ]
          
          stopifnot( length( these_data_link_title ) == length( these_data_link_link ) )
          
          if( length( these_data_link_refs ) > 0 ){
            
            this_catalog <-
              data.frame(
                full_url = these_data_link_link ,
                name = these_data_link_title ,
                download_id = gsub( "(.*)\\((.*)\\)" , "\\2" , these_data_link_refs ) ,
                year = year_link_text[ year_num ] ,
                topic = ra_link_text[ topic_num ] ,
                stringsAsFactors = FALSE
              )
            
            this_catalog[ this_catalog$topic == this_catalog$year , 'year' ] <- gsub( "(.*)([0-9][0-9][0-9][0-9])(.*)" , "\\2" , this_catalog[ this_catalog$topic == this_catalog$year , 'name' ] )
            
            # this_catalog[ !grepl( "^[0-9][0-9][0-9][0-9]$" , this_catalog$year ) , 'year' ] <- NA
            
            # keep only datasets with dl-links for now..  this misses a few datasets
            this_catalog <- subset( this_catalog , these_data_link_link != '' )
            
            catalog <- rbind( catalog , this_catalog )
            
            cat( paste0( "loading " , "pew" , " catalog from " , these_data_webpage , "\r\n\n" ) )
            
          }
          
        }
        
      }
      
    }
    
    catalog$output_folder <- paste0( output_dir , "/" , catalog$topic , "/" , ifelse( !is.na( catalog$year ) , catalog$year , "" ) , "/" , gsub( "/|:|\\(|\\)" , "_" , catalog$name ) )
    
    catalog$output_folder <- gsub( " +" , " " , iconv( catalog$output_folder , "" , "ASCII//TRANSLIT" , sub = " " ) )
    
    catalog$output_folder <- gsub( 'a\\?|\\"' , '' , catalog$output_folder )
    
    # broken zips
    catalog <- 
      catalog[ 
        !( catalog$full_url %in% 
             c( 
               
               # https://github.com/tidyverse/haven/issues/342
               "https://assets.pewresearch.org/wp-content/uploads/sites/5/datasets/Sept07.zip" ,
               "http://assets.pewresearch.org/wp-content/uploads/sites/5/datasets/Iraq2003-2.zip" ,
               "http://assets.pewresearch.org/wp-content/uploads/sites/5/datasets/Oct01NII.zip" ,
               "http://assets.pewresearch.org/wp-content/uploads/sites/5/datasets/april01nii.zip" ,
               
               
               "http://assets.pewresearch.org/wp-content/uploads/sites/11/2015/12/Religion-in-Latin-America-Dataset.zip" , 
               "http://www.people-press.org/files/datasets/Jan%2030-Feb%202%202014%20omnibus.zip" , 
               "http://www.pewforum.org/datasets/a-portrait-of-jewish-americans/?submitted" ,
               'http://assets.pewresearch.org/wp-content/uploads/sites/5/datasets/Jan%2030-Feb%202%202014%20omnibus.zip' ,
               'http://assets.pewresearch.org/wp-content/uploads/sites/5/datasets/Oct+27-30+2011+omnibus.zip' ,
               'http://assets.pewresearch.org/wp-content/uploads/sites/5/datasets/Oct16.zip' ,
               'http://assets.pewresearch.org/wp-content/uploads/sites/14/2015/05/November-2010-–-Facebook-and-Social-Support.zip' ,
               'http://www.people-press.org/files/datasets/Aug16.zip' ,
               'http://assets.pewresearch.org/wp-content/uploads/sites/14/old-datasets/November-2010--Paid-Content-(Omnibus).zip' ,
               'http://assets.pewresearch.org/wp-content/uploads/sites/5/datasets/June16%20public.zip' ,
               
               'http://assets.pewresearch.org/wp-content/uploads/sites/2/2017/07/20111442/Pew-GAP-Spring-2007-survey-for-website.zip' ,
               'http://assets.pewresearch.org/wp-content/uploads/sites/2/2009/06/Pew-GAP-Spring-2009-survey-for-website.zip'
             ) ) , ]
    
    catalog
    
  }

# datavault
datavault_pew <- function( catalog , datavault_dir , skipExist = TRUE ) {
  
  decoded_url <- sapply( catalog$full_url , URLdecode )
  
  # create datavault links
  # catalog[ , "datavault_file" ] <- file.path( datavault_dir , basename( decoded_url ) )
  catalog[ , "datavault_file" ] <- file.path( datavault_dir , catalog[ , "topic" ] , catalog[ , "year" ] , basename( decoded_url ) )
  
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
  cat( "\npew datavault was built at" , datavault_dir , "\n" )
  
  # return catalog
  catalog
  
}

build_pew <-
  function( catalog , ... ){
    
    on.exit( print( catalog ) )
    
    tf <- tempfile()
    
    for ( i in seq_len( nrow( catalog ) ) ){
      
      # cachaca( catalog[ i , 'full_url' ] , tf , mode = 'wb' )
      # download the file
      if ( is.null( catalog[ i , "datavault_file" ] ) ) {
        download.file( catalog[ i , "full_url" ] , tf , quiet = TRUE , mode = "wb" )
      } else if ( is.na( catalog[ i , "datavault_file" ] ) ) {
        download.file( catalog[ i , "full_url" ] , tf , quiet = TRUE , mode = "wb" ) 
      } else {
        file.copy( catalog[ i , "datavault_file" ] , tf )
      }
      
      if( grepl( "\\.zip$" , catalog[ i , 'full_url' ] , ignore.case = TRUE ) ){
        
        unzipped_files <- unzip( tf , exdir = catalog[ i , "output_folder" ] , junkpaths = TRUE )
        
        macosx <- grep( "\\._" , unzipped_files , value = TRUE )
        
        file.remove( macosx )
        
        unzipped_files <- unzipped_files[ !( unzipped_files %in% macosx ) ]
        
        sav_files <- grep( "\\.sav$" , unzipped_files , ignore.case = TRUE , value = TRUE )
        
      } else {
        
        sav_files <- paste0( catalog[ i , "output_folder" ] , "/" , gsub( "%20" , " " , basename( catalog[ i , 'full_url' ] ) ) )
        # sav_files <- file.path( catalog[ i , "output_folder" ] , URLdecode( basename( catalog[ i , 'full_url' ] ) ) )
        
        if ( !dir.exists( dirname( sav_files ) ) ) { dir.create( dirname( sav_files ) ) }
        
        file.copy( tf , sav_files , overwrite = TRUE )
        
      }
      
      if( length( sav_files ) == 0 ){
        
        cat( paste0( "pew" , " catalog entry " , i , " of " , nrow( catalog ) , " unzipped in '" , catalog[ i , 'output_folder' ] , "' but zero spss files to import\r\n\n" ) )
        
      } else {
        
        for( this_sav in sav_files ){
          
          if( catalog[ i , 'full_url' ] == 'http://assets.pewresearch.org/wp-content/uploads/sites/2/2009/09/Pew-GAP-Fall-2009-BW-survey-for-website.zip' ){
            # x <- data.frame( haven::read_spss( this_sav , encoding = "WINDOWS-1250" ) )
            x <- tryCatch( data.frame( haven::read_spss( this_sav , encoding = "WINDOWS-1250" ) ), error = function(e) e )
            
            # x <- foreign::read.spss( this_sav , reencode = "WINDOWS-1250" , to.data.frame = TRUE )
            # x <- tryCatch( foreign::read.spss( this_sav , reencode = "WINDOWS-1250" , to.data.frame = TRUE ), error = function(e) e )
          } else {
            # x <- data.frame( haven::read_spss( this_sav ) )
            x <- tryCatch( data.frame( haven::read_spss( this_sav ) ), error = function(e) e )
            
            # x <- foreign::read.spss( this_sav , to.data.frame = TRUE )
            # x <- tryCatch( foreign::read.spss( this_sav , to.data.frame = TRUE ), error = function(e) e )
          }
          
          if(inherits( x , "error")) next()
          
          # convert all column names to lowercase
          names( x ) <- tolower( names( x ) )
          
          catalog[ i , 'case_count' ] <- max( catalog[ i , 'case_count' ] , nrow( x ) , na.rm = TRUE )
          
          saveRDS( x , file = gsub( "\\.sav$" , ".rds" , this_sav , ignore.case = TRUE ) , compress = FALSE )
          
        }
        
        cat( paste0( "pew" , " catalog entry " , i , " of " , nrow( catalog ) , " stored in '" , catalog[ i , 'output_folder' ] , "'\r\n\n" ) )
        
      }
      
      # delete the temporary files
      suppressWarnings( file.remove( tf ) )
      
    }
    
    on.exit()
    
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

remove_nonutf8_censo_escolar <-
  function( infile ){
    
    tf_a <- tempfile()
    
    outcon <- file( tf_a , "w" )
    
    incon <- file( infile , "r" )
    
    while( length( line <- readLines( incon , 1 , warn = FALSE ) ) > 0 ) writeLines( custom_strip_special( line ) , outcon )
    
    close( incon )
    
    close( outcon )
    
    tf_a
  }

read_excel_metadata <- function( metadata_file , table_type ) {
  
  quietly_sheets <- purrr::quietly(readxl::excel_sheets)
  quietly_readxl <- purrr::quietly(readxl::read_excel)
  
  sheet_name <- grep( table_type, custom_strip_special( quietly_sheets( metadata_file )$result ) , ignore.case = TRUE )
  
  codebook <-  data.frame( quietly_readxl( metadata_file , sheet = sheet_name , skip = 0 )$result , stringsAsFactors = FALSE )
  codebook <- codebook [ which( codebook[ , 1 ] %in% c("N" , "ORD" ) ): nrow(codebook) , ]
  colnames( codebook ) <- codebook[ 1 , ]
  codebook <- codebook[ -1 , ]
  codebook <- codebook[ !is.na( suppressWarnings( as.integer( codebook[ , 1 ] ) ) ), ]
  colnames( codebook ) <- custom_strip_special( colnames(codebook) )
  codebook <- tryCatch(
    codebook <- codebook[ !is.na( codebook[ , 2 ] ) , c( "Nome da Variavel" , "Tipo" , "Tam.(1)" ) ] ,
    error = function( e ) {
      
      possible.names <- list( c( "Nome da Variavel" , "Tipo" , "Tam. (1)" ) , c( "Nome novo da Variavel" , "Tipo" , "Tam. (1)" ), c( "Nome novo da Variavel" , "Tipo" , "Tam.(1)" ) , c( "NOME DA VARIAVEL" , "TIPO" , "TAMANHO" ) )
      
      for( name_set in possible.names ) {
        
        if ( all( name_set %in% colnames( codebook ) ) ) {
          
          codebook <- codebook[ !is.na( codebook[ , 2 ] ) , name_set ]
          colnames( codebook ) <- c( "Nome da Variavel" , "Tipo" , "Tam.(1)" )
          
          return( codebook )
          
        }
        
      }
      
    } )
  codebook$`Nome da Variavel` <- tolower( codebook$`Nome da Variavel` )
  codebook[ is.na( codebook$Tipo ) , "Tipo" ] <- "Char"
  codebook[ is.na( codebook$Tipo ) , "Tam.(1)" ] <- 4
  codebook[ grepl( "^co_curso_[1-9]" , codebook$`Nome da Variavel` ) , c( "Tipo" ) ] <- "Char"
  codebook[ grepl( "^co_curso_[1-9]" , codebook$`Nome da Variavel` ) , c( "Tam.(1)" ) ] <- 10
  codebook[ grepl( "^dt_" , codebook$`Nome da Variavel` ) , c( "Tipo" ) ] <- "Char"
  codebook[ grepl( "^dt_" , codebook$`Nome da Variavel` ) , c( "Tam.(1)" ) ] <- 10
  
  codebook
  
}


custom_extract <- function( zipfile , ext_dir , rm.main = FALSE ) {
  
  # td <- file.path( tempdir() , "temp_unzip" )
  td <- ifelse( .Platform$OS.type == 'windows' , file.path( tempdir() , "temp_unzip" ) , normalizePath( "~/temp_unzip" , mustWork = FALSE ) )
  
  archive::archive_extract( zipfile , dir = td )
  
  new_files <- list.files( td , recursive = TRUE , full.names = FALSE )
  
  for ( expath in unique( lodown:::np_dirname( paste0( ext_dir , "/" , new_files ) ) ) ) {
    if ( !dir.exists( expath ) ) dir.create( expath , showWarnings = FALSE , recursive = TRUE )
  }
  
  file.copy( from = file.path( td , new_files ) , to = file.path( ext_dir , new_files ) )
  
  unlink( td , recursive = TRUE , force = TRUE )
  
  if ( rm.main ) { file.remove( zipfile ) }
  
  return( normalizePath( ( file.path( ext_dir , new_files ) ) , mustWork = FALSE ) )
  
}

custom_strip_special <- function(x) {
  y <- iconv( x , from = ifelse( .Platform$OS.type == 'windows' , "utf8" , "" ) , to = "ASCII//TRANSLIT" , sub = " " )
  y <- gsub( "'|~" , "" , y  )
  y
}