# downloader::source_url( "https://raw.githubusercontent.com/guilhermejacob/guilhermejacob.github.io/master/scripts/install_packages.R" , quiet = TRUE , prompt = FALSE )

table_num_pad <- function( title, y ) paste0( title , stringr::str_pad( y , width = 2 , pad = "0" , side = "left" ) )

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

# scrape dir
link_scrape <- function( x ) { 
  custom_recodes <- c( "í" = "%ED" , "á" = "%E1" , "ã" = "%E3" , " " = "%20" )
  custom_recodes <- NULL
  if ( !is.null( x ) ){
    this_text <- RCurl::getURL( x , .encoding = 'ISO-8859-1' , .opts = list(timeout = 10 , dirlistonly = TRUE , ftplistonly = TRUE , ftp.use.epsv = FALSE ) )
    this_text <- unlist( strsplit( this_text , "\n" ) )
    this_text <- RCurl::curlPercentEncode( this_text , codes = custom_recodes )
    paste0( x , this_text )
  } else { 
    NULL 
  }
}

getlisting <- function( these_urls ) {
  these_urls <- ifelse( grepl( "\\/$" , these_urls ) , these_urls , paste0( these_urls , "/" ) )
  res <- lapply( these_urls , link_scrape )
  res <- unlist( res )
  is.file <- grepl( "\\." , basename( res ) , ignore.case = TRUE )
  res[ !is.file ] <- ifelse( grepl( "\\/$" , res[ !is.file ] ) , res[ !is.file ] , paste0( res[ !is.file ] , "/" ) )
  list( dirs = if ( any(!is.file) ) { res[ !is.file ] } else { NULL } , 
        files = if ( any(is.file) ) { res[ is.file ] } else { NULL } )
}

recursive_ftp_scrape <- function( main_url , max.iter = Inf ) {
  
  final_files <- NULL
  
  directories <- main_url
  
  i=0
  while ( length(directories) > 0 ) {
    i=i+1
    if ( i > max.iter ) { break() }
    listing <- getlisting(directories)
    directories <- listing[[1]]
    files <- listing[[2]]
    final_files <- c(final_files, files )
    # if ( length( directories) > 0 ) cat( length(directories) , "new directories found.\n" ) else cat( "done!\n" )
  }
  
  return( final_files )
  
}

remove_special_character <- function(x) {
  stripped_x <- iconv( x , to = "ascii//translit" , sub = "_" )
  stripped_x <- gsub( "('|~|\\^)" , "" , stripped_x , ignore.case = TRUE )
  stripped_x
}


catalog_ascenso <-
  function( output_dir , ... ){
    
    output_dir <- ifelse( !grepl( "/$" , output_dir ) , paste0( output_dir , "/" ) , output_dir )
    
    # 2000 and 2010
    url_paths <- c("ftp://ftp.ibge.gov.br/Censos/Censo_Demografico_2000/Dados_do_Universo/Agregado_por_Setores_Censitarios/" , "ftp://ftp.ibge.gov.br/Censos/Censo_Demografico_2010/Resultados_do_Universo/Agregados_por_Setores_Censitarios/" )
    
    file_list <- recursive_ftp_scrape( url_paths )
    
    # keep zip files only
    file_list <- file_list[ grepl( "\\.zip$" , basename( file_list ) , ignore.case = TRUE ) ]
    
    # drop documentation file
    file_list <- file_list[ !grepl( "document" , basename( file_list ) , ignore.case = TRUE ) ]
    
    # extract years
    these_years <- as.numeric( gsub( ".*Censo_Demografico_|\\/.*" , "" , file_list ) )
    
    # build catalog
    catalog1 <-
      data.frame(
        full_url = file_list ,
        year = these_years ,
        # area_code = ifelse( these_years == 2010 , gsub( "_2017.*|_[0-9]{4}.*" , "" , basename( file_list ) ) , gsub( ".*_2000_|\\..*" , "" , basename( file_list ) ) ),
        subtype = "setores" ,
        output_folder = paste0( output_dir , "Censo " , these_years , "/" ) ,
        stringsAsFactors = FALSE
      )
    
    # 2007
    url_path <- "ftp://ftp.ibge.gov.br/Contagem_da_Populacao_2007/"
    
    file_list <- recursive_ftp_scrape( url_path )
    
    # keep zip files only
    file_list <- file_list[ grepl( "\\.zip$" , basename( file_list ) , ignore.case = TRUE ) ]
    
    # drop documentation file
    file_list <- file_list[ !grepl( "document" , basename( file_list ) , ignore.case = TRUE ) ]
    
    # drop non-standard files
    file_list <- file_list[ !( dirname( file_list ) %in% "ftp://ftp.ibge.gov.br/Contagem_da_Populacao_2007" ) ]
    
    # get basefolder
    these_subtypes <- gsub( ".*Contagem_da_Populacao_2007/" , "" , dirname( file_list ) )
    contagem_types <- c( "setores" , "bairros" , "distritos" )
    these_cols <- lapply( contagem_types , function( y ) grepl( y , these_subtypes , ignore.case = TRUE ) )
    these_cols <- do.call( cbind , these_cols )
    these_subtypes <- apply( these_cols , 1 , function(y) contagem_types[ y ] )
    
    # build catalog
    catalog2 <-
      data.frame(
        full_url = file_list ,
        year = 2007 ,
        subtype = these_subtypes ,
        output_folder = paste0( output_dir , "Contagem 2007/" , these_subtypes , "/" ) ,
        stringsAsFactors = FALSE
      )
    
    # stack data
    catalog <- do.call( rbind , list( catalog1 , catalog2 ) )
    
    # define database file
    # catalog[ , "dbfile" ] <- paste0( output_dir , "ascenso.sqlite" )
    
    # drop 2007
    catalog <- subset( catalog , year != 2007 )
    
    # return full catalog
    catalog
    
  }

datavault_ascenso <- function( catalog , datavault_dir , skipExist = TRUE ) {
  
  # check datavault main path
  datavault_dir <- ifelse( !grepl( "/$" , datavault_dir ) , paste0( datavault_dir , "/" ) , datavault_dir )
  
  # create datavault file paths
  catalog[ , "datavault_file" ] <- 
    paste0( datavault_dir , 
            catalog[ , "year" ] , "/" , 
            ifelse( catalog$year == 2007 , 
                    paste0( catalog[ , "subtype" ], "/" , basename( catalog[ , "full_url" ] ) ) , 
                    basename( catalog[ , "full_url" ] ) ) )
  
  # check for existing files
  existing_files <- file.exists( catalog[ , "datavault_file" ] )
  
  # parallel
  plyr::llply( seq_along( catalog$full_url ) , function( i ) {
    
    # skip existing file
    if( skipExist & existing_files[ i ] ){ cat( "file" , i , "out of" , nrow( catalog ) , "downloaded to" , datavault_dir , "\r") ; return(NULL) }
    
    
    # download file
    if ( !dir.exists( dirname( catalog[ i , "datavault_file" ] ) ) ) { dir.create( dirname( catalog[ i , "datavault_file" ] ) , recursive = TRUE ) }
    download.file( catalog[ i , "full_url" ], catalog[ i , "datavault_file" ] , mode = "wb" , quiet = TRUE )
    
    # process tracker
    cat( "file" , i , "out of" , nrow( catalog ) , "downloaded to" , datavault_dir , "\r")
    
  } , .parallel = TRUE )
  
  
  # print message
  cat( "\nascenso datavault was built at" , datavault_dir , "\n" )
  
  # return catalog
  catalog
  
}

build_ascenso <-
  function( catalog , ... ){
    
    # load data.table
    library(data.table)
    
    tf <- tempfile()
    td <- file.path( tempdir() , "unzips" )
    
    # create full results list
    full_results <- NULL
    
    for ( i in seq_len( nrow( catalog ) ) ){
      
      # download the file
      if ( is.null( catalog[ i , "datavault_file" ] ) ) {
        download.file( catalog[ i , "full_url" ] , tf , quiet = TRUE , mode = "wb" )
      } else if ( is.na( catalog[ i , "datavault_file" ] ) ) {
        download.file( catalog[ i , "full_url" ] , tf , quiet = TRUE , mode = "wb" ) 
      } else {
        file.copy( catalog[ i , "datavault_file" ] , tf , overwrite = TRUE )
      }
      
      # extract files
      unzip( tf , exdir = td )
      
      # list files
      these_data_files <- list.files( td , recursive = TRUE , full.names = FALSE )
      
      # fix names
      these_data_files_fixed <- sapply( these_data_files , URLdecode , USE.NAMES = FALSE )
      these_data_files_fixed <- iconv( these_data_files_fixed , from = "CP850" )
      for ( this_dir in unique( file.path( td , dirname( these_data_files_fixed ) ) ) ) { dir.create( this_dir , recursive = TRUE , showWarnings = FALSE ) }
      file.rename( file.path( td , these_data_files ) , file.path( td , these_data_files_fixed ) )
      these_data_files <- these_data_files_fixed
      
      # create full paths
      these_data_files <- file.path( td , these_data_files )
      
      # keep xls only
      these_data_files <- these_data_files[ grepl( "\\.xls$" , basename( these_data_files ) , ignore.case = TRUE) ]
      
      # drop documentation
      these_data_files <- these_data_files[ !grepl( "descr|compat" , basename( these_data_files ) , ignore.case = TRUE) ]
      
      if ( catalog[ i , "year" ] == 2000 ) {
        
        # process data
        these_results <- lapply( these_data_files , function( this_file ) {
          
          # read data
          x <- readxl::read_xls( this_file )
          
          # force data.table
          x <- data.table::data.table( x , stringsAsFactors = FALSE )
          
          # fix names
          colnames( x ) <- tolower( colnames( x ) )
          colnames( x ) <- remove_special_character( colnames( x ) )
          
          # set up tablename
          this_tablename <- tolower( gsub( "_.*" , "" , basename( this_file ) ) )
          this_tablename <- paste( this_tablename , catalog [ i , "year" ] , sep = "_" )
          
          # read into database
          # DBI::dbWriteTable( db , this_tablename , x , append = TRUE )
          
          # count cases
          case_count <- nrow(x)
          
          # define output file
          this_output_file <- paste0( catalog[ i, "output_folder" ] , gsub( "\\..*" , "" , tolower( basename( this_file ) ) ) , "_" , catalog[ i, "year" ] , ".fst" )
          
          # save data
          if ( !dir.exists( dirname( this_output_file ) ) ) { dir.create( dirname( this_output_file ) , recursive = TRUE ) }
          fst::write_fst( x , this_output_file , compress = 100 )
          
          # remove data.frame
          rm( x )
          
          # create results data.frame
          data.frame( year = catalog[ i , "year" ] , 
                      full_url = catalog[ i , "full_url" ] , 
                      fst_file = this_output_file , 
                      case_count = case_count , 
                      stringsAsFactors = FALSE)
          
        } )
        
        # stack these_results
        these_results <- do.call( rbind , these_results )
        
        # stack results
        full_results <- rbind( full_results , these_results )
        
        
      } else if ( catalog[ i , "year" ] == 2010 ) {
        
        # process data
        these_results <- lapply( these_data_files , function( this_file ) {
          
          # read data
          x <- readxl::read_xls( this_file )
          
          # force data.table
          x <- data.table::data.table( x , stringsAsFactors = FALSE )
          
          # fix names
          colnames( x ) <- tolower( colnames( x ) )
          colnames( x ) <- remove_special_character( colnames( x ) )
          
          # fix wrong numeric
          these_cols <- grep( "^cod" , colnames( x ) , value = TRUE )
          x[ , (these_cols) := lapply( .SD , as.character ) , .SDcols = these_cols ]
          
          # set up tablename
          this_tablename <- tolower( gsub( "_.*" , "" , basename( this_file ) ) )
          this_tablename <- paste( this_tablename , catalog [ i , "year" ] , sep = "_" )
          
          # read into database
          # DBI::dbWriteTable( db , this_tablename , x , append = TRUE )
          
          # count cases
          case_count <- nrow(x)
          
          # define output file
          this_output_file <- paste0( catalog[ i, "output_folder" ] , gsub( "\\..*" , "" , tolower( basename( this_file ) ) ) , "_" , catalog[ i, "year" ] , ".fst" )
          
          # save data
          if ( !dir.exists( dirname( this_output_file ) ) ) { dir.create( dirname( this_output_file ) , recursive = TRUE ) }
          fst::write_fst( x , this_output_file , compress = 100 )
          
          # remove data.frame
          rm( x )
          
          # create results data.frame
          data.frame( year = catalog[ i , "year" ] , 
                      full_url = catalog[ i , "full_url" ] , 
                      fst_file = this_output_file , 
                      case_count = case_count , 
                      stringsAsFactors = FALSE)
          
        } )
        
        # stack these_results
        these_results <- do.call( rbind , these_results )
        
        # stack results
        full_results <- rbind( full_results , these_results )
        
      } else if ( catalog[ i , "year" ] == 2007 ) {
        
        # process data
        these_results <- lapply( these_data_files , function( this_file ) {
          
          # read data
          x <- readxl::read_xls( this_file )
          
          # skip empty files
          if ( nrow( x ) <= 0 ) { next() }
          
          # force data.table
          x <- data.table::data.table( x , stringsAsFactors = FALSE )
          
          # fix names
          colnames( x ) <- tolower( colnames( x ) )
          colnames( x ) <- remove_special_character( colnames( x ) )
          
          # fix wrong numeric
          these_cols <- grep( "^cod" , colnames( x ) , value = TRUE )
          x[ , (these_cols) := lapply( .SD , as.character ) , .SDcols = these_cols ]
          
          # set up tablename
          this_tablename <- tolower( gsub( "_.*" , "" , basename( this_file ) ) )
          this_tablename <- paste( this_tablename , catalog [ i , "year" ] , sep = "_" )
          
          # read into database
          # DBI::dbWriteTable( db , this_tablename , x , append = TRUE )
          
          # count cases
          case_count <- c( case_count , nrow(x) )
          
          # define output file
          this_output_file <- paste0( catalog[ i, "output_folder" ] , gsub( "\\..*" , "" , tolower( basename( this_file ) ) ) , "_" , catalog[ i, "year" ] , ".fst" )
          
          # save data
          if ( !dir.exists( dirname( this_output_file ) ) ) { dir.create( dirname( this_output_file ) , recursive = TRUE ) }
          fst::write_fst( x , this_output_file , compress = 100 )
          
          # remove data.frame
          rm( x )
          
          # create results data.frame
          data.frame( year = catalog[ i , "year" ] , 
                      full_url = catalog[ i , "full_url" ] , 
                      fst_file = this_output_file , 
                      case_count = case_count , 
                      stringsAsFactors = FALSE)
          
        } )
        
        # stack results
        full_results <- rbind( full_results , these_results )
        
      }
      
      # store max case count
      catalog[ i , "case_count" ] <- max( these_results$case_count )
      
      # delete temporary folder and files
      unlink( td , recursive = TRUE )
      file.remove( tf )
      
      # process tracker
      cat( paste0( "ascenso catalog entry " , i , " of " , nrow( catalog ) , " stored at '" , catalog[ i , "output_folder" ] , "'\r" ) )
      
    }
    
    catalog
    
  }

