downloader::source_url( "https://raw.githubusercontent.com/guilhermejacob/guilhermejacob.github.io/master/scripts/install_packages.R" , quiet = TRUE , prompt = TRUE )

# catalog
catalog_issp <-
  function( output_dir ){
    
    issp_portal <- "http://www.issp.org/data-download/by-year/"
    
    issp_html <- xml2::read_html(issp_portal)
    w <- rvest::html_nodes( issp_html , 'a' )
    w <- w[ grepl( "Data Download [1-9]" , w , ignore.case = TRUE ) ]
    
    links <- rvest::html_attr( w , "href" )
    full_text <- rvest::html_text( w )
    themes <- gsub( "[0-9]{4} " , "" , full_text )
    years <- gsub( " .*" , "" , full_text )
    goi <- gsub( ".*no=|\\&.*" , "" , links )
    
    catalog <-
      data.frame(
        full_url = links ,
        year = as.numeric( years ) ,
        theme = themes ,
        # dbfolder = paste0( output_dir , "/MonetDB" ) ,
        output_file = file.path( output_dir , paste0( years , " ", themes , ".Rds" ) ) ,
        goi = goi ,
        stringsAsFactors = FALSE
      )
    
    # sort by year
    catalog[ order( catalog$year ) , ]
    
  }

# datavault
datavault_issp <- function( catalog , datavault_dir , skipExist = TRUE , username , password , purpose = 6 ) {
  
  s <- gesis::login( username = username , password = password )
  
  destdirs <- file.path( datavault_dir , paste( catalog$year , catalog$theme ) )
  
  for ( i in seq_len( nrow(catalog) ) ) {
    
    if ( !dir.exists( destdirs[ i ] ) ) dir.create( destdirs[ i ] , recursive = TRUE )
    
    full_df <- extract_docs_df( catalog[ i , "full_url" ] , catalog[ i , "year" ] , destdir = destdirs[ i ] )
    if (skipExist) full_df <- full_df[ !file.exists( full_df[ , "destfile" ] ) , ]
    
    for ( j in seq_len( nrow(full_df) ) ) {
      download.file( full_df[ j , "link_src" ] , full_df[ j , "destfile" ] , mode = "wb" , quiet = TRUE )
      # cat( j , "out of" , nrow(full_df) , "downloaded\r" )
    }
    
    
    if (skipExist) { 
      if ( length( list.files( destdirs[ i ] , pattern = "\\.dta" ) ) ) { 
        catalog[ i , "datavault_file" ] <- list.files( destdirs[ i ] , pattern = "\\.dta" , full.names = TRUE )
        next() 
      } 
    }
    
    gesis::download_dataset( s , doi = catalog[ i , "goi" ] , path = destdirs[ i ] , purpose = purpose , quiet = TRUE )
    
    catalog[ i , "datavault_file" ] <- list.files( destdirs[ i ] , pattern = "\\.dta" , full.names = TRUE )
    
    cat( "item" , i , "out of" , nrow( catalog ) , "downloaded to" , datavault_dir , "\r")
    
  }
  
  cat( "\nissp datavault was built at" , datavault_dir , "\n" )
  
  catalog
  
}

# build issp
build_issp <- function( catalog , output_dir , username , password , purpose = 6 ) {
  
  # tf <- tempfile()
  td <- tempdir()
  
  s <- gesis::login( username = username , password = password )
  
  for ( i in seq_len( nrow(catalog) ) ) { 
    
    this_data_file <- NULL
    unlink( file.path( td , "unzipped" ) , recursive = TRUE )
    
    # download the file
    if ( is.null( catalog[ i , "datavault_file" ] ) ) {
      if ( !dir.exists( file.path( td , "unzipped" ) ) ) dir.create( file.path( td , "unzipped" ) )
      gesis::download_dataset( s , doi = catalog[ i , "goi" ] , path = file.path( td , "unzipped" ) , purpose = purpose , quiet = TRUE )
      this_data_file <- list.files( file.path( td , "unzipped" ) , full.names = TRUE )
    } else if ( is.na( catalog[ i , "datavault_file" ] ) ) {
      if ( !dir.exists( file.path( td , "unzipped" ) ) ) dir.create( file.path( td , "unzipped" ) )
      gesis::download_dataset( s , doi = catalog[ i , "goi" ] , path = file.path( td , "unzipped" ) , purpose = purpose , quiet = TRUE )
      this_data_file <- list.files( file.path( td , "unzipped" ) , full.names = TRUE )
    } else {
      if ( !dir.exists( file.path( td , "unzipped" ) ) ) dir.create( file.path( td , "unzipped" ) )
      this_data_file <- catalog[ i , "datavault_file" ]
    }
    
    if ( grepl( "\\.zip$" , this_data_file ) ) { unzip( this_data_file , exdir = file.path( td , "unzipped" ) ) ; this_data_file <- list.files( file.path( td , "unzipped" ) , full.names = TRUE , pattern = "\\.dta$" ) }
    
    x <- data.frame( haven::read_dta( this_data_file , encoding = NULL ) , stringsAsFactors = FALSE )
    colnames(x) <- tolower( colnames(x) )
    
    x <- haven::as_factor( x , only_labelled = TRUE )
    
    if ( !dir.exists( dirname( catalog[ i , "output_file" ] ) ) ) { dir.create( dirname( catalog[ i , "output_file" ] ) , recursive = TRUE ) }
    
    saveRDS( x , catalog[ i , "output_file" ] )
    
    cat( paste0( "issp" , " catalog entry " , i , " of " , nrow( catalog ) , " stored in '" , output_dir , "'\r\n\n" ) )
    
  }
  
  catalog
  
}




# auxilliary functions
# extract data file from Gesis page
extract_docs_df <- function( gesis_page , year , destdir ) {
  
  main_url <- ifelse(year >= 2015 , "https://dbk.gesis.org/DBKsearch/" , "https://dbk.gesis.org/dbksearch/")
  
  this_html <- xml2::read_html(gesis_page)
  full_text <- rvest::html_text( rvest::html_nodes( this_html , 'a' ) )
  these_links <- rvest::html_attr( rvest::html_nodes( this_html , 'a' ) , "href" )
  
  doc_links <- paste0( main_url , these_links[ grepl( "\\.pdf$" , full_text ) ] )
  doc_names <- file.path( destdir , full_text[ grepl( "\\.pdf$" , full_text ) ] )
  
  data.frame( destfile = doc_names , link_src = doc_links , stringsAsFactors = FALSE )
  
}

extract_data_df <- function( gesis_page , year , destdir ) {
  
  main_url <- ifelse(year >= 2015 , "https://dbk.gesis.org/DBKsearch/" , "https://dbk.gesis.org/dbksearch/")
  
  this_html <- xml2::read_html(gesis_page)
  full_text <- rvest::html_text( rvest::html_nodes( this_html , 'a' ) )
  these_links <- rvest::html_attr( rvest::html_nodes( this_html , 'a' ) , "href" )
  
  # subset for data files
  these_links <- these_links[ grepl( "\\.(dta|sav|por)" , full_text , ignore.case = TRUE ) ]
  full_text <- full_text[ grepl( "\\.(dta|sav|por)" , full_text , ignore.case = TRUE ) ]
  
  # # subset for `datatype`
  # these_links <- these_links[ grepl( datatype , full_text , ignore.case = TRUE ) ]
  # full_text <- full_text[ grepl( datatype , full_text , ignore.case = TRUE ) ]
  
  # fix links and dest files
  these_links <- paste0( main_url , these_links )
  full_text <- file.path( destdir , full_text )
  
  data.frame( destfile = full_text , link_src = these_links , stringsAsFactors = FALSE )
  
}