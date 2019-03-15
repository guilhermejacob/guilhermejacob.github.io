downloader::source_url( "https://raw.githubusercontent.com/guilhermejacob/guilhermejacob.github.io/master/scripts/install_packages.R" , quiet = TRUE , prompt = TRUE )

catalog_lapop <- 
  function( output_dir ) { 
    
    # reset handle
    httr::handle_reset( "http://datasets.americasbarometer.org" )
    
    # request a valid cookie from the server and then don't touch it
    response <- httr::GET(
      url = "http://datasets.americasbarometer.org/database/index.php?freeUser=true", 
      httr::add_headers(
        `Accept` = 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8',
        `Accept-Encoding` = 'gzip, deflate',
        `Accept-Language` = 'pt-BR,pt;q=0.9,en-US;q=0.8,en;q=0.7,fr;q=0.6,it;q=0.5',
        `Cache-Control` = 'max-age=0',
        `Connection` = 'keep-alive',
        `Host` = 'datasets.americasbarometer.org',
        `Upgrade-Insecure-Requests` = 1,
        `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/65.0.3325.162 Safari/537.36'
      ))
    
    set_cookie <- httr::headers(response)$`set-cookie`
    cookies <- strsplit(set_cookie, ';')
    my_cookie <- cookies[[1]][1]
    
    
    response <- 
      httr::POST( url = "http://datasets.americasbarometer.org/database/getdata.php",
                  httr::config(followlocation = TRUE),
                  body = list( `search` = '*' ),
                  encode = "form" ,
                  httr::add_headers( 
                    `Accept` = '*/*',
                    `Accept-Encoding` = 'gzip, deflate',
                    `Accept-Language` = 'pt-BR,pt;q=0.9,en-US;q=0.8,en;q=0.7,fr;q=0.6,it;q=0.5',
                    `Connection` = 'keep-alive',
                    `Content-Length` = 8,
                    `Content-Type` = 'application/x-www-form-urlencoded; charset=UTF-8',
                    `Cookie` = my_cookie ,
                    `Host` = 'datasets.americasbarometer.org',
                    `Origin` = 'http://datasets.americasbarometer.org',
                    `Referer` = 'http://datasets.americasbarometer.org/database/index.php?freeUser=true',
                    `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/65.0.3325.162 Safari/537.36',
                    `X-Requested-With` = 'XMLHttpRequest' ) )
    
    html_file <- xml2::read_html( response )
    these_links <- rvest::html_attr( rvest::html_nodes( html_file , "a" ) , "href" )
    these_texts <- rvest::html_text( rvest::html_nodes( html_file , "a" ) )
    these_links <- gsub( "^\\./" , "" , these_links )
    
    # these_links <- these_links[ grepl( "\\.(sav|spss)$" , these_links , ignore.case = TRUE ) ]
    these_texts <- these_texts[ grepl( "(dta|pdf)$" , these_links , ignore.case = TRUE ) & grepl( "\\..*" , these_links , ignore.case = TRUE ) ]
    these_links <- these_links[ grepl( "(dta|pdf)$" , these_links , ignore.case = TRUE ) & grepl( "\\..*" , these_links , ignore.case = TRUE ) ]
    
    these_links <- these_links[ nchar(these_texts) > 0 ]
    these_texts <- these_texts[ nchar(these_texts) > 0 ]
    
    these_links <- paste0( 'http://datasets.americasbarometer.org/database/' , these_links )
    
    catalog <- 
      data.frame(
        full_url = these_links , 
        item_title = these_texts ,
        output_file = gsub( "\\.(dta|zip)$" , ".Rds" , gsub( 'http://datasets.americasbarometer.org/database/files' , output_dir , these_links ) ) ,
        stringsAsFactors = FALSE
      )
    
    catalog
  }

build_lapop <- function( catalog ) {
  
  tf <- tempfile()
  catalog[ , "size" ] <- NA
  
  for ( i in seq_along( catalog$full_url ) ) {
    
    this_link <- catalog[ i , "full_url" ]
    
    this_file <- catalog[ i , "output_file" ]
    
    # if ( file.exists( this_file ) ) { cat( "lapop entry" , i , "of" , nrow( catalog ) , "stored at", dirname( this_file ) , "\n" ) ; next() }
    
    if ( grepl( "\\.(dta|zip)$" , this_link , ignore.case = TRUE ) ) {
      
      download.file( this_link , tf , quiet = TRUE , method = "wget" )
      
      x <- haven::read_dta( tf , encoding = "latin1" )
      x <- haven::as_factor( x , only_labelled = TRUE )
      # x <- sjlabelled::zap_labels( x )
      x <- as.data.frame( x )
      
      if ( !dir.exists( dirname( this_file ) ) ) { dir.create( dirname( this_file ) , recursive = TRUE ) }
      
      saveRDS( x , this_file )
      
      catalog[ i , "size" ] <- nrow(x)
      
    } else {
      if ( !dir.exists( dirname( this_file ) ) ) { dir.create( dirname( this_file ) , recursive = TRUE ) }
      download.file( this_link , this_file , quiet = TRUE , method = "wget" )
    }
    
    cat( "lapop entry" , i , "of" , nrow( catalog ) , "stored at", paste0( "'", dirname( this_file ) , "/'" ), "\r" )
    
  }
  
  catalog
  
}
