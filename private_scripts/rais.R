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
  codes <- c( "í" = "%ED" )
  these_urls <- ifelse( grepl( "\\/$" , these_urls ) , these_urls , paste0( these_urls , "/" ) )
  # res <- lapply( these_urls , function( x ) { if ( !is.null( x ) ) paste0( x , RCurl::curlPercentEncode( unlist( strsplit( RCurl::getURL( x, dirlistonly = TRUE ) , "\n" ) ) , codes = codes ) ) else NULL } )
  res <- plyr::llply( these_urls , function( x ) { if ( !is.null( x ) ) paste0( x , RCurl::curlPercentEncode( unlist( strsplit( RCurl::getURL( x, dirlistonly = TRUE ) , "\n" ) ) , codes = codes ) ) else NULL } , .parallel = TRUE )
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
    if ( length( directories) > 0 ) cat( length(directories) , "new directories found.\n" ) else cat( "done!\n" )
  }
  
  return( final_files )
  
}


remove_special_character <- function(x) {
  stripped_x <- iconv( x , to = "ascii//translit" , sub = "_" )
  stripped_x <- gsub( "('|~|\\^)" , "" , stripped_x , ignore.case = TRUE )
  stripped_x
}



catalog_rais <-
  function( output_dir , ... ){
    
    output_dir <- ifelse( !grepl( "/$" , output_dir ) , paste0( output_dir , "/" ) , output_dir )
    
    url_path <- "ftp://ftp.mtps.gov.br/pdet/microdados/RAIS/"
    
    mtps_files <- suppressWarnings( recursive_ftp_scrape( url_path ) )
    
    catalog <-
      data.frame(
        full_url = mtps_files ,
        stringsAsFactors = FALSE
      )
    
    catalog$type[ grepl( "\\.7z$" , basename( catalog$full_url ) ) ] <- ifelse( grepl( "ESTB|estabelecimento" , basename( catalog$full_url ) , ignore.case = TRUE ) , "estb" , "vinc" )[ grepl( "\\.7z$" , basename( catalog$full_url ) ) ]
    
    catalog$uf[ catalog$type == "vinc" & !is.na( catalog$type ) ] <- 
      ifelse( grepl( "^IGN" , basename( catalog$full_url ) , ignore.case = TRUE ) , 
              "ignorado" , 
              gsub( "[0-9].*" , "" , basename( catalog$full_url ) ) ) [ catalog$type == "vinc" & !is.na( catalog$type ) ]
    
    catalog$output_filename <- gsub( "7z$|zip$" , "fst" ,
                                     gsub( url_path , paste0( output_dir , "rais/" ) , tolower( catalog$full_url ) , ignore.case = TRUE ) ,
                                     ignore.case = TRUE )
    catalog$output_filename <- sapply( catalog$output_filename , utils::URLdecode )
    catalog$output_filename <- gsub( "\\%ed", "i" , catalog$output_filename )
    catalog$output_filename <- gsub( "ignorando", "ignorado" , catalog$output_filename , ignore.case = TRUE )
    
    catalog$year <- NULL
    catalog$year[ grepl( "\\.7z$" , basename( catalog$full_url ) ) ] <- gsub( "^(ESTB|IGNORADOS|IGNORADO|IGNORANDO|IGNORANDOS|[A-Z]{2})|\\..*" , "" , grep( "\\.7z$" , basename( catalog$full_url ) , value = TRUE ) , ignore.case = TRUE ) 
    catalog$year <- as.numeric( catalog$year )
    
    # # insert layout
    # catalog$layout[ catalog$type == "estb" & !is.na(catalog$type) ] <- 
    #   ifelse( catalog$year %in% 1985:1989 , "1985_1989" ,
    #           ifelse( catalog$year %in% 1991:1992 , "1991_1992" ,
    #                   ifelse( catalog$year %in% 1999:2000 , "1999_2000" , 
    #                           ifelse( catalog$year %in% 2002:2013 , "2002_2013" , 
    #                                   ifelse( catalog$year %in% 2014:2016 , "2014_2016" , 
    #                                           catalog$year ) ) ) ) )[ catalog$type == "estb" & !is.na(catalog$type) ]
    # catalog$layout[ catalog$type == "vinc" & !is.na(catalog$type) ] <- 
    #   ifelse( catalog$year %in% 1985:1993 , "1985_1993" ,
    #           ifelse( catalog$year %in% 2015 , "2015" , 
    #                   ifelse( catalog$year %in% 2016 , "2016" , "geral" ) ) )[ catalog$type == "vinc" & !is.na(catalog$type) ]
    # catalog$layout[ !grepl( "\\.7z$" , basename( catalog$full_url ) ) ] <- NA
    
    # all files in one table per type
    catalog$db_tablename <- ifelse( grepl( "\\.7z$" , basename( catalog$full_url ) ) , paste0( "rais_" , catalog$type , "_" , catalog$year ) , NA )
    
    # database file
    # catalog$dbfolder <- ifelse( is.na( catalog$db_tablename ) , NA , paste0( output_dir , "/MonetDB" ) )
    catalog$dbfile <- ifelse( is.na( catalog$db_tablename ) , NA , paste0( output_dir , "/mtps.sqlite" ) )
    catalog$dbfile <- gsub( "//" , "/" , catalog$dbfile )
    
    # raisEST_062012.7z tem um erro: ";" sobrando.
    # catalog <- catalog[ catalog$year >= 2013 | is.na( catalog$year ), ]
    
    catalog
    
  }

# build datavault
datavault_rais <- function( catalog , datavault_dir , skipExist = TRUE ) {
  
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
  cat( "\nrais datavault was built at" , datavault_dir , "\n" )
  
  # return catalog
  catalog
  
}

build_rais <-
  function( catalog , ... ){
    
    tf <- tempfile()
    td <- tempdir()
    
    # create directory structure
    # for ( i in seq_len( nrow( catalog ) ) )  if ( !dir.exists( dirname( catalog[ i , "dbfile" ] ) ) ) { dir.create( dirname( catalog[ i , "dbfile" ] ) , recursive = TRUE ) }
    
    for ( i in seq_len( nrow( catalog ) ) ){
      
      # download the file
      if ( is.null( catalog[ i , "datavault_file" ] ) ) {
        download.file( catalog[ i , "full_url" ] , tf , quiet = TRUE , mode = "wb" )
      } else if ( is.na( catalog[ i , "datavault_file" ] ) ) {
        download.file( catalog[ i , "full_url" ] , tf , quiet = TRUE , mode = "wb" ) 
      } else {
        file.copy( catalog[ i , "datavault_file" ] , tf )
      }
      
      if ( !grepl( "\\.(7z|zip)$" , catalog[ i , 'full_url' ] , ignore.case = TRUE ) ) {
        
        if ( !dir.exists( dirname( catalog[ i , 'output_filename' ] ) ) ) { dir.create( dirname( catalog[ i , 'output_filename' ] ) , recursive = TRUE ) }
        file.copy( tf , catalog[ i , 'output_filename' ] )
        file.remove( tf )
        
      } else {
        
        archive::archive_extract( normalizePath( tf ) , dir = file.path( td , "unzipped" ) )
        
        this_data_file <- list.files( file.path( td , "unzipped" ) , full.names = TRUE , recursive = TRUE )
        
        this_data_file <- grep( "\\.csv|\\.txt$", this_data_file, value = TRUE, ignore.case = TRUE )
        
        x <- tryCatch( suppressWarnings( data.table::fread( this_data_file , sep = ";" , encoding = "Latin-1" , dec = "," , na.strings = "-1" , showProgress = FALSE , data.table = TRUE , stringsAsFactors = FALSE , fill = TRUE , strip.white = TRUE ) ) , 
                       error = function(e) utils::read.csv( this_data_file , sep = ";" , dec = "," , header = TRUE , fileEncoding = "latin1" , as.is = TRUE ) )
        
        suppressWarnings( unlink( file.path( td , "unzipped" ) , recursive = TRUE ) )
        file.remove( tf )
        
        # convert all column names to lowercase
        names( x ) <- tolower( names( x ) )
        
        # remove special characters
        names( x ) <- remove_special_character( names( x ) )
        
        # remove trailing spaces
        names( x ) <- trimws( names( x ) , which = "both" )
        
        # change dots for underscore
        names( x ) <- gsub( "\\.$" , "" , names( x ) )
        names( x ) <- gsub( "\\(|\\)" , "" , names( x ) )
        names( x ) <- gsub( "\\.|\\.\\.|\\/| |\\-" , "_" , names( x ) )
        
        # fix NAs
        # x[ , ] <- apply( x[ , ] , 2 , function( this_vec ) { this_vec [ this_vec == -1 ] <- NA ; return( this_vec ) } )
        # x[ , colnames( x ) := lapply(.SD,function(y) {y [ y == -1 ] <- NA ; return( y )} ), .SDcols=colnames( x ) ]
        
        if ( !dir.exists( dirname( catalog[ i , 'output_filename' ] ) ) ) { dir.create( dirname( catalog[ i , 'output_filename' ] ) , recursive = TRUE ) }
        # saveRDS( x, file = catalog[ i , "output_filename" ] , compress = TRUE )
        fst::write_fst( x , path = catalog[ i , "output_filename" ] , compress = 70 )
        # DBI::dbWriteTable( db , catalog[ i , "db_tablename" ] , x )
        catalog[ i , "count" ] <- nrow( x )
        rm(x) ; gc()
        
        # process tracker
        cat( paste0( "mtps catalog entry " , i , " of " , nrow( catalog ) , " stored at '" , catalog[ i , 'output_filename' ] , "'\r" ) )
        
        # if this is the final catalog entry for the unique db_tablename, then write them all to the database
        if ( i == max( which( catalog$db_tablename == catalog[ i , 'db_tablename' ] & catalog$dbfile == catalog[ i , "dbfile" ] ) ) ) {
          
          cat("\n")
          
          # conect to database
          db <- DBI::dbConnect( RSQLite::SQLite() , catalog[ i , "dbfile" ] )
          
          indices <- which( catalog$db_tablename == catalog[ i , 'db_tablename' ] & catalog$dbfile == catalog[ i , "dbfile" ] )
          these_files <- catalog[ indices , "output_filename" ]
          these_cols <- lapply( these_files, function( this_file ) if ( file.exists( this_file ) ) colnames( fst::read_fst( this_file , from = 1 , to = 2 ) ) else NA )
          these_cols <- Reduce( intersect, these_cols )
          
          these_formats <- 
            lapply( these_files, function( this_file ) if ( file.exists( this_file ) ) { 
              y <- fst::read_fst( this_file , from = 1 , to = 2 )
              data.frame( namecols = colnames(y) , colclass = sapply( y , typeof ) , row.names = NULL , stringsAsFactors = FALSE ) 
            } else { NA } )
          these_formats <- suppressWarnings( Reduce( function(x, y) merge(x, y, all=TRUE , by = "namecols"), these_formats ) )
          these_formats <- these_formats[ match(these_cols, these_formats$namecols ) , ]
          # these_formats <- apply( these_formats[ ,-1], 1, function(v) if ( length( unique( v ) ) ) { v[[1]] } else if ( "character" %in% v ) { "character" } )
          if ( length( these_files ) > 1 ){
          these_formats <- apply( these_formats[ ,-1], 1, function(v) if ( length( unique( v ) ) ) { v[[1]] } else if ( "character" %in% v ) { "character" } )
          } else { these_formats <- these_formats[ , -1 ] }
          
          this_format_table <- data.frame( columns = these_cols , formats = these_formats , stringsAsFactors = FALSE )
          
          k=0
          for ( j in indices ) {
            k=k+1
            
            x <- fst::read_fst( catalog[ j , "output_filename" ] , columns = this_format_table[ , 1 ] )
            x <- x[ , this_format_table[ , 1 ] ]
            
            # force formats
            # for (z in seq_along(this_format_table[ , 1 ])) { class( x[ , z ] ) <- this_format_table[ z , 2 ] }
            # for (z in seq_along(this_format_table[ , 1 ])) { typeof( x[ , z ] ) <- this_format_table[ z , 2 ] }
            
            DBI::dbWriteTable( db , catalog[ j, "db_tablename" ] , x , append = TRUE )
            rm(x)
            
            cat( k , "of" , length(indices) , "stored at" , catalog[ j , "db_tablename" ] , "\r" )
            
          }
          
          DBI::dbDisconnect( db )
          cat("\n")
          
        }
        
      }
      
    }
    
    catalog
    
  }
