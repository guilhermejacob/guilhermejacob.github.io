# auxilliary functions
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
  res <- plyr::llply( these_urls , link_scrape , .parallel = TRUE )
  res <- unlist( res )
  res <- res[ !grepl( "SISPRENATAL\\/201201_\\/Doc$", res ) ] # folder not loading in server
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


# create local data catalog:
catalog_datasus <- function( output_dir ){
  
  # define main folder
  sus_htmls <- paste0( "ftp://ftp.datasus.gov.br/dissemin/publicos/" , c( "SIM" , "SINASC" , "SISPRENATAL" ) , "/" )
  
  # scrape directories
  these_links <- recursive_ftp_scrape( sus_htmls )
  
  # drop preliminary files
  these_links <- these_links[ !grepl( "\\/prelim" , these_links , ignore.case = TRUE ) ]
  
  # drop stacked files
  these_links <- these_links[ !grepl( "^D.*BR[0-9]{4}" , basename( these_links ) , ignore.case = FALSE ) ]
  
  # keep data files only
  these_links <- these_links[ grepl( "\\.dbc$" , these_links , ignore.case = TRUE ) ]
  
  # create catalog
  catalog <-
    data.frame(
      full_url = these_links ,
      type = ifelse( grepl( sus_htmls[1] , these_links ) , "sim" ,
                     ifelse( grepl( sus_htmls[2] , these_links ) , "sinasc" ,
                             ifelse( grepl( sus_htmls[3] , these_links ) , "sisprenatal" , NA ) ) ) ,
      stringsAsFactors = FALSE
    )
  
  
  catalog$output_filename <-
    gsub( "dados/" , "" ,
          gsub( "201201_/" , "" ,
                gsub( "\\.dbc$" , ".fst" ,
                      gsub( "ftp://ftp.datasus.gov.br/dissemin/publicos" , output_dir , tolower( these_links ) ) ,
                      ignore.case = TRUE )
          )
    )
  
  year_lines <- gsub( "[^0-9]" , "" , basename( these_links ) )
  
  catalog$year <-
    ifelse( nchar( year_lines ) == 2 & as.numeric( year_lines ) < 79 , 2000 + as.numeric( year_lines ) ,
            ifelse( nchar( year_lines ) == 2 & as.numeric( year_lines ) >= 79 , 1900 + as.numeric( year_lines ) ,
                    ifelse( nchar( year_lines ) == 4 & as.numeric( year_lines ) >= 1996 , as.numeric( year_lines ) ,
                            ifelse( nchar( year_lines ) == 4 & as.numeric( year_lines ) < 1996 , 2000 + as.numeric( substr( year_lines , 1 , 2 ) ) , NA ) ) ) )
  catalog$year <- ifelse( grepl( "\\.dbc$" , these_links , ignore.case = TRUE ) , catalog$year , NA )
  
  catalog$db_tablename <-
    ifelse( !grepl( "dbc$" , catalog$full_url , ignore.case = TRUE ) , NA ,
            ifelse( grepl( "/dofet" , catalog$output_filename ) ,
                    paste0( substr( basename( catalog$output_filename ) , 3 , 5 ) , ifelse( grepl( "/cid9" , catalog$output_filename ) , "_cid9" , "_cid10" ) ) ,
                    ifelse( grepl( "/dores" , catalog$output_filename ) ,
                            paste0( "geral" , ifelse( grepl( "/cid9" , catalog$output_filename ) , "_cid9" , "_cid10" ) ) ,
                            ifelse( grepl( "/sinasc" , catalog$output_filename ) ,
                                    ifelse( grepl( "/dnign" , catalog$output_filename ) , "nign" ,
                                            paste0( "nasc" , ifelse( grepl( "/ant" , catalog$output_filename ) , "_cid9" , "_cid10" ) ) ) ,
                                    ifelse( grepl( "/sisprenatal" , catalog$output_filename ) , "pn" ,
                                            ifelse( grepl( "doign" , catalog$output_filename ) , "dign" , NA ) ) ) ) ) )
  
  # add year suffix
  catalog$db_tablename <- paste( catalog$db_tablename , catalog$year , sep = "_" )
  
  
  catalog$dbfolder <- ifelse( is.na( catalog$db_tablename ) , NA , paste0( output_dir , "/MonetDB" ) )
  
  catalog
  
}

# datavault
datavault_datasus <- function( catalog , datavault_dir , skipExist = TRUE ) {
  
  catalog[ , "datavault_file" ] <- gsub( "ftp://ftp.datasus.gov.br/dissemin/publicos" , datavault_dir , catalog$full_url )
  catalog[ , "datavault_file" ] <- gsub( "//" , "/" , catalog[ , "datavault_file" ] , ignore.case = TRUE )
  
  # check for existing files
  existing_files <- file.exists( catalog[ , "datavault_file" ] )
  
  plyr::llply( seq_along( catalog$full_url ) , function( this_entry ) {
    
    # skip existing file
    if ( file.exists( catalog[ this_entry , "datavault_file" ] ) ){ cat( "file" , this_entry , "out of" , nrow( catalog ) , "downloaded to" , datavault_dir , "\r") ; return( NULL ) }
    
    if ( !dir.exists( dirname( catalog[ this_entry , "datavault_file" ] ) ) ) { dir.create( dirname( catalog[ this_entry , "datavault_file" ] ) , recursive = TRUE ) }
    
    # download file
    download.file( catalog[ this_entry , "full_url" ], catalog[ this_entry , "datavault_file" ] , mode = "wb" , quiet = TRUE )
    
    # process tracker
    cat( "file" , this_entry , "out of" , nrow( catalog ) , "downloaded to" , datavault_dir , "\r")
    
  } , .parallel = TRUE )
  
  # print message
  cat( "\ndatasus datavault was built at" , datavault_dir , "\n" )
  
  # return catalog
  catalog
  
}

# build dataset from catalog:
build_datasus <- function( catalog ) {
  
  if ( !requireNamespace( "read.dbc" , quietly = TRUE ) ) stop( "read.dbc needed for this function to work. to install it, type `install.packages( 'read.dbc' )`" , call. = FALSE )
  
  tf <- tempfile()
  
  for ( i in seq_len( nrow( catalog ) ) ){
    
    # download the file
    if ( is.null( catalog[ i , "datavault_file" ] ) ) {
      download.file( catalog[ i , "full_url" ] , tf , quiet = TRUE , mode = "wb" )
    } else if ( is.na( catalog[ i , "datavault_file" ] ) ) {
      download.file( catalog[ i , "full_url" ] , tf , quiet = TRUE , mode = "wb" ) 
    } else {
      file.copy( catalog[ i , "datavault_file" ] , tf )
    }
    
    # read dbc file
    x <- read.dbc::read.dbc( tf , as.is = TRUE )
    
    # convert all column names to lowercase
    names( x ) <- tolower( names( x ) )
    
    # add underscores after monetdb illegal names
    for ( j in names( x )[ toupper( names( x ) ) %in% getFromNamespace( "reserved_monetdb_keywords" , "MonetDBLite" ) ] ) names( x )[ names( x ) == j ] <- paste0( j , "_" )
    
    # remove trailing spaces
    names( x ) <- trimws( names( x ) , which = "both" )
    
    # fix 2014 format
    if ( "sexo" %in% names(x) ) {
      x[ , sexo := as.character( sexo ) ]
      x[ !( sexo %in% c("1","2","M","F") ) , sexo := NA ]
      x[ sexo == "M" , sexo := 1 ] ; x[ sexo == "F" , sexo := 2 ] 
    }
    
    # figure out which columns really ought to be numeric
    for( this_col in names( x ) ){
      
      # if the column can be coerced without a warning, coerce it to numeric
      this_result <- tryCatch( as.numeric( x[ , this_col ] ) , warning = function(c) NULL , error = function(c) NULL )
      
      if( !is.null( this_result ) ) x[ , this_col ] <- as.numeric( x[ , this_col ] )
      
    }
    
    # force dates to numeric:
    # x[ , grep( "^dt|^data" , colnames(x) ) ] <- apply( x[ , grep( "^dt|^data" , colnames(x) ) ] , 2 , as.numeric )
    
    catalog[ i , 'case_count' ] <- nrow( x )
    
    fst::write_fst( x , path = catalog[ i , 'output_filename' ] , compress = 100 )
    
    these_cols <- sapply( x , class )
    
    these_cols <- data.frame( col_name = names( these_cols ) , col_type = these_cols , stringsAsFactors = FALSE )
    
    if( exists( catalog[ i , 'db_tablename' ] ) ){
      
      same_table_cols <- get( catalog[ i , 'db_tablename' ] )
      same_table_cols <- unique( rbind( these_cols , same_table_cols ) )
      
    } else same_table_cols <- these_cols
    
    dupe_cols <- same_table_cols$col_name[ duplicated( same_table_cols$col_name ) ]
    
    # if there's a duplicate, remove the numeric typed column
    same_table_cols <- same_table_cols[ !( same_table_cols$col_type == 'numeric' & same_table_cols$col_name %in% dupe_cols ) , ]
    
    assign( catalog[ i , 'db_tablename' ] , same_table_cols )
    
    # process tracker
    cat( paste0( "datasus catalog entry " , i , " of " , nrow( catalog ) , " stored at '" , catalog[ i , 'output_filename' ] , "'\r" ) )
    
    
    # if this is the final catalog entry for the unique db_tablename, then write them all to the database
    if ( i == max( which( catalog$db_tablename == catalog[ i , 'db_tablename' ] ) ) ) {
      
      correct_columns <- get( catalog[ i , 'db_tablename' ] )
      
      # open the connection to the monetdblite database
      db <- DBI::dbConnect( MonetDBLite::MonetDBLite() , catalog[ i , 'dbfolder' ] )
      
      cat("\n")
      # loop through all tables that match the current db_tablename
      for( this_file in catalog[ catalog$db_tablename %in% catalog[ i , 'db_tablename' ] , 'output_filename' ] ){
        
        x <- fst::read_fst( this_file )
        
        for( this_col in setdiff( correct_columns$col_name , names( x ) ) ) x[ , this_col ] <- NA
        
        # get final table types
        same_table_cols <- get( catalog[ i , 'db_tablename' ] )
        
        for( this_row in seq( nrow( same_table_cols ) ) ){
          
          if( same_table_cols[ this_row , 'col_type' ] != class( x[ , same_table_cols[ this_row , 'col_name' ] ] ) ){
            
            if( same_table_cols[ this_row , 'col_type' ] == 'numeric' ) x[ , same_table_cols[ this_row , 'col_name' ] ] <- as.numeric( x[ , same_table_cols[ this_row , 'col_name' ] ] )
            if( same_table_cols[ this_row , 'col_type' ] == 'character' ) x[ , same_table_cols[ this_row , 'col_name' ] ] <- as.character( x[ , same_table_cols[ this_row , 'col_name' ] ] )
            
          }
          
        }
        
        # put the columns of x in alphabetical order so they're always the same
        x <- x[ sort( names( x ) ) ]
        
        # re-save the file
        fst::write_fst( x , path = this_file , compress = 100 )
        
        # append the file to the database
        DBI::dbWriteTable( db , catalog[ i , 'db_tablename' ] , x , append = TRUE , row.names = FALSE )
        
        file_index <- seq_along( catalog[ ( catalog[ i , 'db_tablename' ] == catalog$db_tablename ) & ! is.na( catalog$db_tablename ) , 'output_filename' ] ) [ this_file == catalog[ ( catalog[ i , 'db_tablename' ] == catalog$db_tablename ) & ! is.na( catalog$db_tablename ) , 'output_filename' ] ]
        cat( paste0( "datasus entry " , file_index , " of " , nrow( catalog[ catalog$db_tablename == catalog[ i , 'db_tablename' ] , ] ) , " stored at '" , catalog[ i , 'db_tablename' ] , "'\r" ) )
        
      }
      
      cat( "\n" )
      
      # disconnect from the current monet database
      DBI::dbDisconnect( db , shutdown = TRUE )
      
    }
    
    
    # delete the temporary files
    suppressWarnings( file.remove( tf ) )
    
  }
  
  catalog
  
}

