# downloader::source_url( "https://raw.githubusercontent.com/guilhermejacob/guilhermejacob.github.io/master/scripts/install_packages.R" , quiet = TRUE , prompt = FALSE )

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
  # res <- lapply( these_urls , links_crape )
  res <- plyr::llply( these_urls , link_scrape , .parallel = TRUE )
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


catalog_fnde <-
  function( output_dir , ... ){
    
    custom_recodes <- c( "í" = "%ED" , "á" = "%E1" , "ã" = "%E3" , " " = "%20" )
    
    output_dir <- ifelse( !grepl( "/$" , output_dir ) , paste0( output_dir , "/" ) , output_dir )
    
    url_path <- "ftp://ftp2.fnde.gov.br/dadosabertos/"
    
    # scrape files
    fnde_files_main <- recursive_ftp_scrape( url_path )
    
    # keep data files only
    fnde_files_main <- fnde_files_main[ grepl( "\\.csv$" , basename( fnde_files_main ) , ignore.case = TRUE ) ]
    
    # # drop opc-pdde files
    # fnde_files_main <- fnde_files_main[ !grepl( "OPC_PDDE" , basename( fnde_files_main ) , ignore.case = TRUE ) ]
    
    # drop incomplete file
    incomplete_files <- c( "PNAE_Escolas_Atendidas.csv" , "PNAE_Recursos_Repassados.csv" )
    fnde_files_main <- fnde_files_main[ !( basename(fnde_files_main) %in% incomplete_files ) ]
    
    # # drop stacked files
    # stacked_files <- grepl( "[0-9]{4}_[0-9]{4}" , basename(fnde_files_main) )
    # fnde_files_main <- fnde_files_main[ !stacked_files ]
    
    # recode urls
    fnde_files <- RCurl::curlPercentEncode( fnde_files_main , codes = custom_recodes )
    
    # create catalog
    catalog <-
      data.frame(
        full_url = fnde_files ,
        type = gsub( "ftp://ftp2.fnde.gov.br/dadosabertos/|\\/.*" , "" , tolower( fnde_files_main ) ) ,
        stringsAsFactors = FALSE
      )
    
    # define subtypes
    siope_types <- c( "consolidados" , "responsaveis" , "estados" , "municipios" , "indicadores" , "complementares" , "receita" , "remuneracao" )
    these_cols <- lapply( siope_types , function( y ) grepl( y , basename( fnde_files_main ) , ignore.case = TRUE ) )
    these_cols <- do.call( cbind , these_cols )[ catalog$type == "siope", ]
    these_types <- apply( these_cols , 1 , function(y) siope_types[ y ] )
    catalog$subtype[ catalog$type == "siope" ] <- these_types
    
    fies_types <- c( "financiamento" , "recompra" )
    these_cols <- lapply( fies_types , function( y ) grepl( y , basename( fnde_files_main ) , ignore.case = TRUE ) )
    these_cols <- do.call( cbind , these_cols )[ catalog$type == "fies", ]
    these_types <- apply( these_cols , 1 , function(y) fies_types[ y ] )
    catalog$subtype[ catalog$type == "fies" ] <- these_types
    
    catalog$subtype[ catalog$type == "fundeb" ] <- "repasses"
    
    pdde_types <- c( paste0( "estimativas_pdde_" , c( "ed_bas" , "ed_esp_priv" ) ) , paste0( "exec_financ_" , c( "ed_bas" , "ed_esp_priv" ) ) )
    pdde_types <- c( pdde_types , paste0( "saldo_contas_pdde_" , c( "part" , "pub" ) ) )
    these_cols <- lapply( pdde_types , function( y ) grepl( y , basename( fnde_files_main ) , ignore.case = TRUE ) )
    these_cols <- do.call( cbind , these_cols )[ catalog$type == "pdde", ]
    pdde_types <- gsub( "_pdde" , "" , pdde_types )
    these_types <- apply( these_cols , 1 , function(y) pdde_types[ y ] )
    catalog$subtype[ catalog$type == "pdde" ] <- these_types
    
    catalog$subtype[ catalog$type == "opc" ] <- "pdde"
    
    pnae_types <- c( "alunos" , "conselho" , "escolas" , "nutricionistas" , "recursos" )
    these_cols <- lapply( pnae_types , function( y ) grepl( y , basename( fnde_files_main ) , ignore.case = TRUE ) )
    these_cols <- do.call( cbind , these_cols )[ catalog$type == "pnae", ]
    these_types <- apply( these_cols , 1 , function(y) pnae_types[ y ] )
    catalog$subtype[ catalog$type == "pnae" ] <- these_types
    
    # define years
    id_years <- grepl( "_[0-9]{4}" , fnde_files_main )
    these_years <- gsub( ".*_([0-9]{4}).*" , "\\1" , fnde_files_main )[ id_years ]
    catalog[ id_years , "year" ] <- as.numeric( these_years )
    
    # define filenames
    catalog$output_filename <- gsub( "\\.csv$" , ".fst" , gsub( url_path , output_dir , tolower( fnde_files_main ) , ignore.case = TRUE ) , ignore.case = TRUE )
    
    # define tablenames
    catalog[ , "db_tablename" ] <- paste( catalog[ , "type" ] , catalog[ , "subtype" ] , catalog[ , "year" ] , sep = "_" )
    catalog[ , "db_tablename" ] <- gsub( "_NA$" , "" , catalog[ , "db_tablename" ] )
    
    # define database folder
    catalog$dbfolder <- ifelse( is.na( catalog$db_tablename ) , NA , paste0( output_dir , "MonetDB/" ) )
    
    catalog
    
  }

# build datavault
datavault_fnde <- function( catalog , datavault_dir , skipExist = TRUE ) {
  
  # get common directory
  url_path <- get_common_dir( tolower( catalog$full_url ) )
  
  # create datavault links
  catalog$datavault_file <- gsub( url_path , paste0( datavault_dir , "/" ), tolower( catalog$full_url ) , ignore.case = TRUE )
  catalog$datavault_file <- gsub( "//" , "/" , catalog$datavault_file , ignore.case = TRUE )
  
  # check for existing files
  existing_files <- file.exists( catalog$datavault_file )
  
  # if there isn't any non-downloaded, run download procedure:
  if ( any( !existing_files ) ) {
    
    # print message
    cat( sum( 1*!existing_files ) , "missing files detected.\nDownloading missing files only.\n")
    
    # download files to datavault directory
    for ( i in seq_len( nrow(catalog) )[ !existing_files ] ) {
      
      # skip existing file
      if( skipExist & existing_files[ i ] ) next()
      
      # download file
      if ( !dir.exists( dirname( catalog$datavault_file[ i ] ) ) ) dir.create( dirname( catalog$datavault_file[ i ] ) , recursive = TRUE ) 
      download.file( catalog$full_url[ i ] , catalog$datavault_file[ i ] , quiet = FALSE , method = "wget" , extra = c( "retry" = 20 ) )
      # f = RCurl::CFILE( catalog$datavault_file[ i ] , mode="wb" )
      # tryCatch( RCurl::curlPerform( url = catalog$full_url[ i ] , writedata = f@ref , .opts = list( ftp.use.epsv = FALSE ) , noprogress = FALSE ) ,
      #           error = function(e) file.remove( catalog[ i , "datavault_file" ]) )
      # RCurl::close(f)
      
      # process tracker
      cat( "file" , i , "out of" , nrow( catalog ) , "downloaded to" , datavault_dir , "\r")
      
    }
    
  }
  
  # print message
  cat( "\nfnde datavault was built at" , datavault_dir , "\n" )
  
  # return catalog
  catalog
  
}

build_fnde <-
  function( catalog ){
    
    # check single database
    if ( length( unique( catalog[ !is.na( catalog[ , "dbfolder" ] ) , "dbfolder" ] ) ) > 1 ) { stop( "Non-unique dbfolder." ) }
    
    tf <- tempfile()
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
      
      if ( !grepl( ".7z$|.zip$" , catalog[ i , 'full_url' ] , ignore.case = TRUE ) ) {
        
        file.copy( tf , catalog[ i , 'output_filename' ] )
        
      } else {
        
        if ( !file.exists( catalog[i , "output_filename" ] ) ) {
          
          archive::archive_extract( normalizePath( tf ) , dir = file.path( td , "unzipped" ) )
          
          this_data_file <- list.files( file.path( tempdir() , "unzipped" ) , full.names = TRUE )
          
          this_data_file <- grep( "\\.csv|\\.txt$", this_data_file, value = TRUE, ignore.case = TRUE )
          
          # faster read file
          x <- tryCatch( suppressWarnings( data.table::fread( this_data_file , sep = ";" , encoding = "Latin-1" , dec = "," , na.strings = "-1" , showProgress = FALSE , data.table = FALSE , stringsAsFactors = FALSE , fill = TRUE , strip.white = TRUE , colClasses = "character" ) ) , 
                         error = function(e) utils::read.csv( this_data_file , sep = ";" , dec = "," , header = TRUE , fileEncoding = "latin1" , as.is = TRUE , colClasses = "character" ) )
          
          # coerce to data.table
          x <- data.table::as.data.table( x )
          
          # remove files and delete temporary unzips folder
          file.remove( tf )
          suppressWarnings( unlink( file.path( td , "unzipped" ) , recursive = TRUE ) )
          
          # convert all column names to lowercase
          names( x ) <- tolower( names( x ) )
          
          # remove special characters
          names( x ) <- remove_special_character( names( x ) )
          
          # remove trailing spaces
          names( x ) <- trimws( names( x ) , which = "both" )
          
          # remove special characters
          names( x ) <- gsub( "\\.$|\\(|\\)" , "" , names( x ) )
          
          # change dots and spaces for underscore
          names( x ) <- gsub( "\\.| |\\/|\\-" , "_" , names( x ) )
          
          # # fix NAs
          # x[ , ] <- apply( x[ , ] , 2 , function( this_vec ) { this_vec [ this_vec == -1 ] <- NA ; return( this_vec ) } )
          
          # figure out which columns really ought to be numeric
          this_pattern <- "cnae|cbo|regiao|municipio|tipo|regioes|ibge|uf|faixa|grau|ind_|sexo|bairro|distrito|raca_cor|competencia|admitidos|saldo"
          for( this_col in names( x )[ !grepl( this_pattern , names(x) ) ] ){
            
            # if the column can be coerced without a warning, coerce it to numeric
            this_result <- tryCatch( as.numeric( gsub( "," , "." , x[ , this_col , with = F ][[1]] ) ), warning = function(c) NULL )
            
            if( !is.null( this_result ) ) x[ , (this_col) := lapply( .SD , function(y) as.numeric( gsub( ",", "." , y ) ) ) , .SDcols = this_col  ]
            
          }
          
          # force uf to numeric
          suppressWarnings( x[ , uf := as.numeric( uf ) ] )
          
          catalog[ i , 'case_count' ] <- nrow( x )
          
          # save file
          if ( !dir.exists( dirname( catalog[ i , 'output_filename' ] ) ) ) dir.create( dirname( catalog[ i , 'output_filename' ] ) , recursive = TRUE )
          fst::write.fst( x , path = catalog[ i , 'output_filename' ] , compress = 100 )
          
        } else {
          x <- fst::read_fst( path = catalog[ i , 'output_filename' ] , as.data.table = TRUE )
        }
        
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
        cat( paste0( "fnde catalog entry " , i , " of " , nrow( catalog ) , " stored at '" , catalog[ i , 'output_filename' ] , "'\n" ) )
        rm( x ) ; gc()
        
        # if this is the final catalog entry for the unique db_tablename, then write them all to the database
        if( i == max( which( catalog$db_tablename == catalog[ i , 'db_tablename' ] ) ) ){
          
          correct_columns <- get( catalog[ i , 'db_tablename' ] )
          
          # open the connection to the monetdb database
          if ( !dir.exists( catalog[ i , "dbfolder" ] ) ) dir.create( catalog[ i , "dbfolder" ] , recursive = TRUE ) 
          db <- DBI::dbConnect( MonetDBLite::MonetDBLite() , catalog[ i , 'dbfolder' ] )
          
          # loop through all tables that match the current db_tablename
          for( this_file in catalog[ catalog$db_tablename %in% catalog[ i , 'db_tablename' ] , 'output_filename' ] ){
            
            x <- fst::read_fst( this_file , as.data.table = TRUE )
            
            for( this_col in setdiff( correct_columns$col_name , names( x ) ) ) x[ , (this_col) := NA ]
            
            # get final table types
            same_table_cols <- get( catalog[ i , 'db_tablename' ] )
            
            for( this_row in seq( nrow( same_table_cols ) ) ){
              
              if( same_table_cols[ this_row , 'col_type' ] != sapply( x , class , USE.NAMES = FALSE )[ same_table_cols[ this_row , 'col_name' ] ] ) {
                
                if( same_table_cols[ this_row , 'col_type' ] == 'numeric' ) x[ , ( same_table_cols[ this_row , 'col_name' ] ) := lapply( .SD , as.numeric ) , .SDcols = same_table_cols[ this_row , 'col_name' ] ]
                if( same_table_cols[ this_row , 'col_type' ] == 'character' ) x[ , ( same_table_cols[ this_row , 'col_name' ] ) := lapply( .SD , as.character ) , .SDcols = same_table_cols[ this_row , 'col_name' ] ]
                
              }
              
            }
            
            # put the columns of x in alphabetical order so they're always the same
            data.table::setcolorder( x , sort( colnames( x ) ) )
            
            # re-save the file
            fst::write.fst( x , path = this_file , compress = 100 )
            
            # append the file to the database
            DBI::dbWriteTable( db , catalog[ i , 'db_tablename' ] , x , append = TRUE , row.names = FALSE )
            
            file_index <- seq_along( catalog[ ( catalog[ i , 'db_tablename' ] == catalog$db_tablename ) & ! is.na( catalog$db_tablename ) , 'output_filename' ] ) [ this_file == catalog[ ( catalog[ i , 'db_tablename' ] == catalog$db_tablename ) & !is.na( catalog$db_tablename ) , 'output_filename' ] ]
            cat( "\r", paste0( "fnde entry " , file_index , " of " , nrow( catalog[ catalog$db_tablename == catalog[ i , 'db_tablename' ] , ] ) , " stored at '" , catalog[ i , 'db_tablename' ] , "'\r" ) )
            
          }
          
        }
        
      }
      
    }
    
    # disconnect from the current database
    DBI::dbDisconnect( db , shutdown = TRUE )
    
    catalog
    
  }

