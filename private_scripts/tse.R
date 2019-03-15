downloader::source_url( "https://raw.githubusercontent.com/guilhermejacob/guilhermejacob.github.io/master/scripts/install_packages.R" , quiet = TRUE , prompt = TRUE )

# catalog
catalog_tse <-
  function( output_dir ){
    
    tse_portal <- "http://www.tse.jus.br/hotSites/pesquisas-eleitorais/"
    
    tse_html <- xml2::read_html( paste0( tse_portal , "index.html" ) )
    w <- rvest::html_attr( rvest::html_nodes( tse_html , "a" ) , "href" )
    
    these_branches <- grep( "index|mail" , w , value = TRUE , ignore.case = TRUE , invert = TRUE )
    
    these_links <- plyr::llply( these_branches , function ( this_branch ) {
      
      this_html <- xml2::read_html( paste0( tse_portal , this_branch ) )
      w <- rvest::html_attr( rvest::html_nodes( this_html , "a" ) , "href" )
      these_years <- grep( "_anos" , w , value = TRUE , ignore.case = TRUE )
      
      # years <- gsub( ".*\\/|\\..*" , "" , w )
      these_data_files <- NULL
      
      for ( this_year_page in these_years ) {
        
        this_html <- xml2::read_html( paste0( tse_portal , this_year_page ) )
        w <- rvest::html_attr( rvest::html_nodes( this_html , "a" ) , "href" )
        
        these_data_files <- c( these_data_files , grep( "\\.zip$" , w , value = TRUE , ignore.case = TRUE ) )
        
      }
      
      these_data_files
      
    } , .progress = "text" )
    
    # first result set
    eleitorado_df <-
        data.frame( full_url = these_links[[1]] , 
                    year = suppressWarnings( as.numeric( gsub( ".*_|\\..*" , "" , these_links[[1]] ) ) ),
                    type = "perfil_eleitorado" ,
                    stringsAsFactors = FALSE )
    eleitorado_df[ , "tablename" ] <- paste0( eleitorado_df[ , "type" ] , "_" , eleitorado_df[ , "year" ] )
    # eleitorado_df[ , "dbfolder" ] <- file.path( output_dir , "MonetDB" )
    eleitorado_df <- eleitorado_df[ !is.na( eleitorado_df[ , "year" ] ) , ]
    
    # second result set
    candidatos_df <-
      data.frame( full_url = these_links[[2]] , 
                  year = suppressWarnings( as.numeric( gsub( ".*_|\\..*" , "" , these_links[[2]] ) ) ) ,
                  type = ifelse( grepl( "legend" , basename( these_links[[2]] ) ) , "legenda" ,
                                 ifelse( grepl( "cand" , basename( these_links[[2]] ) ) , "candidato" ,
                                         ifelse( grepl( "cassa" , basename( these_links[[2]] ) ) , "cassacao" ,
                                         ifelse(  grepl( "vaga" , basename( these_links[[2]] ) ) , "vaga" , NA ) ) ) ) ,
                  stringsAsFactors = FALSE )
    candidatos_df[ , "tablename" ] <- paste0( candidatos_df[ , "type" ] , "_" , candidatos_df[ , "year" ] )
    # candidatos_df[ , "dbfolder" ] <- file.path( output_dir , "MonetDB" )
    
    # third result set
    resultados_df <-
      data.frame( full_url = these_links[[3]] , 
                  year = suppressWarnings( as.numeric( gsub( ".*_|\\..*" , "" , these_links[[3]] ) ) ) ,
                  type = ifelse( grepl( "detalhe" , basename( these_links[[3]] ) ) , "votacao_detalhe" ,
                                 ifelse( grepl( "cand" , basename( these_links[[3]] ) ) , "votacao_candidato" ,
                                         ifelse( grepl( "partido" , basename( these_links[[3]] ) ) , "votacao_partido" , NA ) ) ) ,
                  stringsAsFactors = FALSE )
    resultados_df$type <- paste0( resultados_df$type , "_" ,
                                  ifelse( grepl( "_uf" , basename( these_links[[3]] ) ) , "uf" ,
                                          ifelse( grepl( "_secao" , basename( these_links[[3]] ) ) , "secao" ,
                                                  ifelse( grepl( "_munzona" , basename( these_links[[3]] ) ) , "mun" , NA ) ) ) )
    resultados_df <- resultados_df[ grepl( "^http" , resultados_df[ , "full_url" ] ) , ]
    resultados_df[ , "tablename" ] <- paste0( resultados_df[ , "type" ] , "_" , resultados_df[ , "year" ] )
    
    # fourth result set
    contas_df <-
      data.frame( full_url = these_links[[4]] , 
                  year = suppressWarnings( as.numeric( gsub( ".*_|\\..*" , "" , these_links[[4]] ) ) ) ,
                  type = ifelse( grepl( "contas|final" , basename( these_links[[4]] ) ) , "prestacao_contas" ,
                                 ifelse( grepl( "cnpj" , basename( these_links[[4]] ) , ignore.case = TRUE ) , "cnpj" , NA ) ) ,
                  stringsAsFactors = FALSE )
    contas_df$year[ is.na( contas_df$year ) ] <- as.numeric( gsub( ".*cnpj|\\..*", "" , contas_df$full_url[ is.na( contas_df$year ) ] ) )
    contas_df <- contas_df[ !grepl( "parcial" , contas_df[ , "full_url"] ) , ]
    contas_df$type <- paste0( contas_df$type , ifelse( grepl( "financeiro" , basename( contas_df$full_url ) ) , "_financeiro" ,
            ifelse( grepl( "sup" , basename( basename( contas_df$full_url ) ) , ignore.case = TRUE ) , "_sup" , "" ) ) )
    contas_df <- contas_df[ grepl( "^http" , contas_df[ , "full_url" ] ) , ]
    contas_df[ , "tablename" ] <- paste0( contas_df[ , "type" ] , "_" , contas_df[ , "year" ] )
    
    # stack datasets
    catalog <- do.call( rbind , list( eleitorado_df , candidatos_df , resultados_df , contas_df ) )
    
    # create folder/file
    catalog[ , "dbfolder" ] <- file.path( output_dir , "MonetDB" )
    
    # create documentation folder
    catalog[ , "doc_folder" ] <- file.path( output_dir , catalog[ , "year" ] )
    
    # sort by year
    catalog[ order( catalog$year ) , ]
    
  }

# datavault
datavault_tse <- function( catalog , datavault_dir , skipExist = TRUE ) {
  
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
      
      # process tracker
      cat( "file" , i , "out of" , nrow( catalog ) , "downloaded to" , datavault_dir , "\r")
      
    }
    
  }
  
  # print message
  cat( "\ntse datavault was built at" , datavault_dir , "\n" )
  
  # return catalog
  catalog
  
}

build_tse <-
  function( catalog ){
    
    on.exit( print( catalog ) )
    
    tf <- tempfile()
    
    for ( i in seq_len( nrow( catalog ) ) ){
      
      # open the connection to the monetdblite database
      db <- DBI::dbConnect( MonetDBLite::MonetDBLite() , catalog[ i , 'dbfolder' ] )
      
      # download the file
      if ( is.null( catalog[ i , "datavault_file" ] ) ) {
        download.file( catalog[ i , "full_url" ] , tf , quiet = TRUE , mode = "wb" )
      } else if ( is.na( catalog[ i , "datavault_file" ] ) ) {
        download.file( catalog[ i , "full_url" ] , tf , quiet = TRUE , mode = "wb" ) 
      } else {
        file.copy( catalog[ i , "datavault_file" ] , tf )
      }
      
      td <- file.path( tempdir() , "unzipped" )
      
      unzipped_files <- unzip( zipfile = tf , exdir = td )
      
      if( .Platform$OS.type != 'windows' ){
        sapply(unique(dirname(gsub( "\\\\" ,  "/" , unzipped_files ))),dir.create,showWarnings=FALSE)
        file.rename( unzipped_files , gsub( "\\\\" ,  "/" , unzipped_files ) )
        unzipped_files <- gsub( "\\\\" ,  "/" , unzipped_files )
      }
      
      csv_file <- unzipped_files[ grepl( "\\.txt$" , unzipped_files , ignore.case = TRUE ) ]
      csv_file <- csv_file[ !grepl( "_BR\\." , csv_file , ignore.case = TRUE ) ]
      
      # read and fix data
      for (this_csv in csv_file) {
        
        if ( length( readLines( this_csv ) ) <= 0 ) next()
        
        x <- read.csv( this_csv , encoding = "latin1" , stringsAsFactors = FALSE , sep = ";" , header = FALSE , na.strings = "#NULO#" , as.is = TRUE )
        colnames( x ) <- tolower( colnames( x ) )
        
        # write to database
        DBI::dbWriteTable( db , catalog[ i , "tablename" ] , x , append = TRUE )
        
      }
      
      # copy non-data files
      for ( this_file in unzipped_files[ !grepl( "\\.txt$" , unzipped_files , ignore.case = TRUE ) ] ) {
        dest_file <- file.path( catalog[ i , "doc_folder" ] , basename( this_file ) )
        if ( !dir.exists( dirname( dest_file ) ) ) { dir.create( dirname( dest_file ) , recursive = TRUE ) }
        file.copy( from = this_file , to =  dest_file , overwrite = TRUE )
      }
      
      # disconnect from the current monet database
      DBI::dbDisconnect( db , shutdown = TRUE )
      
      # delete the temporary files?  or move some docs to a save folder?
      suppressWarnings( file.remove( tf ) )
      suppressWarnings( unlink( td , recursive = TRUE , force = TRUE ) )
      
      cat( paste0( "tse" , " catalog entry " , i , " of " , nrow( catalog ) , " stored in '" , catalog[ i , 'dbfolder' ] , "'\r\n\n" ) )
      
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

