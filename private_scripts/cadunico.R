downloader::source_url( "https://raw.githubusercontent.com/guilhermejacob/guilhermejacob.github.io/master/scripts/install_packages.R" , quiet = TRUE , prompt = TRUE )

catalog_cadunico <-
  function( output_dir ){
    
    data_portal <- "https://aplicacoes.mds.gov.br/sagi/portal/index.php?grupo=212"
    
    raw_html <- RCurl::getURL( data_portal , .opts = list( ssl.verifypeer = FALSE ) )
    
    w <- rvest::html_attr( rvest::html_nodes( xml2::read_html( raw_html ) , "a" ) , "href" )
    
    these_links <- grep( "base_amostra_cad_|\\/files\\/" , w , value = TRUE , ignore.case = TRUE )
    
    # remove duplicates
    these_links <- these_links[ !duplicated( these_links ) ]
    
    is.microdata <- grepl( "base_amostra_cad_" , these_links , ignore.case = TRUE )
    
    cadunico_years <- as.numeric( ifelse( is.microdata , substr( gsub( "[^0-9]" , "" , these_links ) , 1 , 4 ) , NA ) )
    
    catalog <-
      data.frame(
        year = cadunico_years ,
        dbfolder = paste0( output_dir , "/MonetDB" ) ,
        output_folder = output_dir ,
        full_url = these_links ,
        is.microdata = is.microdata ,
        stringsAsFactors = FALSE
      )
    
    # # remove 2012 because of weird encoding
    # catalog <- catalog[ is.na(catalog$year) | catalog$year != 2012 , ]
    
    # remove documentation
    catalog <- catalog[ catalog$is.microdata , ]
    catalog$is.microdata <- NULL
    
    # sort by year
    catalog[ order( catalog$year , na.last = FALSE ) , ]
    
  }

# datavault
datavault_cadunico <- function( catalog , datavault_dir , skipExist = TRUE ) {
  
  # create datavault links
  catalog[ , "datavault_file" ] <- paste0( datavault_dir , "/" , gsub( ".zip.*" , ".zip" , basename( catalog[ , "full_url" ] ) ) )
  
  # check for existing files
  existing_files <- file.exists( catalog[ , "datavault_file" ] )
  
  # if there isn't any non-downloaded, run download procedure:
  if ( any( !existing_files ) ) {
    
    # print message
    cat( sum( 1*!existing_files ) , "missing files detected.\nDownloading missing files only.\n")
    
    # download files to datavault directory
    for ( i in seq_len( nrow(catalog) )[ !existing_files ] ) {
      
      # skip existing file
      if( skipExist & existing_files[ i ] ) next()
      
      # create directory
      if ( !dir.exists( dirname( catalog[i , "datavault_file" ] ) ) ) dir.create( dirname( catalog[i , "datavault_file" ] ) , recursive = TRUE )
      
      # download file
      download.file( catalog[ i , "full_url" ] , catalog[ i , "datavault_file" ] , quiet = FALSE )
      
      # process tracker
      cat( "file" , i , "out of" , nrow( catalog ) , "downloaded to" , datavault_dir , "\r")
      
    }
    
  }
  
  # print message
  cat( "\ncadunico datavault was built at" , datavault_dir , "\n" )
  
  # return catalog
  catalog
  
}


build_cadunico <-
  function( catalog ){
    
    # define temporary file and folder
    tf <- tempfile()
    td <- file.path( tempdir() , "unzips" )
    
    for ( i in seq_len( nrow( catalog ) ) ){
      
      # create directory
      if ( !dir.exists( dirname( catalog[ i , 'dbfolder' ] ) ) ) dir.create( dirname( catalog[ i , 'dbfolder' ] ) , recursive = TRUE )
      
      # open the connection to the monetdblite database
      db <- DBI::dbConnect( MonetDBLite::MonetDBLite() , catalog[ i , "dbfolder" ] )
      
      # download the file
      if ( is.null( catalog[ i , "datavault_file" ] ) ) {
        download.file( catalog[ i , "full_url" ] , tf , quiet = TRUE , mode = "wb" )
      } else if ( is.na( catalog[ i , "datavault_file" ] ) ) {
        download.file( catalog[ i , "full_url" ] , tf , quiet = TRUE , mode = "wb" ) 
      } else {
        file.copy( catalog[ i , "datavault_file" ] , tf )
      }
      
      # process tracker
      cat( "extracting files.\n")
      
      unzip( tf , exdir = td )
      unzipped_files <- list.files( td , full.names = TRUE , recursive = TRUE )
      
      if( .Platform$OS.type != 'windows' ){
        sapply(unique(dirname(gsub( "\\\\" ,  "/" , unzipped_files ))),dir.create,showWarnings=FALSE)
        file.rename( unzipped_files , gsub( "\\\\" ,  "/" , unzipped_files ) )
        unzipped_files <- gsub( "\\\\" ,  "/" , unzipped_files )
      }
      
      data_files <- grep( "\\.csv$", unzipped_files, value = TRUE , ignore.case = TRUE )
      
      for ( this_table_type in c( "familia" , "pessoa" ) ) {
        
        # process tracker
        cat( "processing data.\n")
        
        # define data file
        this_data_file <- data_files [ grepl( this_table_type , basename( data_files ) , ignore.case = TRUE ) ]
        
        # read data set
        csvdata <- data.table::fread( this_data_file , sep = ";" , dec = "," , showProgress = FALSE , data.table = TRUE )
        
        # fix column names
        column.names <- colnames( csvdata )
        column.names <- gsub( '\"' , "" , column.names ) # remove quotes
        column.names <- gsub( '\\.' , "_" , column.names ) # remove dots
        colnames( csvdata ) <- column.names
        
        # add column of ones
        csvdata[ , one := 1 ]
        
        # store records in table
        DBI::dbWriteTable( db , paste0( this_table_type , "_" , catalog[ i , "year" ] ) , csvdata )
        
        # drop objects and delete file
        rm( csvdata ) ; file.remove( this_data_file ) ; gc()
        
        # process tracker
        cat( tolower( gsub( "\\..*" , "" , basename( this_data_file ) ) ) , "stored at" , paste0( this_table_type , "_" , catalog[ i , "year" ] ) , "\n" )
        
        # # create and merge fpc
        # {
        #   # create fpc table
        #   table_fpc_query <-
        #     paste0( "CREATE TABLE " ,
        #             ifelse( this_table_type == "pessoa" , "pes" , "fam" ) ,
        #             "_fpc AS ( SELECT estrato , SUM( ",
        #             ifelse( this_table_type == "pessoa" , "peso_pes" , "peso_fam" ) ,
        #             " ) as " , ifelse( this_table_type == "pessoa" , "pes" , "fam" ) , "_fpc FROM " ,
        #             this_table_type ,"_" , catalog[ i , 'year' ] ,
        #             " GROUP BY estrato ) "
        #     )
        #   DBI::dbSendQuery( db , table_fpc_query )
        #
        #   fpc_merge_query <- paste0(
        #     'create table cadunico_' ,
        #     catalog[ i , 'year' ] ,
        #     ' as (select a.* , ' , paste( paste0( "b." , setdiff( pes_cols , fam_cols ) ) , collapse = " , " ) ,
        #     ' from familia_' ,
        #     catalog[ i , 'year' ] ,
        #     ' as a inner join pessoa_' ,
        #     catalog[ i , 'year' ] ,
        #     ' as b on a.estrato = b.id_familia' ,
        #     ') WITH DATA'
        #   )
        #
        #   # add fpc variables to the main table
        #
        # }
        
        if (this_table_type == "pessoa") {
          
          # process tracker
          cat( "merging datasets.\n")
          
          # get columns
          pes_cols <- DBI::dbListFields( db , paste0( "pessoa_" , catalog[ i , "year" ] ) )
          fam_cols <- DBI::dbListFields( db , paste0( "familia_" , catalog[ i , "year" ] ) )
          
          # intersect( pes_cols , fam_cols )
          setdiff( pes_cols , fam_cols )
          
          # create merge query
          merge.query <- paste0(
            'create table cadunico_' ,
            catalog[ i , 'year' ] ,
            ' as select a.* , ' , paste( paste0( "b." , setdiff( pes_cols , fam_cols ) ) , collapse = " , " ) ,
            ' from familia_' ,
            catalog[ i , 'year' ] ,
            ' as a inner join pessoa_' ,
            catalog[ i , 'year' ] ,
            ' as b on a.id_familia = b.id_familia' ,
            ''
          )
          
          # send query
          DBI::dbSendQuery( db , merge.query )
          
          # test results
          # stopifnot(
          #   DBI::dbGetQuery( db , paste0( "SELECT COUNT(*) FROM pessoa_", catalog[ i , "year" ] ) )[[1]] ==
          #     DBI::dbGetQuery( db , paste0( "SELECT COUNT(*) FROM cadunico_", catalog[ i , "year" ] ) )[[1]]
          # )
          # stopifnot(
          #   DBI::dbGetQuery( db , paste0( "SELECT SUM( peso_pes ) FROM pessoa_", catalog[ i , "year" ] ) )[[1]] ==
          #     DBI::dbGetQuery( db , paste0( "SELECT SUM( peso_pes ) FROM cadunico_", catalog[ i , "year" ] ) )[[1]]
          # )
          
          # drop persons table
          # DBI::dbRemoveTable( db , paste0( "pessoa_" , catalog[ i , 'year' ] ) )
          
        } else { next() }
        
        # process tracker
        cat( "building survey designs.\n")
        
        # create family design
        this_design <-
          survey::svydesign(
            ids = ~id_familia ,
            strata = ~estrato ,
            weight = ~peso_fam ,
            nest = TRUE ,
            data = paste0( "familia_" , catalog[ i , "year" ] ) ,
            dbtype = "MonetDBLite" ,
            dbname = catalog[ i , 'dbfolder' ]
          )
        
        saveRDS( this_design , file = file.path( output_dir , paste0( "familia " , catalog[ i , "year" ] , " design.rds" ) ) )
        rm( this_design ) 
        
        # cat( paste0( data_name , " survey design entry " , i , " of " , nrow( unique_designs ) , " stored at '" , unique_designs[ i , 'design' ] , "'\r\n\n" ) )
        cat( paste0( "familia " , catalog[ i , "year" ] ) , "design stored" , "\n" )
        
        # create person design
        this_design <-
          survey::svydesign(
            ids = ~id_familia ,
            strata = ~estrato ,
            weight = ~peso_pes ,
            nest = TRUE ,
            data = paste0( "cadunico_" , catalog[ i , "year" ] ) ,
            dbtype = "MonetDBLite" ,
            dbname = catalog[ i , 'dbfolder' ]
          )
        
        saveRDS( this_design , file = file.path( output_dir , paste0( "cadunico " , catalog[ i , "year" ] , " design.rds" ) ) )
        rm( this_design )
        
        # cat( paste0( data_name , " survey design entry " , i , " of " , nrow( unique_designs ) , " stored at '" , unique_designs[ i , 'design' ] , "'\r\n\n" ) )
        cat( paste0( "cadunico " , catalog[ i , "year" ] ) , "design stored" , "\n" )
        
      }
      
      # store case count
      catalog[ i , 'case_count' ] <- DBI::dbGetQuery( db , paste0( "SELECT sum(peso_pes) FROM pessoa_" , catalog[ i , "year" ] ) )[ 1 , 1 ]
      
      # disconnect from the current monet database
      DBI::dbDisconnect( db , shutdown = TRUE )
      
      # delete the temporary files?  or move some docs to a save folder?
      suppressWarnings( file.remove( c(tf , unzipped_files ) ) )
      unlink( td , recursive = TRUE )
      
      # process tracker
      cat( paste0( "cadunico catalog entry " , i , " of " , nrow( catalog ) , " stored in '" , catalog[ i , 'dbfolder' ] , "'\n\n" ) )
      
    }
    
    catalog
    
  }
