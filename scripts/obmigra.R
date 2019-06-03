string_to_num_with_commas <- function( string ) {
  tryCatch( as.numeric( gsub( ",", "." , string ) ) , error=function(e) string )
}

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
  library(RCurl)
  custom_recodes <- c( "í" = "%ED" , "á" = "%E1" , "ã" = "%E3" , " " = "%20" )
  if ( !is.null( x ) ){
    Sys.sleep(runif(1,.2,1))
    h <- getCurlHandle()
    this_text <- getURL( x , curl = h , .encoding = 'ISO-8859-1' , .opts = list( timeout = 240 , dirlistonly = TRUE , ftplistonly = TRUE , ftp.use.epsv = FALSE ) )
    reset(h)
    this_text <- unlist( strsplit( this_text , "\n" ) )
    this_text <- curlPercentEncode( this_text , codes = custom_recodes )
    paste0( x , this_text )
  } else { 
    NULL 
  }
}

getlisting <- function( these_urls ) {
  library(future.apply)
  these_urls <- ifelse( grepl( "\\/$" , these_urls ) , these_urls , paste0( these_urls , "/" ) )
  res <- future.apply::future_lapply( these_urls , link_scrape )
  res <- unlist( res )
  res <- res[ !grepl( "\\/EEC" , res ) ] # drop EEC folder
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


catalog_obmigra <-
  function( output_dir , ... ){
    
    # define main page
    url_path <- "ftp://ftp.mtps.gov.br/obmigra/dados/microdados/"
    
    # scrape files
    file_list <- recursive_ftp_scrape(url_path)
    
    # create intermediary object
    obm_files <- file_list
    
    # drop layout
    obm_files <- obm_files[ !grepl("layout" , obm_files , ignore.case = TRUE ) ]
    
    # creates catalog
    catalog <-
      data.frame(
        full_url = obm_files ,
        type = ifelse( grepl( "rais" , obm_files ) , "rais" , ifelse( grepl( "cnig" , obm_files ) , "cgig_cnig" , ifelse( grepl( "sincre" , obm_files ) , "sincre" , NA ) ) ) ,
        stringsAsFactors = FALSE
      )
    
    # creates subtype
    catalog$subtype[ catalog$type == "cgig_cnig" ] <- gsub( ".*CNIg(_|\\.)|s(\\.|_).*" , "" , basename( obm_files[ catalog$type == "cgig_cnig" ] ) )
    
    # fix names
    catalog[ grepl( "^prorro" , catalog$subtype ) , "subtype" ] <- "prorrogado"
    catalog[ grepl( "^ind" , catalog$subtype ) , "subtype" ] <- "indeferido"
    
    # get years
    catalog[ , "year" ] <- gsub( "[[:alpha:]]|[[:punct:]]" , "" , basename( obm_files ) )
    
    # creates filename
    catalog[ , "output_filename" ] <- file.path( output_dir , catalog[,"type"] , paste0( ifelse( is.na(catalog[,"subtype"]) , catalog[,"type"] , catalog[,"subtype"] ) , "_" , catalog[,"year"] , ".fst" ) )
    
    # creates database folder
    catalog[ , "dbfolder" ] <- file.path( output_dir , "MonetDB" )
    
    # create tablenames
    # catalog[ , "db_tablename" ] <- paste0( catalog[ , "type" ] , ifelse( is.na( catalog[ , "subtype" ] ) , "" , paste0( "_" , catalog[ , "subtype" ] ) ) )
    catalog[ , "db_tablename" ] <- catalog[ , "type" ]
    
    # # drop RAIS
    # catalog <- catalog[ !grepl( "rais" , catalog$full_url , ignore.case = TRUE ) , ]
    
    # return catalog
    catalog
    
  }

# build datavault
datavault_obmigra <- function( catalog , datavault_dir , skipExist = TRUE ) {
  
  # # get common directory
  # url_path <- get_common_dir( tolower( catalog$full_url ) )
  
  # create datavault links
  catalog$datavault_file <- file.path( datavault_dir , basename(catalog$full_url) )
  
  # check for existing files
  existing_files <- file.exists( catalog$datavault_file )
  
  # create temporary file
  tf <- tempfile()
  
  # if there isn't any non-downloaded, run download procedure:
  if ( any( !existing_files ) ) {
    
    # print message
    cat( sum( 1*!existing_files ) , "missing files detected.\nDownloading missing files only.\n")
    
    # download files to datavault directory
    for ( i in seq_len( nrow(catalog) )[ !existing_files ] ) {
      
      # skip existing file
      if( skipExist & existing_files[ i ] ) next()
      
      # download to temporary file
      download.file( catalog[ i , "full_url" ] , tf , quiet = FALSE , mode = "wb" )
      
      # upon download completion, copy to main file
      dir.create( dirname( catalog[ i , "datavault_file" ] ) , recursive = TRUE , showWarnings = FALSE )
      file.copy( tf , catalog[ i , "datavault_file" ] , overwrite = TRUE )
      
      # remove temporary file
      file.remove(tf)
      
      # process tracker
      cat( "file" , i , "out of" , nrow( catalog ) , "downloaded to" , datavault_dir , "\r")
      
    }
    
  }
  
  # print message
  cat( "\nobmigra datavault was built at" , datavault_dir )
  
  # return catalog
  catalog
  
}

build_obmigra <-
  function( catalog ){
    
    # load libraries
    library(data.table)
    library(readxl)
    library(fst)
    
    # create temporary file
    tf <- tempfile()
    
    # loop through catalog entries
    for ( i in seq_len( nrow( catalog ) ) ) {
      
      # download the file
      if ( is.null( catalog[ i , "datavault_file" ] ) ) {
        download.file( catalog[ i , "full_url" ] , tf , quiet = TRUE , mode = "wb" )
      } else if ( is.na( catalog[ i , "datavault_file" ] ) ) {
        download.file( catalog[ i , "full_url" ] , tf , quiet = TRUE , mode = "wb" ) 
      } else {
        file.copy( catalog[ i , "datavault_file" ] , tf )
      }
      
      # read data
      if ( grepl( "xls" , catalog[ i , "full_url" ] ) ) {
        x <- suppressWarnings( read_xlsx( tf , sheet = 1 ) )
        x <- as.data.table( x )
        x$ano <- catalog[ i , "year" ]
      } else {
        x <- fread( tf , sep = ";" , dec = "," , encoding = "Latin-1" , data.table = TRUE , showProgress = FALSE )
      }
      
      # convert all column names to lowercase
      names( x ) <- tolower( names( x ) )
      
      # remove special characters
      names( x ) <- remove_special_character( names( x ) )
      
      # remove dots
      names( x ) <- gsub( "\\." , "_" , names( x ) )
      
      # remove trailing spaces
      names( x ) <- trimws( names( x ) , which = "both" )
      
      # remove duplicated columns
      if ( any( duplicated( names(x) ) ) ) x <- x[ , !duplicated( names(x) ) , with = FALSE ]
      
      # find some columns character that should be numeric
      vl_cols <- names(x)[ grepl( "_vl"  , names(x) ) & (sapply( x , typeof ) == "character") ]
      
      # transform them to numeric
      suppressWarnings( x[ , ( vl_cols ):= lapply( .SD , function(z) as.numeric( gsub( "," , "." , z ) ) ) , .SDcols = vl_cols ] )
      
      # fix date
      if ( catalog[ i , "type"] == "sincre" & catalog[ i , "year"] == 2000 ) {
        date_cols <- grep( "^data_" , names(x) , value = TRUE )
        x[ , ( date_cols ) := lapply( .SD , function(z) as.character( as.Date( z , format = "%d/%m/%Y" ) ) ) , .SDcols = date_cols ]
      }
      
      # fix formatting
      if ( !grepl( "xls" , catalog[ i , "full_url" ] ) ) {
        char_cols <- colnames(x) [sapply( x , typeof ) == "character"]
        # x[ , ( char_cols ) := lapply( .SD , function(z) iconv( z , from = "latin1" , to = "ASCII//TRANSLIT" ) ) , .SDcols = char_cols ]
        x[ , ( char_cols ) := lapply( .SD , function(z) iconv( z , from = "latin1" ) ) , .SDcols = char_cols ]
      }
      
      # store case count
      catalog[ i , 'case_count' ] <- nrow( x )
      
      # save data
      dir.create( dirname( catalog[ i , "output_filename" ] ) , recursive = TRUE , showWarnings = FALSE )
      write_fst( x , catalog[ i , "output_filename" ] , compress = 100 )
      
      # delete temporary file
      suppressWarnings( file.remove( tf ) )
      
      # process tracker
      cat( paste0( "\robmigra catalog entry " , i , " of " , nrow( catalog ) , " stored at '" , catalog[ i , 'output_filename' ] , "'" ) )
      
    }
    
    # return catalog
    catalog
    
  }

# cria base monetdb
monetdb_obmigra <- function( catalog ) {
  
  # carrega libraries
  library(data.table)
  library(fst)
  library(DBI)
  library(MonetDBLite)
  
  # # deleta base de dados
  # unlink( dbentries[ 1 , "dbfolder" ] , recursive = TRUE )
  
  # combinações únicas de bases de dados e tabelas
  dbentries <- unique( catalog[ , c( "dbfolder" , "db_tablename" ) ] )
  
  # # deleta bases de dados preexistentes
  # for ( this_folder in unique( dbentries[ , "dbfolder" ] ) ) { unlink( this_folder , recursive = TRUE ) }
  
  # para cada base de dados e tabela associada
  for ( i in seq_len( nrow(dbentries) ) ) {
    
    # abre conexão com a tabela
    db <- dbConnect( MonetDBLite() , dbentries[ i , "dbfolder" ] )
    
    # deleta tabela se ela existir:
    if ( dbExistsTable( db , dbentries[ i , "db_tablename" ] ) ) { dbRemoveTable( db , dbentries[ i , "db_tablename" ] ) }
    
    # lista todos os arquivos desta tabela
    these_files <- subset( catalog, dbfolder == dbentries[ i , "dbfolder" ] & db_tablename == dbentries[ i , "db_tablename" ] ) [ , 'output_filename' ]
    
    # coleta nomes e formatos de colunas em todos os arquivos desta base
    these_dts <- lapply( these_files , function(this_file) {
      
      # pula arquivos inexistentes
      if ( !file.exists( this_file ) ) return( NULL )
      
      # lê metadados
      x <- read_fst( this_file , as.data.table = TRUE , from = 1 , to = 1 )
      
      # coleta nome de colunas
      these_cols <- colnames( x )
      
      # coleta formato de colunas
      these_formats <- sapply( x , typeof )
      
      # monta data.table
      data.table( filename = this_file , column_name = these_cols , column_format = these_formats )
      
    } )
    
    # empilha dados
    data_structure <- rbindlist( these_dts , use.names = TRUE )
    
    # reformata estrutura
    data_structure <- dcast( data_structure , column_name ~ column_format , fill = "double" , drop = FALSE , value.var = "column_format" , fun.aggregate = unique )
    
    # define formato final da coluna
    data_structure <- as.data.frame( data_structure )
    data_structure$col_format <- apply( data_structure[ , -1 ] , 1 , function( y ) { ifelse( any( y %in% c( "character", "date" ) ) , "character" ,"numeric" ) } )
    
    # cria estrutura final
    data_structure <- data_structure[ , c( "column_name" , "col_format" ) ]
    
    # lê arquivo, formata colunas e salva dados na tabela da base de dados
    for ( this_file in these_files ) {
      
      # lê arquivo
      x <- read_fst( this_file , as.data.table = TRUE )
      
      # cria colunas ausentes
      missing_cols <- data_structure$column_name [ !is.element( data_structure$column_name , colnames(x) ) ]
      if ( length( missing_cols ) > 0 )  x[ , (missing_cols) := NA ]
      
      # altera formatos
      these_num_cols <- data_structure$column_name[ data_structure$col_format == "numeric" ]
      x[ , (these_num_cols) := lapply( .SD , as.numeric ) , .SDcols = these_num_cols ]
      these_char_cols <- data_structure$column_name[ data_structure$col_format == "character" ]
      x[ , (these_char_cols) := lapply( .SD , as.character ) , .SDcols = these_char_cols ]
      
      # reordena colunas
      setcolorder( x , data_structure$column_name )
      
      # salva dados na tabela
      dbWriteTable( db , dbentries[ i , "db_tablename" ] , x , append = TRUE )
      rm( x ) ; gc()
      
      # acompanhamento de processo
      cat( basename( this_file ) , "stored at" , dbentries[ i , "db_tablename" ] , "\r" )
      
    }
    
    # disconecta da base de dados
    dbDisconnect( db , shutdown = TRUE )
    
  }
  
  # acompanhamento de processo
  cat( "\nobmigra monetdb database was built." )
  
}