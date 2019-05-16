# DATASUS: SIM, SINASC e SISPRENATAL.

### Este arquivo contém as principais funções necessárias para
### montas as bases de dados do SIM, SINASC e SISPRENATAL do DataSUS.
### A idéia é construir um catálogo com a função catalog_datasus(),
### filtrar (ou não) os dados desejados, depois montar as bases de dados
### no disco usando a função build_datasus().

### funções auxiliares
link_scrape <- function( x ) {
  library(RCurl , quietly = TRUE )
  custom_recodes <- c( "í" = "%ED" , "á" = "%E1" , "ã" = "%E3" , " " = "%20" )
  custom_recodes <- NULL
  if ( !is.null( x ) ){
    this_text <- getURL( x , .encoding = 'ISO-8859-1' , .opts = list(timeout = 10 , dirlistonly = TRUE , ftplistonly = TRUE , ftp.use.epsv = FALSE ) )
    this_text <- unlist( strsplit( this_text , "\n" ) )
    this_text <- gsub( "\\r$" , "" , this_text )
    this_text <- curlPercentEncode( this_text , codes = custom_recodes )
    paste0( x , this_text )
  } else { 
    NULL 
  }
}

getlisting <- function( these_urls ) {
  
  library(future.apply)
  plan(multiprocess)
  
  these_urls <- ifelse( grepl( "\\/$" , these_urls ) , these_urls , paste0( these_urls , "/" ) )
  res <- future_lapply( these_urls , link_scrape )
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

### cria catálogo de dados
catalog_datasus <- function( output_dir , drop.prelim = TRUE ){
  
  # define pastas-raízes
  sus_htmls <- paste0( "ftp://ftp.datasus.gov.br/dissemin/publicos/" , c( "SIM" , "SINASC" , "SISPRENATAL" ) , "/" )
  
  # percorre diretórios
  these_links <- recursive_ftp_scrape( sus_htmls )
  
  # remove arquivos preliminares
  if ( drop.prelim ) { these_links <- these_links[ !grepl( "\\/prelim" , these_links , ignore.case = TRUE ) ] }
  
  # deleta arquivos "empilhados"
  these_links <- these_links[ !grepl( "^D.*BR[0-9]{4}" , basename( these_links ) , ignore.case = FALSE ) ]
  
  # mantém apenas arquivos de dados
  these_links <- these_links[ grepl( "\\.dbc$" , these_links , ignore.case = TRUE ) ]
  
  # cria catálogo de links
  catalog <-
    data.frame(
      full_url = these_links ,
      type = ifelse( grepl( sus_htmls[1] , these_links ) , "sim" ,
                     ifelse( grepl( sus_htmls[2] , these_links ) , "sinasc" ,
                             ifelse( grepl( sus_htmls[3] , these_links ) , "sisprenatal" , NA ) ) ) ,
      stringsAsFactors = FALSE
    )
  
  # define nome do arquivo destino
  catalog$output_filename <-
    gsub( "dados/" , "" ,
          gsub( "201201_/" , "" ,
                gsub( "\\.dbc$" , ".fst" ,
                      gsub( "ftp://ftp.datasus.gov.br/dissemin/publicos" , output_dir , tolower( these_links ) ) ,
                      ignore.case = TRUE )
          )
    )
  
  # define anos
  year_lines <- gsub( "[^0-9]" , "" , basename( these_links ) )
  catalog$year <-
    ifelse( nchar( year_lines ) == 2 & as.numeric( year_lines ) < 79 , 2000 + as.numeric( year_lines ) ,
            ifelse( nchar( year_lines ) == 2 & as.numeric( year_lines ) >= 79 , 1900 + as.numeric( year_lines ) ,
                    ifelse( nchar( year_lines ) == 4 & as.numeric( year_lines ) >= 1996 , as.numeric( year_lines ) ,
                            ifelse( nchar( year_lines ) == 4 & as.numeric( year_lines ) < 1996 , 2000 + as.numeric( substr( year_lines , 1 , 2 ) ) , NA ) ) ) )
  catalog$year <- ifelse( grepl( "\\.dbc$" , these_links , ignore.case = TRUE ) , catalog$year , NA )
  
  # cria nomes das tabelas na base de dados
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
  
  # adiciona sufixo de ano
  catalog$db_tablename <- paste( catalog$db_tablename , catalog$year , sep = "_" )
  
  # cria endereço da base de dados
  catalog$dbfolder <- ifelse( is.na( catalog$db_tablename ) , NA , paste0( output_dir , "/MonetDB" ) )
  
  # retorna catálogo
  catalog
  
}

# cria datavault
datavault_datasus <- function( catalog , datavault_dir , skipExist = TRUE ) {
  
  # carrega libraries
  library(future.apply)
  
  # define processamento paralelo
  plan(multiprocess)
  
  # define pastas do datavault
  catalog[ , "datavault_file" ] <- gsub( "ftp://ftp.datasus.gov.br/dissemin/publicos" , datavault_dir , catalog$full_url )
  catalog[ , "datavault_file" ] <- gsub( "//" , "/" , catalog[ , "datavault_file" ] , ignore.case = TRUE )
  
  # procura arquivos existentes
  existing_files <- file.exists( catalog[ , "datavault_file" ] )
  
  # baixa arquivos
  future_lapply( seq_along( catalog$full_url ) , function( this_entry ) {
    
    # pula arquivos existentes
    if ( file.exists( catalog[ this_entry , "datavault_file" ] ) ){ cat( "file" , this_entry , "out of" , nrow( catalog ) , "downloaded to" , datavault_dir , "\r") ; return( NULL ) }
    
    # cria pasta destino
    dir.create( dirname( catalog[ this_entry , "datavault_file" ] ) , recursive = TRUE , showWarnings = FALSE )
    
    # baixa arquivo
    download.file( catalog[ this_entry , "full_url" ], catalog[ this_entry , "datavault_file" ] , mode = "wb" , quiet = TRUE )
    
    # acompanhamento de processo
    cat( "file" , this_entry , "out of" , nrow( catalog ) , "downloaded to" , datavault_dir , "\r")
    
  } )
  
  # mostra mensagem
  cat( "\ndatasus datavault was built at" , datavault_dir , "\n" )
  
  # returna catálogo
  catalog
  
}


### monta bases de dados
build_datasus <- function( catalog , skipExist = TRUE ) {
  
  # carrega pacotes
  library(data.table)
  library(read.dbc)
  library(fst)
  
  # cria arquivo temporário
  tf <- tempfile()
  
  # "circula" pelas entradas do catálogo
  for ( i in seq_len( nrow( catalog ) ) ){
    
    # pula arquivo existente
    if (skipExist & file.exists( catalog[ i , "output_filename"] ) ) {
      cat( paste0( catalog[ i , 'output_filename' ] , " already exists. Skippping.\r" ) )
      next()
    } 
    
    # baixa arquivo de dados
    if ( is.null( catalog[ i , "datavault_file" ] ) ) {
      download.file( catalog[ i , "full_url" ] , tf , quiet = TRUE , mode = "wb" )
    } else if ( is.na( catalog[ i , "datavault_file" ] ) ) {
      download.file( catalog[ i , "full_url" ] , tf , quiet = TRUE , mode = "wb" ) 
    } else {
      file.copy( catalog[ i , "datavault_file" ] , tf , overwrite = TRUE )
    }
    
    # lê arquivo .dbc
    x <- read.dbc( tf , as.is = TRUE )
    
    # converte para data.table
    x <- data.table( x )
    
    # nomes das colunas em minúsculo
    names( x ) <- tolower( names( x ) )
    
    # remove espaços desnecessários nos nomes
    names( x ) <- trimws( names( x ) , which = "both" )
    
    # força variáveis de códigos para caractere
    code_vars <- names( x )[ grepl( "^(cod|causabas|linha|ocup|dt|numero|idade|sexo)" , names( x ) ) ]
    x[ , (code_vars) := lapply( .SD , as.character ) , .SDcols = code_vars ]
    
    
    # ajusta formato da variável sexo em 2014
    if ( "sexo" %in% names(x) ) {
      x[ !( sexo %in% c("1","2","M","F") ) , sexo := NA ]
      x[ sexo == "M" , sexo := "1" ] ; x[ sexo == "F" , sexo := "2" ]
      x[ , sexo := as.numeric( sexo) ]
    }
    
    # descobre quais colunas podem ser numéricas
    for( this_col in names( x )[ !grepl( "^(cod|causabas|linha|ocup|dt|numero|idade|sexo)" , names( x ) ) ] ){
      
      # se pode ser convertida sem alerta, converte para númerico
      this_result <- tryCatch( as.numeric( x[[this_col]] ) , warning = function(c) NULL , error = function(c) NULL )
      
      # converte para numérico
      if( !is.null( this_result ) ) x[ , (this_col) := lapply( .SD , as.numeric ) , .SDcols = this_col ]
      
    }
    
    # salva contagem de casos
    catalog[ i , 'case_count' ] <- nrow( x )
    
    # salva arquivo no disco
    dir.create( dirname( catalog[ i , 'output_filename' ] ) , recursive = TRUE , showWarnings = FALSE )
    write_fst( x , path = catalog[ i , 'output_filename' ] , compress = 100 )
    
    # acompanhamento de processo
    cat( paste0( "datasus catalog entry " , i , " of " , nrow( catalog ) , " stored at '" , catalog[ i , 'output_filename' ] , "'\r" ) )
    
    # deleta arquivo temporário
    suppressWarnings( file.remove( tf ) )
    
  }
  
  # retorna catálogo
  catalog
  
}

### cria base de dados monetdb
monetdb_datasus <- function( catalog ) {
  
  # carrega libraries
  library(data.table)
  library(fst)
  library(DBI)
  library(MonetDBLite)
  
  # combinações únicas de bases de dados e tabelas
  dbentries <- unique( catalog[ , c( "dbfolder" , "db_tablename" ) ] )
  
  # # deleta bases de dados preexistentes
  # for ( this_folder in unique( dbentries[ , "dbfolder" ] ) ) { unlink( this_folder , recursive = TRUE ) }
  
  # para cada base de dados e tabela associada
  for ( i in nrow(dbentries) ) {
    
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
  cat( "\ndatasus monetdb database was built." )
  
}
