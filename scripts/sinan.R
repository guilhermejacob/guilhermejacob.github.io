# DATASUS: SINAN.

### Este arquivo contém as principais funções necessárias para
### montar as bases de dados do SINAN/DataSUS.
### A idéia é construir um catálogo com a função catalog_sinan(),
### filtrar (ou não) os dados desejados, depois montar as bases de dados
### no disco usando a função build_sinan().

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
catalog_sinan <- function( output_dir , drop.prelim = TRUE ){
  
  # define pastas-raízes
  sus_htmls <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/"
  
  # percorre diretórios
  these_links <- recursive_ftp_scrape( sus_htmls )
  
  # remove arquivos preliminares
  if ( drop.prelim ) { these_links <- these_links[ !grepl( "\\/prelim" , these_links , ignore.case = TRUE ) ] }
  
  # mantém apenas arquivos de dados
  these_links <- these_links[ grepl( "dados.*\\.(dbc|dbf)$" , tolower(these_links) ) ]
  
  # cria catálogo de links
  catalog <-
    data.frame(
      full_url = these_links ,
      type = "sinan" ,
      subtype = substr( basename( tolower(these_links) ) , 1 , 4 ) ,
      stringsAsFactors = FALSE
    )
  
  # redefine subtipos
  catalog$subtype[ catalog$subtype == "anim" ] <- "peconhento"
  catalog$subtype[ catalog$subtype == "botu" ] <- "botulismo"
  catalog$subtype[ catalog$subtype == "chag" ] <- "chagas"
  catalog$subtype[ catalog$subtype == "coqu" ] <- "coqueluche"
  catalog$subtype[ catalog$subtype == "cole" ] <- "colera"
  catalog$subtype[ catalog$subtype == "deng" ] <- "dengue"
  catalog$subtype[ catalog$subtype == "dift" ] <- "difteria"
  catalog$subtype[ catalog$subtype == "esqu" ] <- "esquistossomose"
  catalog$subtype[ catalog$subtype == "exan" ] <- "exantematica"
  catalog$subtype[ catalog$subtype == "fama" ] <- "febre_amarela"
  catalog$subtype[ catalog$subtype == "fmac" ] <- "febre_maculosa"
  catalog$subtype[ catalog$subtype == "ftif" ] <- "febre_tifoide"
  catalog$subtype[ catalog$subtype == "hans" ] <- "hanseniase"
  catalog$subtype[ catalog$subtype == "hant" ] <- "hantavirose"
  catalog$subtype[ catalog$subtype == "hepa" ] <- "hepa"
  catalog$subtype[ catalog$subtype == "iexo" ] <- "intoxicacao_exogena"
  catalog$subtype[ catalog$subtype == "lept" ] <- "leptospirose"
  catalog$subtype[ catalog$subtype == "leiv" ] <- "leishmaniose_visceral"
  catalog$subtype[ catalog$subtype == "ltan" ] <- "leishmaniose_tegumentar"
  catalog$subtype[ catalog$subtype == "mala" ] <- "malaria"
  catalog$subtype[ catalog$subtype == "meni" ] <- "meningite"
  catalog$subtype[ catalog$subtype == "pest" ] <- "peste"
  catalog$subtype[ catalog$subtype == "pfan" ] <- "paralisia_flacida_aguda"
  catalog$subtype[ catalog$subtype == "raiv" ] <- "raiva"
  catalog$subtype[ catalog$subtype == "teta" ] <- "tetano_acidental"
  catalog$subtype[ catalog$subtype == "tetn" ] <- "tetano_neonatal"
  catalog$subtype[ catalog$subtype == "tube" ] <- "tuberculose"
  catalog$subtype[ catalog$subtype == "viol" ] <- "violencia"
  
  # define anos
  year_lines <- gsub( "[^0-9]" , "" , basename( these_links ) )
  catalog$year <-
    ifelse( nchar( year_lines ) == 2 & as.numeric( year_lines ) < 79 , 2000 + as.numeric( year_lines ) ,
            ifelse( nchar( year_lines ) == 2 & as.numeric( year_lines ) >= 79 , 1900 + as.numeric( year_lines ) ,
                    ifelse( nchar( year_lines ) == 4 & as.numeric( year_lines ) >= 1996 , as.numeric( year_lines ) ,
                            ifelse( nchar( year_lines ) == 4 & as.numeric( year_lines ) < 1996 , 2000 + as.numeric( substr( year_lines , 1 , 2 ) ) , NA ) ) ) )
  catalog$year <- ifelse( grepl( "\\.(dbc|dbf)$" , these_links , ignore.case = TRUE ) , catalog$year , NA )
  
  # define nome do arquivo destino
  catalog$output_filename <- 
    file.path( output_dir , 
               catalog$type , 
               catalog$subtype , 
               gsub( "\\.db(c|f)$" , ".fst" , tolower( basename( these_links ) ) ) )
  
  # reordena catálogo
  catalog <- catalog[ order( catalog$type , catalog$subtype , catalog$year ) , ]
  
  # corrige numero de linhas
  row.names(catalog) <- NULL
  
  # retorna catálogo
  catalog
  
}

# cria datavault
datavault_sinan <- function( catalog , datavault_dir , skipExist = TRUE ) {
  
  # carrega libraries
  library(future.apply)
  
  # define processamento paralelo
  plan(multiprocess)
  
  # define pastas do datavault
  catalog[ , "datavault_file" ] <- gsub( "ftp://ftp.datasus.gov.br/dissemin/publicos" , datavault_dir , catalog$full_url , fixed = TRUE )
  catalog[ , "datavault_file" ] <- gsub( "//" , "/" , catalog[ , "datavault_file" ] , fixed = TRUE )
  
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
  cat( "\nsinan datavault was built at" , datavault_dir , "\n" )
  
  # returna catálogo
  catalog
  
}


### monta bases de dados
build_sinan <- function( catalog , skipExist = TRUE ) {
  
  # carrega pacotes
  library(data.table)
  library(read.dbc)
  library(fst)
  library(stringr)
  
  # cria arquivo temporário
  tf <- tempfile()
  
  # "circula" pelas entradas do catálogo
  for ( i in seq_len( nrow( catalog ) ) ){
    
    # pula arquivo existente
    if (skipExist & file.exists( catalog[ i , "output_filename"] ) ) {
      catalog[ i , 'case_count' ] <- fst.metadata( catalog[ i , "output_filename"] )$nrOfRows
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
    code_vars <- names( x )[ grepl( "(munic|uf)" , names( x ) ) ]
    x[ , (code_vars) := lapply( .SD , as.character ) , .SDcols = code_vars ]
    
    # # ajusta datas
    # date_vars <- names( x )[ grepl( "(dt)_" , names( x ) ) ]
    # x[ , (date_vars) := lapply( .SD , function(z) as.Date( z , format = "%YYYY-%MM-%DD" ) ) , .SDcols = date_vars ]
    # x[ , (date_vars) := lapply( .SD , as.character ) , .SDcols = date_vars ]
    
    # salva contagem de casos
    catalog[ i , 'case_count' ] <- nrow( x )
    
    # salva arquivo no disco
    dir.create( dirname( catalog[ i , 'output_filename' ] ) , recursive = TRUE , showWarnings = FALSE )
    if ( nrow(x) > 0 ) write_fst( x , path = catalog[ i , 'output_filename' ] , compress = 100 )
    
    # acompanhamento de processo
    cat( paste0( "sinan catalog entry " , i , " of " , nrow( catalog ) , " stored at '" , catalog[ i , 'output_filename' ] , "'\r" ) )
    
    # deleta arquivo temporário
    suppressWarnings( file.remove( tf ) )
    
  }
  
  # retorna catálogo
  catalog
  
}

### cria base de dados monetdb
monetdb_sinan <- function( catalog , skipExist = TRUE ) {
  
  # carrega libraries
  library(data.table)
  library(fst)
  library(DBI)
  library(MonetDBLite)
  
  # filtra apenas para arquivos existentes
  catalog <- catalog[ file.exists( catalog$output_filename ) , ]
  
  # cria nomes das tabelas na base de dados
  catalog$db_tablename <- paste( catalog$type , catalog$subtype , sep = "_")
  
  # adiciona sufixo de ano
  catalog$db_tablename <- paste( catalog$db_tablename , catalog$year , sep = "_" )
  
  # cria endereço da base de dados
  catalog$dbfolder <- ifelse( is.na( catalog$db_tablename ) , NA , paste0( output_dir , "/MonetDB" ) )
  
  # combinações únicas de bases de dados e tabelas
  dbentries <- unique( catalog[ , c( "dbfolder" , "db_tablename" ) ] )
  
  # ajusta linhas
  rownames(dbentries) <- NULL
  
  # # deleta bases de dados preexistentes
  # for ( this_folder in unique( dbentries[ , "dbfolder" ] ) ) { unlink( this_folder , recursive = TRUE ) }
  
  # abre conexão com a tabela
  db <- dbConnect( MonetDBLite() , unique( dbentries[ , "dbfolder" ] )[[1]] )
  
  # acompanhamento de processo
  cat( "\nsinan monetdb process started.\n" )
  
  # para cada base de dados e tabela associada
  for ( i in seq_len( nrow(dbentries) ) ) {
    
    # # abre conexão com a tabela
    # db <- dbConnect( MonetDBLite() , dbentries[ i , "dbfolder" ] )
    
    # testa se a tabela já existe na base de dados
    if ( dbExistsTable( db , dbentries[ i , "db_tablename" ] ) ) {
      
      # Se skipExist == TRUE, pula para a próxima tabela.
      # Caso contrário, deleta tabela existente e processa os dados
      if ( skipExist ) { 
        cat( dbentries[ i , "db_tablename" ] , "already exists. Skipping.\r" )
        next() 
      } else { 
        cat( dbentries[ i , "db_tablename" ] , "already exists. Removing.\r" )
        dbRemoveTable( db , dbentries[ i , "db_tablename" ] ) 
      }
      
    }
    
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
      these_formats <- sapply( x , class )
      
      # monta data.table
      data.table( filename = this_file , column_name = these_cols , column_format = these_formats )
      
    } )
    
    # empilha dados
    data_structure <- rbindlist( these_dts , use.names = TRUE )
    
    # reformata estrutura
    data_structure <- dcast( data_structure , column_name ~ column_format , fill = "double" , drop = FALSE , value.var = "column_format" , fun.aggregate = unique )
    
    # define formato final da coluna
    data_structure <- as.data.frame( data_structure )
    data_structure$col_format <- apply( data_structure[ , -1 ] , 1 , function( y ) { ifelse( any( tolower(y) %in% c( "character", "date" ) ) , "character" ,"numeric" ) } )
    
    # cria estrutura final
    data_structure <- data_structure[ , c( "column_name" , "col_format" ) ]
    
    # lê arquivo, formata colunas e salva dados na tabela da base de dados
    for ( this_file in these_files ) {
      
      # pula arquivos inexistentes
      if ( !file.exists( this_file ) ) next()
      
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
    
    # # disconecta da base de dados
    # dbDisconnect( db , shutdown = TRUE )
    
  }
  
  # disconecta da base de dados
  dbDisconnect( db , shutdown = TRUE )
  
  # acompanhamento de processo
  cat( "\nsinan monetdb database was built." )
  
}