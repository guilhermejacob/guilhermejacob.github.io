# DATASUS: SIM, SINASC e SISPRENATAL.

### Este arquivo contém as principais funções necessárias para
### montas as bases de dados do SIM, SINASC e SISPRENATAL do DataSUS.
### A idéia é construir um catálogo com a função catalog_datasus(),
### filtrar (ou não) os dados desejados, depois montar a base de dados no disco.

### funções auxiliares
link_scrape <- function( x ) {
  library(RCurl , quietly = TRUE )
  custom_recodes <- c( "í" = "%ED" , "á" = "%E1" , "ã" = "%E3" , " " = "%20" )
  custom_recodes <- NULL
  if ( !is.null( x ) ){
    this_text <- getURL( x , .encoding = 'ISO-8859-1' , .opts = list(timeout = 10 , dirlistonly = TRUE , ftplistonly = TRUE , ftp.use.epsv = FALSE ) )
    this_text <- unlist( strsplit( this_text , "\n" ) )
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
build_datasus <- function( catalog ) {
  
  # carrega pacotes
  library(data.table)
  library(read.dbc)
  library(fst)
  
  # cria arquivo temporário
  tf <- tempfile()
  
  # "circula" pelas entradas do catálogo
  for ( i in seq_len( nrow( catalog ) ) ){
    
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
