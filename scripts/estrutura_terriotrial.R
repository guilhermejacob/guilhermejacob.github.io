# baixa dados sobre estrutura territorial
# Fonte: IBGE.
# Link: http://geoftp.ibge.gov.br/organizacao_do_territorio/estrutura_territorial/divisao_territorial/

# define output dir
output_dir <- "~/Bases/Estrutura Territorial"
output_dir <-  path.expand( output_dir )

unlink( output_dir , recursive = TRUE )

# define datavault
datavault_dir <- "~/DataVault/Estrutura Territorial"

# funcções auxiliares
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
  library(xml2)
  library(rvest)
  these_urls <- ifelse( grepl( "\\/$" , these_urls ) , these_urls , paste0( these_urls , "/" ) )
  res <- lapply( these_urls , function( x ) { if ( !is.null( x ) ) paste0( x , html_attr( html_nodes( read_html(x) , "a" ) , "href" ) )[-1:-5] else NULL } )
  res <- unlist( res )
  is.file <- grepl( "\\.[a-z]{3}$" , basename( res ) , ignore.case = TRUE )
  res[ !is.file ] <- ifelse( grepl( "\\/$" , res[ !is.file ] ) , res[ !is.file ] , paste0( res[ !is.file ] , "/" ) )
  list( dirs = if ( any(!is.file) ) { res[ !is.file ] } else { NULL } , 
        files = if ( any(is.file) ) { res[ is.file ] } else { NULL } )
}

recursive_ftp_scrape <- function( main_url ) {
  
  final_files <- NULL
  
  directories <- main_url
  # directories <- directories[ 1:5 ]
  
  while ( length(directories) > 0) {
    listing <- getlisting(directories)
    directories <- listing[[1]]
    files <- listing[[2]]
    final_files <- c(final_files, files )
  }
  
  final_files
  
}

remove_special_character <- function(x) {
  stripped_x <- iconv( x , to = "ascii//translit" , sub = "_" )
  stripped_x <- gsub( "('|~|\\^)" , "" , stripped_x , ignore.case = TRUE )
  stripped_x
}

### cria catálogo

# deifne diretório principal
url_path <- "http://geoftp.ibge.gov.br/organizacao_do_territorio/estrutura_territorial/divisao_territorial/"

# coleta links
full_list <- recursive_ftp_scrape( url_path )

# filtra links de dados
file_list <- full_list[ !grepl( "leia|txt" , basename(full_list) , ignore.case = TRUE ) ]

# build catalog
catalog <-
  data.frame(
    full_url = file_list ,
    year = as.integer( gsub( ".*\\/" , "" , dirname( file_list ) ) ) ,
    output_folder = file.path( output_dir , gsub( ".*\\/" , "" , dirname( file_list ) ) ) ,
    stringsAsFactors = FALSE
  )

# remove 1994
catalog <- catalog[ !( catalog$year %in% c(1994,2013) ) , ] 


### cria datavault

# get common directory
url_path <- get_common_dir( tolower( catalog$full_url ) )

# create datavault links
catalog[ , "datavault_file" ] <- gsub( url_path , paste0( datavault_dir , "/" ), tolower( catalog$full_url ) , ignore.case = TRUE )
catalog[ , "datavault_file" ] <- gsub( "//" , "/" , catalog[ , "datavault_file" ] , ignore.case = TRUE )

# check for existing files
existing_files <- file.exists( catalog[ , "datavault_file" ] )

# if there isn't any non-downloaded, run download procedure:
if ( any( !existing_files ) ) {
  
  # print message
  cat( sum( 1*!existing_files ) , "missing files detected.\nDownloading missing files only.\n")
  
  # download files to datavault directory
  for ( i in seq_len( nrow(catalog) )[ !existing_files ] ) {
    
    # skip existing file
    if( existing_files[ i ] ) next()
    
    # create missing directory
    if ( !dir.exists( dirname( catalog[ i , "datavault_file" ] ) ) ) { dir.create( dirname( catalog[ i , "datavault_file" ] ) , recursive = TRUE ) }
    
    # download file
    download.file( catalog[ i , "full_url" ] , catalog[ i , "datavault_file" ] , quiet = FALSE )
    
    # process tracker
    cat( "file" , i , "out of" , nrow( catalog ) , "downloaded to" , datavault_dir , "\r")
    
  }
  
}

### processa dados

# carrega libraries
library(data.table)
library(readxl)

# cria arquivo e diretório temporários
tf <- tempfile()
td <- file.path( tempdir() , "unzips" )

# loop pelas linhas do catálogo
for ( i in seq_len( nrow( catalog ) ) ){
  
  # download the file
  if ( is.null( catalog[ i , "datavault_file" ] ) ) {
    download.file( catalog[ i , "full_url" ] , tf , quiet = TRUE , mode = "wb" )
  } else if ( is.na( catalog[ i , "datavault_file" ] ) ) {
    download.file( catalog[ i , "full_url" ] , tf , quiet = TRUE , mode = "wb" ) 
  } else {
    file.copy( catalog[ i , "datavault_file" ] , tf )
  }
  
  # lê arquivo
  if ( grepl( ".xls$" , catalog[i , "full_url"] , ignore.case = TRUE ) ) {
    this_data_file <- tf
    
    # lê arquivo
    x <- read_xls(this_data_file)
    
    # converte para data.table
    x <- as.data.table( x )
    
    # padroniza colunas
    colnames(x) <- tolower( colnames(x) )
    
    # salva arquivo
    if ( !dir.exists( catalog[ i, "output_folder" ] ) ) dir.create( catalog[ i, "output_folder" ] , recursive = TRUE )
    saveRDS( x , file = file.path( catalog[ i, "output_folder" ] , gsub( "xls" , "Rds" , basename( catalog[ i , "full_url" ] ) ) ) )
  }
  
  # extrai e lê arquivo
  if ( grepl( ".zip$" , catalog[i , "full_url"] , ignore.case = TRUE ) & catalog[ i , "year" ] < 2013 ) {
    unzip( tf , exdir = td )
    this_data_file <- list.files( td , full.names = TRUE )[[1]]
    
    # lê arquivo
    x <- read_xls(this_data_file)
    
    # converte para data.table
    x <- as.data.table( x )
    
    # padroniza colunas
    colnames(x) <- tolower( colnames(x) )
    
    # salva arquivo
    if ( !dir.exists( catalog[ i, "output_folder" ] ) ) dir.create( catalog[ i, "output_folder" ] , recursive = TRUE )
    saveRDS( x , file = file.path( catalog[ i, "output_folder" ] , gsub( "xls" , "Rds" , basename( catalog[ i , "full_url" ] ) ) ) )
    
  }
  
  # extrai e lê arquivo
  if ( grepl( ".zip$" , catalog[i , "full_url"] , ignore.case = TRUE ) & catalog[ i , "year" ] >= 2013 ) {
    unzip( tf , exdir = td )
    these_data_files <- list.files( td , recursive = TRUE , full.names = TRUE , pattern = "xls" )
    
    for (this_data_file in these_data_files) {
      
      # lê arquivo
      x <- read_xls(this_data_file)
      
      # converte para data.table
      x <- as.data.table( x )
      
      # padroniza colunas
      colnames(x) <- tolower( colnames(x) )
      
      # salva arquivo
      if ( !dir.exists( catalog[ i, "output_folder" ] ) ) dir.create( catalog[ i, "output_folder" ] , recursive = TRUE )
      saveRDS( x , file = file.path( catalog[ i, "output_folder" ] , gsub( "xls" , "Rds" , tolower( basename( this_data_file ) ) ) ) )
      
    }
  }
  
  # exclui arquivo e pasta temporarios
  file.remove( tf )
  if ( dir.exists( td ) ) unlink( td , recursive = TRUE )
  
}
