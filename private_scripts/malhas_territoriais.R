downloader::source_url( "https://raw.githubusercontent.com/guilhermejacob/guilhermejacob.github.io/master/scripts/install_packages.R" , quiet = TRUE , prompt = TRUE )

# output_dir <- "/Volumes/Trabalho/Teste"

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

# function
# link_scrape <- function( x ) { if ( !is.null( x ) ) paste0( x , RCurl::curlPercentEncode( unlist( strsplit( RCurl::getURL( x, dirlistonly = TRUE ) , "\n" ) ) ) ) else NULL }
link_scrape <- function( x ) { if ( !is.null( x ) ) paste0( x , RCurl::curlPercentEncode( unlist( strsplit( RCurl::getURL( x, dirlistonly = TRUE ) , "\n" ) ) ) ) else NULL }
link_scrape <- function( x ) { 
  if ( !is.null( x ) ) {
    these_links <-  rvest::html_nodes( xml2::read_html(x) , "a" ) 
    these_titles <- rvest::html_text( these_links )
    these_links <- rvest::html_attr( these_links , "href" )
    these_links <- these_links [ !( these_titles %in% c("Name","Last modified","Size" , "Description", "Parent Directory") ) ]
    paste0( x , these_links )
  } else{ NULL } }

# recursive ftp scrape
getlisting <- function( these_urls ) {
  these_urls <- ifelse( grepl( "\\/$" , these_urls ) , these_urls , paste0( these_urls , "/" ) )
  # res <- lapply( these_urls , link_scrape )
  res <- plyr::llply( these_urls , link_scrape , .parallel = TRUE )
  
  res <- unlist(res)
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

catalog_malhas <-
  function( output_dir , ... ){
    
    output_dir <- ifelse( !grepl( "/$" , output_dir ) , paste0( output_dir , "/" ) , output_dir )
    
    # restriction
    not_this_pattern <- "\\.(pdf|doc|txt|mdb|kml|ods|kmz)"
    
    # municípios a partir de 2000
    url_path <- "http://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/" 
    
    url_path <- suppressWarnings( getlisting(url_path)[[1]] )
    url_path <- url_path[ !grepl( "municipio_200(5|7)" , url_path ) ]
    municipio_novo_links <- suppressWarnings( recursive_ftp_scrape( url_path ) )
    municipio_novo_links <- sapply( municipio_novo_links, URLdecode , USE.NAMES = FALSE )
    
    # select_file <- grep( "\\.(zip|shp)" , municipio_novo_links , ignore.case = TRUE ))
    select_file <- grep( not_this_pattern , basename(municipio_novo_links) , invert = TRUE , ignore.case = TRUE )
    select_file <- sort( unique(select_file) , decreasing = F)
    
    df1 <- data.frame( 
      full_url = municipio_novo_links[ select_file ] ,
      type = "municipio_pos2000" ,
      year = gsub( ".*municipio_|\\/.*" , "" , municipio_novo_links[ select_file ] ) ,
      output_filename = gsub( ".*malhas_municipais/" , paste0( output_dir , "municipios pos2000/" ) , municipio_novo_links[ select_file ] ) ,
      stringsAsFactors = FALSE
      )
    
    # evolução dos municípios
    municipio_antigo_links <- suppressWarnings( recursive_ftp_scrape( "http://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/municipios_1872_1991/" ) )
    municipio_antigo_links <- sapply( municipio_antigo_links, URLdecode , USE.NAMES = FALSE )
    
    select_file <- grep( not_this_pattern , basename(municipio_antigo_links) , invert = TRUE , ignore.case = TRUE )
    select_file <- sort( unique(select_file) , decreasing = F)
    
    df2 <- data.frame( 
      full_url = municipio_antigo_links[ select_file ] ,
      type = "municipio_pre2000" ,
      year = gsub( ".*divisao_territorial_1872_1991\\/|\\/.*" , "" , municipio_antigo_links[ select_file ] ) ,
      output_filename = gsub( ".*divisao_territorial_1872_1991/" , paste0( output_dir , "municipios pre2000/" ) , municipio_antigo_links[ select_file ] ) ,
      stringsAsFactors = FALSE
    )
    
    # setores censitários 2010
    setor_links <- suppressWarnings( recursive_ftp_scrape( "http://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_de_setores_censitarios__divisoes_intramunicipais/censo_2010/" ) )
    # setor_links <- suppressWarnings( recursive_ftp_scrape( "http://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_de_setores_censitarios__divisoes_intramunicipais/" ) )
    setor_links <- sapply( setor_links, URLdecode , USE.NAMES = FALSE )
    
    select_file <- grep( not_this_pattern , basename(setor_links) , invert = TRUE , ignore.case = TRUE )
    select_file <- sort( unique(select_file) , decreasing = F)
    
    df3 <- data.frame( 
      full_url = setor_links[ select_file ] ,
      type = "setores" ,
      year = gsub( ".*malhas_de_setores_censitarios__divisoes_intramunicipais/censo_|\\/.*" , "" , setor_links[ select_file ] ) ,
      output_filename = gsub( ".*malhas_de_setores_censitarios__divisoes_intramunicipais/" , paste0( output_dir , "setores/" ) , setor_links[ select_file ] ) ,
      stringsAsFactors = FALSE
    )
    
    # localidades
    localidade_links <- suppressWarnings( recursive_ftp_scrape( "http://geoftp.ibge.gov.br/organizacao_do_territorio/estrutura_territorial/localidades/" ) )
    localidade_links <- sapply( localidade_links, URLdecode , USE.NAMES = FALSE )
    
    select_file <- grep( not_this_pattern , basename(localidade_links) , invert = TRUE , ignore.case = TRUE )
    select_file <- sort( unique(select_file) , decreasing = F)
    
    df4 <- data.frame( 
      full_url = localidade_links[ select_file ] ,
      type = "localidades" ,
      year = 2010 ,
      output_filename = gsub( ".*/localidades/" , paste0( output_dir , "localidades/" ) , localidade_links[ select_file ] ) ,
      stringsAsFactors = FALSE
    )
    
    # tipologia
    tipologia_links <- suppressWarnings( recursive_ftp_scrape( "http://geoftp.ibge.gov.br/organizacao_do_territorio/tipologias_do_territorio/" ) )
    tipologia_links <- sapply( tipologia_links, URLdecode , USE.NAMES = FALSE )
    
    select_file <- seq_len(length(tipologia_links))
    # select_file <- grep( not_this_pattern , basename(tipologia_links) , invert = TRUE , ignore.case = TRUE )
    # select_file <- sort( unique(select_file) , decreasing = F)
    
    df5 <- data.frame( 
      full_url = tipologia_links[ select_file ] ,
      # type = "tipologia" ,
      type = gsub( ".*/tipologias_do_territorio/|/.*" , "" , tipologia_links[ select_file ] ) ,
      year = 2010 ,
      output_filename = gsub( ".*/tipologias_do_territorio/" , paste0( output_dir , "tipologia/" ) , tipologia_links[ select_file ] ) ,
      stringsAsFactors = FALSE
    )
    
    catalog <- do.call( rbind , list( df1, df2, df3, df4, df5 ) )
    
    catalog
    
  }

build_malhas <- function( catalog ) {
  
  existing_file <- !file.exists( catalog[ , "output_filename" ] )
  
  catalog <- catalog[ existing_file , ]
  
  plyr::llply( seq_len( nrow(catalog) ) , function( i ) {
    
    if ( !dir.exists( dirname(catalog[i,"output_filename"] ) ) ) { dir.create( dirname(catalog[i,"output_filename"] ) , recursive = TRUE ) }
    download.file( URLencode( catalog[ i , "full_url" ] ), catalog[ i,"output_filename"] , mode = "wb" , quiet = TRUE )
    
  } , .parallel = TRUE )
  
}