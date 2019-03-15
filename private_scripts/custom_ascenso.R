source( "~/Documents/GitLab/src-website/content/private_scripts/ascenso.R" , prompt.echo = FALSE )

library(doParallel)
library(plyr)
nodes <- detectCores()
cl <- makeCluster(nodes)
registerDoParallel(cl)
# stopImplicitCluster()

output_dir <- "/Volumes/Trabalho/ASC"
unlink( output_dir , recursive = TRUE )
datavault_dir <- "/Volumes/DataVault/ASC"
# unlink( datavault_dir , recursive = TRUE )

catalog_init <- catalog_ascenso( output_dir = output_dir )

catalog_dv <- datavault_ascenso( catalog = catalog_init , datavault_dir = datavault_dir )

catalog <- catalog_dv
# catalog <- catalog[ catalog$year == 2007 , ]

# catalog <- catalog[ sample( seq_len( nrow(catalog) ) , 10 ) , ]

##### parallelized build

# load data.table
library(data.table)

tf <- tempfile()
td <- file.path( tempdir() , "unzips" )

# create full results list
full_results <- NULL


for ( i in seq_len( nrow( catalog ) ) ){
  
  # download the file
  if ( is.null( catalog[ i , "datavault_file" ] ) ) {
    download.file( catalog[ i , "full_url" ] , tf , quiet = TRUE , mode = "wb" )
  } else if ( is.na( catalog[ i , "datavault_file" ] ) ) {
    download.file( catalog[ i , "full_url" ] , tf , quiet = TRUE , mode = "wb" )
  } else {
    file.copy( catalog[ i , "datavault_file" ] , tf , overwrite = TRUE )
  }
  
  # extract files
  unzip( tf , exdir = td )
  
  # list files
  these_data_files <- list.files( td , recursive = TRUE , full.names = FALSE )
  
  # fix names
  these_data_files_fixed <- sapply( these_data_files , URLdecode , USE.NAMES = FALSE )
  these_data_files_fixed <- iconv( these_data_files_fixed , from = "CP850" )
  for ( this_dir in unique( file.path( td , dirname( these_data_files_fixed ) ) ) ) { dir.create( this_dir , recursive = TRUE , showWarnings = FALSE ) }
  file.rename( file.path( td , these_data_files ) , file.path( td , these_data_files_fixed ) )
  these_data_files <- these_data_files_fixed
  
  # create full paths
  these_data_files <- file.path( td , these_data_files )
  
  # keep xls only
  these_data_files <- these_data_files[ grepl( "\\.xls$" , basename( these_data_files ) , ignore.case = TRUE) ]
  
  # drop documentation
  these_data_files <- these_data_files[ !grepl( "descr|compat" , basename( these_data_files ) , ignore.case = TRUE) ]
  
  if ( catalog[ i , "year" ] == 2000 ) {
    
    # set variables for passing into plyr::llply
    this_output_folder <- catalog[ i, "output_folder" ]
    this_year <- catalog[ i, "year" ]
    this_url <- catalog[ i, "full_url" ]
    
    # process data
    these_results <- plyr::llply( these_data_files , function( this_file ) {
      
      # read data
      x <- readxl::read_xls( this_file )
      
      # force data.table
      x <- data.table::data.table( x , stringsAsFactors = FALSE )
      
      # fix names
      colnames( x ) <- tolower( colnames( x ) )
      colnames( x ) <- remove_special_character( colnames( x ) )
      
      # set up tablename
      this_tablename <- tolower( gsub( "_.*" , "" , basename( this_file ) ) )
      this_tablename <- paste( this_tablename , this_year , sep = "_" )
      
      # read into database
      # DBI::dbWriteTable( db , this_tablename , x , append = TRUE )
      
      # count cases
      case_count <- nrow(x)
      
      # define output file
      this_output_file <- paste0( this_output_folder , "Parciais/" , gsub( "\\..*" , "" , tolower( basename( this_file ) ) ) , "_" , this_year , ".fst" )
      
      # save data
      if ( !dir.exists( dirname( this_output_file ) ) ) { dir.create( dirname( this_output_file ) , recursive = TRUE ) }
      fst::write_fst( x , this_output_file , compress = 100 )
      
      # remove data.frame
      rm( x )
      
      # create results data.frame
      data.frame( year = this_year ,
                  full_url = this_url ,
                  fst_file = this_output_file ,
                  case_count = case_count ,
                  stringsAsFactors = FALSE)
      
    } , .parallel = TRUE , .paropts = list( .export = c( "catalog" , "remove_special_character" , "this_output_folder" , "this_year" , "this_url" ) ) )
    
    # stack these_results
    these_results <- do.call( rbind , these_results )
    
    # stack results
    full_results <- rbind( full_results , these_results )
    
  } else if ( catalog[ i , "year" ] == 2010 ) {
    
    # set variables for passing into plyr::llply
    this_output_folder <- catalog[ i, "output_folder" ]
    this_year <- catalog[ i, "year" ]
    this_url <- catalog[ i, "full_url" ]
    
    # process data
    these_results <- plyr::llply( these_data_files , function( this_file ) {
      
      # read data
      x <- readxl::read_xls( this_file )
      
      # force data.table
      x <- data.table::data.table( x , stringsAsFactors = FALSE )
      
      # fix names
      colnames( x ) <- tolower( colnames( x ) )
      colnames( x ) <- remove_special_character( colnames( x ) )
      
      # set up tablename
      this_tablename <- tolower( gsub( "_.*" , "" , basename( this_file ) ) )
      this_tablename <- paste( this_tablename , this_year , sep = "_" )
      
      # read into database
      # DBI::dbWriteTable( db , this_tablename , x , append = TRUE )
      
      # count cases
      case_count <- nrow(x)
      
      # define output file
      this_output_file <- paste0( this_output_folder , "Parciais/" , gsub( "\\..*" , "" , tolower( basename( this_file ) ) ) , "_" , this_year , ".fst" )
      
      # save data
      if ( !dir.exists( dirname( this_output_file ) ) ) { dir.create( dirname( this_output_file ) , recursive = TRUE ) }
      fst::write_fst( x , this_output_file , compress = 100 )
      
      # remove data.frame
      rm( x )
      
      # create results data.frame
      data.frame( year = this_year ,
                  full_url = this_url ,
                  fst_file = this_output_file ,
                  case_count = case_count ,
                  stringsAsFactors = FALSE )
      
    } , .parallel = TRUE , .paropts = list( .export = c( "catalog" , "remove_special_character" , "this_output_folder" , "this_year" , "this_url" ) ) )
    
    # stack these_results
    these_results <- do.call( rbind , these_results )
    
    # stack results
    full_results <- rbind( full_results , these_results )
    
  } else if ( catalog[ i , "year" ] == 2007 ) {
    
    # set variables for passing into plyr::llply
    this_output_folder <- catalog[ i, "output_folder" ]
    this_year <- catalog[ i, "year" ]
    this_url <- catalog[ i, "full_url" ]
    
    # process data
    these_results <- plyr::llply( these_data_files , function( this_file ) {
      
      # read data
      x <- readxl::read_xls( this_file )
      
      # skip empty files
      if ( nrow( x ) <= 0 ) { return( NULL ) }
      
      # force data.table
      x <- data.table::data.table( x , stringsAsFactors = FALSE )
      
      # fix names
      colnames( x ) <- tolower( colnames( x ) )
      colnames( x ) <- remove_special_character( colnames( x ) )
      
      # set up tablename
      this_tablename <- tolower( gsub( "_.*" , "" , basename( this_file ) ) )
      this_tablename <- paste( this_tablename , this_year , sep = "_" )
      
      # read into database
      # DBI::dbWriteTable( db , this_tablename , x , append = TRUE )
      
      # count cases
      case_count <- nrow(x)
      
      # define output file
      this_output_file <- paste0( this_output_folder , "Parciais/" , gsub( "\\..*" , "" , tolower( basename( this_file ) ) ) , "_" , this_year , ".fst" )
      
      # save data
      if ( !dir.exists( dirname( this_output_file ) ) ) { dir.create( dirname( this_output_file ) , recursive = TRUE ) }
      fst::write_fst( x , this_output_file , compress = 100 )
      
      # remove data.frame
      rm( x )
      
      # create results data.frame
      data.frame( year = this_year ,
                  full_url = this_url ,
                  fst_file = this_output_file ,
                  case_count = case_count ,
                  stringsAsFactors = FALSE )
      
    } , .parallel = TRUE , .paropts = list( .export = c( "catalog" , "remove_special_character" , "this_output_folder" , "this_year" , "this_url" ) ) )
    
    # stack these_results
    these_results <- do.call( rbind , these_results )
    
    # stack results
    full_results <- rbind( full_results , these_results )
    
  }
  
  # store max case count
  catalog[ i , "case_count" ] <- max( these_results$case_count )
  
  # delete temporary folder and files
  unlink( td , recursive = TRUE )
  file.remove( tf )
  
  # process tracker
  cat( paste0( "ascenso catalog entry " , i , " of " , nrow( catalog ) , " stored at '" , catalog[ i , "output_folder" ] , "'\r" ) )
  
  
  # if this is the final catalog entry for this year, then write them all to the database
  if ( i == max( which( catalog[ , "year" ] == catalog[ i , 'year' ] ) ) ) {
    
    # map files
    these_dirs <- catalog[ catalog[ , "year" ] == catalog[ i , "year" ] , "output_folder" ]
    these_dirs <- file.path( unique( these_dirs ) , "Parciais" )
    these_files <- list.files( these_dirs , full.names = TRUE , recursive = TRUE )
    
    # set tables
    if ( catalog[ i , "year" ] == 2000 ) {
      these_tables <- c(
        "basico" , "domicilio" , "morador" ,
        paste0( "instrucao" , 1:6 ) ,
        paste0( "pessoa" , 1:7 ) ,
        paste0( "responsavel" , 1:5 )
      )
    } else if ( catalog[ i , "year" ] == 2010 ) {
      these_tables <- c(
        "basico" ,
        table_num_pad( "pessoa" , 1:13 ) ,
        table_num_pad( "responsavel" , 1:2 ) ,
        table_num_pad( "entorno" , 1:5 ) ,
        table_num_pad( "domicilio" , 1:2 ) ,
        paste0( c("domicilio" , "pessoa" , "responsavel") , "renda")
      )
    } else if ( catalog[ i , "year" ] == 2007 ) {
      # these_tables <- c(
      #   "domicilio" ,
      #   paste0( "pessoap" , 1:4 ) ,
      #   table_num_pad( "responsavel" , 1:2 ) ,
      #   table_num_pad( "entorno" , 1:5 ) ,
      #   table_num_pad( "domicilio" , 1:2 ) ,
      #   paste0( c("domicilio" , "pessoa" , "responsavel") , "renda")
      # )
    }
    
    for ( this_table in these_tables ) {
      
      # list respective files
      table_files <- these_files [ grep( this_table , basename( these_files ) , ignore.case = TRUE )]
      
      # read data.tables in list
      dt_list <- lapply( table_files, function( this_file ) { fst::read_fst( this_file , as.data.table = TRUE ) } )
      
      # stack files
      this_dt <- rbindlist( dt_list , use.names = TRUE , fill = TRUE ) ; rm( dt_list )
      
      # fix wrong formats
      these_cols <- grep( "^(cod|nome)" , colnames( this_dt ) , value = TRUE )
      this_dt[ , (these_cols) := lapply( .SD , as.character ) , .SDcols = these_cols ]
      
      # save output
      this_table_file <- paste0( catalog[ i, "output_folder" ] , this_table , ".fst" )
      if ( !dir.exists( dirname( this_table_file ) ) ) { dir.create( dirname( this_table_file ) , recursive = TRUE ) }
      fst::write_fst( this_dt , this_table_file , compress = 100 )
      
      # # delete folder
      # unlink( paste0( catalog[ i, "output_folder" ] , "Parciais" ) , recursive = TRUE)
      
      # process tracker
      cat( this_table_file , "saved.\n" )
    }
  }
}

catalog

stopImplicitCluster()

q("no")
