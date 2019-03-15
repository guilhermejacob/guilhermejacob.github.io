# results folder
res_folder <- path.expand( "~/Documents/PNADC/Tabelas" )

library(DBI)

# db <- dbConnect( RSQLite::SQLite() , file.path( "/Volumes/Trabalho" , "PNAD Contínua" , "pnadc.db" ) , flags = RSQLite::SQLITE_RO )
db <- dbConnect( RSQLite::SQLite() , file.path( "~/Documents" , "PNADC" , "pnadc.db" ) , flags = RSQLite::SQLITE_RO )

pnad_list <- dbListTables( db )
# pnad_list <- grep( "03$" , pnad_list , ignore.case = TRUE , value = TRUE )
# pnad_list <- pnad_list [ seq( 1 , length(pnad_list) , 2 ) ]
# pnad_list <- pnad_list [ c( 1 , length(pnad_list) ) ]
pnad_list <- pnad_list [ c( length(pnad_list) ) ]

# this_table <- pnad_list [ 1 ]

library(survey)
library(convey)

options( survey.lonely.psu = "adjust" )

for ( this_table in pnad_list ) {
  
  # timestamp
  this_year <- substr( gsub( "pnadc" , "" , basename( this_table ) , ignore.case = TRUE ) , 1 , 4 )
  this_quarter <- substr( gsub( "pnadc" , "" , basename( this_table ) , ignore.case = TRUE ) , 5 , 6 )
  
  this_output_file <- paste0( res_folder , "/Trabalho/trab_v1_" , this_year , this_quarter , ".Rds" )
  if ( file.exists( this_output_file ) ) next()
  
  # read data
  # pnadc_df <- dbReadTable( db , this_table )
  these_vars <- grep( "uf|vd400|v2009|v2010|v1022|vd401|upa|v102|estrato|posest", dbListFields( db , this_table ) , ignore.case = TRUE , value = TRUE )
  pnadc_df <- dbGetQuery( db , paste0( "SELECT ", paste0( these_vars , collapse = ", " ) , " FROM " , this_table ) )
  
  # add a column of all ones
  pnadc_df$one <- 1
  
  # construct a data.frame object with all state names.
  uf <-
    structure(list(V1 = c(11L, 12L, 13L, 14L, 15L, 16L, 17L, 21L, 
                          22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L, 31L, 32L, 33L, 35L, 41L, 
                          42L, 43L, 50L, 51L, 52L, 53L), V2 = structure(c(22L, 1L, 4L, 
                                                                          23L, 14L, 3L, 27L, 10L, 18L, 6L, 20L, 15L, 17L, 2L, 26L, 5L, 
                                                                          13L, 8L, 19L, 25L, 16L, 24L, 21L, 12L, 11L, 9L, 7L), .Label = c("Acre", 
                                                                                                                                          "Alagoas", "Amapa", "Amazonas", "Bahia", "Ceara", "Distrito Federal", 
                                                                                                                                          "Espirito Santo", "Goias", "Maranhao", "Mato Grosso", "Mato Grosso do Sul", 
                                                                                                                                          "Minas Gerais", "Para", "Paraiba", "Parana", "Pernambuco", "Piaui", 
                                                                                                                                          "Rio de Janeiro", "Rio Grande do Norte", "Rio Grande do Sul", 
                                                                                                                                          "Rondonia", "Roraima", "Santa Catarina", "Sao Paulo", "Sergipe", 
                                                                                                                                          "Tocantins"), class = "factor")), .Names = c("uf", "uf_name"), 
              class = "data.frame", row.names = c(NA, -27L))
  
  # merge this data.frame onto the main `x` data.frame
  # using `uf` as the merge field, keeping all non-matches.
  pnadc_df$uf_name <- factor(pnadc_df$uf , levels = uf$uf , labels = uf$uf_name )
  
  # add a column of all ones
  pnadc_df$one <- 1
  
  # add a column of country variable
  pnadc_df$brasil <- "brasil"
  
  # add a column of region variable
  pnadc_df$regiao <- factor( substr( pnadc_df$uf , 1 , 1 ) , 1:5 , labels = c( "norte" , "nordeste" , "sudeste" , "sul" , "centro-oeste" ) )
  
  # specific variables
  
  # create age breaks: 
  pnadc_df$age_break <- cut( pnadc_df$v2009 , breaks = c( 14, 16 , 18 , seq( 20 , 50 , 10 ) , Inf ) , right = FALSE )
  
  # create work status 1: employed, unemployed, and under-employed
  pnadc_df$work_status1 <- 
    ifelse( pnadc_df$vd4001 == 1 , 
            ifelse( pnadc_df$vd4002 == 1 , 
                    ifelse( !is.na( pnadc_df$vd4004 ) , "subocupado" , "ocupado" ) ,  "desocupado" ) , NA )
  
  # create work status 2
  pnadc_df$work_status2 <- 
    ifelse( pnadc_df$vd4001 == 1 , 
            ifelse( pnadc_df$vd4002 == 1 , 
                    ifelse( !is.na( pnadc_df$vd4004 ) , "subocupado" , "ocupado" ) ,  "desocupado" ) , 
            ifelse( pnadc_df$vd4003 == 1 , ifelse( !is.na( pnadc_df$vd4005 ) , "desalentado" , "frç. t. pot." ) , NA ) )
  
  # create work status 3: carteira assinada, estatutário e informalidade
  pnadc_df$work_status3 <- 
    ifelse( !is.na(pnadc_df$vd4009) ,
            ifelse( pnadc_df$vd4009 %in% c(1,3,5) , "carteira assinada" , 
                    ifelse( pnadc_df$vd4009 %in% c(7) , "serviço público (militar/estatutário)" , 
                            ifelse( pnadc_df$vd4009 %in% c(2,4,6) , "sem carteira assinada" , 
                                    ifelse( pnadc_df$vd4009 %in% c(8) , "empregador" , 
                                            ifelse( pnadc_df$vd4009 %in% c(8) , "conta-própria" , 
                                                    ifelse( pnadc_df$vd4009 %in% c(10) , "trabalhador auxiliar familiar" , NA ) ) ) ) ) ) , NA )
  
  
  # confirm complete matches
  stopifnot( all( !is.na( pnadc_df$uf_name ) ) )
  
  # preliminary survey design
  pre_stratified <-
    svydesign(
      ids = ~ upa , 
      strata = ~ estrato , 
      weights = ~ v1027 , 
      data = pnadc_df ,
      nest = TRUE
    )
  # warning: do not use `pre_stratified` in your analyses!
  # you must use the `pnadc_design` object created below.
  
  # post-stratification targets
  df_pos <- data.frame( posest = unique( pnadc_df$posest ) , Freq = unique( pnadc_df$v1029 ) )
  
  # final survey design object
  pnadc_design <- postStratify( pre_stratified , ~ posest , df_pos )
  
  # create convey survey design object
  pnadc_design <- convey_prep( pnadc_design )
  
  # remove the `pnadc_df` data.frame object
  # and the `pre_stratified` design before stratification
  rm( pnadc_df , pre_stratified , uf )
  
  # create variables
  
  # presets
  these_variables <- c( "work_status1" , "work_status2" , "work_status3" )
  # these_variables <- "vd4004a"
  # these_categories <- c( "brasil" , "regiao" , "uf_name" )
  these_categories <- c( "brasil" , "regiao" , "age_break" )
  # these_categories <- c( "brasil" , "age_break" )
  x <- expand.grid( year = this_year , quarter = this_quarter , variable = these_variables , category = these_categories , stringsAsFactors = TRUE )
  
  # calculate summaries
  res.prop <- plyr::mlply( 
    x , 
    function( year , quarter , variable , category ) { 
      res <- svyby( as.formula( paste0( "~factor(" , variable , ")" ) ) , 
                    as.formula( paste0( "~" , category ) ) , 
                    pnadc_design , 
                    svymean , 
                    na.rm = TRUE )
      # if ( colnames(res)[1] %in% c( "brasil" , "regiao" , "uf_name" ) ) colnames(res)[1] <- "area"
      colnames(res)[1] <- "category"
      rownames(res) <- NULL
      return(res)
    } ,
    .expand = TRUE ,
    .parallel = TRUE , 
    .progress = "none" )
  res.prop <- 
    plyr::llply( these_variables , 
                 function ( this_variable ) { 
                   do.call( 
                     rbind , 
                     res.prop[ which( attr( res.prop , "split_labels" )$variable %in% this_variable ) ]
                   ) 
                 } )
  
  
  res.totals <- plyr::mlply( 
    x[ , -5 ] , 
    function( year , quarter , variable , category ) { 
      res <- svyby( as.formula( paste0( "~factor(" , variable , ")" ) ) , 
                    as.formula( paste0( "~" , category ) ) , 
                    pnadc_design , 
                    svytotal , 
                    na.rm = TRUE )
      # if ( colnames(res)[1] %in% c( "brasil" , "regiao" , "uf_name" ) ) colnames(res)[1] <- "area"
      colnames(res)[1] <- "category"
      rownames(res) <- NULL
      return(res)
    } ,
    .expand = TRUE ,
    .parallel = TRUE , 
    .progress = "none" )
  res.totals <- 
    plyr::llply( these_variables , 
                 function ( this_variable ) { 
                   do.call( 
                     rbind , 
                     res.totals[ which( attr( res.totals , "split_labels" )$variable %in% this_variable ) ] 
                   ) 
                 } )
  
  
  # these_categories <- c( "brasil" , "regiao" , "vd4008" )
  these_categories <- c( "brasil" , "regiao" , "work_status3" )
  these_categories <- c( "brasil" , "work_status3" )
  x <- expand.grid( year = this_year , quarter = this_quarter , category = these_categories , stringsAsFactors = TRUE )
  x$post_cat <- ifelse( x$category %in% c( "brasil" , "regiao" , "uf_name" ) , "area" , x$category )
  res.gini <- plyr::mlply( 
    x[ , 1:3 ] , 
    function( year , quarter , category ) { 
      res <- svyby( ~vd4019 , 
                    as.formula( paste0( "~" , category ) ) , 
                    subset( pnadc_design , vd4019 > 0 ) , 
                    svygini , 
                    na.rm = TRUE )
      # if ( colnames(res)[1] %in% c( "brasil" , "regiao" , "uf_name" ) ) colnames(res)[1] <- "area"
      colnames(res)[1] <- "category"
      rownames(res) <- NULL
      return(res)
    } ,
    .expand = TRUE ,
    .parallel = TRUE , 
    .progress = "none" )
  res.gini <- do.call( rbind , res.gini )
  
  full_res <- list( year = this_year , quarter = this_quarter , props = res.prop , totals = res.totals , ginis = res.gini )
  
  if ( !dir.exists( dirname( this_output_file ) ) ) dir.create( dirname( this_output_file ) , recursive = TRUE )
  saveRDS( full_res , this_output_file )
  
  cat( this_year , this_quarter , "results stored at" , this_output_file )
  
}

confint(full_set[1][[1]][[3]][[1]])