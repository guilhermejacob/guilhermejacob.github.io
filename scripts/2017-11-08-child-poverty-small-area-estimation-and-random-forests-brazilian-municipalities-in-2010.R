## ----setup_paths , message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE----
# Step 1: check if the files are there.
these_files <- c( file.path( getwd() , "static/datasets/BR2010_PovEsts.Rdata" ) )
these_scripts <- c( file.path( getwd() , "content/scripts/BR2010_child_poverty_rates.R" ) )
these_file_exists <- sapply( these_files , function(x) file.exists( x ) , USE.NAMES = FALSE , simplify = TRUE )
# Step 2: if not, run respective code.
if ( any( !these_file_exists ) ) {
  for ( j in seq_along( these_file_exists ) ) {
    if ( these_file_exists [j] == FALSE ) {
      cat( basename( these_files[ j ] ) , "not found\nrunning" , basename( these_scripts[ j ] ) , "\r" )
      # source( these_scripts[ j ] ) # this takes forever!
      cat( "running" , basename( these_scripts[ j ] ) , "\tdone!\n" )
    }
  }
}

## ----cv_plot , echo=FALSE, message=FALSE, warning=FALSE , cache = TRUE----
library(survey)
load( file.path( "~/Documents/GitHub/website-source" , "static/datasets/BR2010_PovEsts.Rdata" ) )
gini_data <- data.frame( 
  codmun7 = as.character(gini_list[,1]) , 
  gini_est = coef(gini_list) ,
  gini_var = (SE(gini_list))^2 ,
  ci_l = confint(gini_list)[ , 1 ] , 
  ci_u = confint(gini_list)[ , 2 ] , 
  gini_cv = cv( gini_list ) , 
  stringsAsFactors = FALSE )


library(ggplot2)
library(gridExtra)
library(grid)
library(scales)
library(ggiraph)

gini_data <- gini_data[ order( -gini_data$gini_est ) , ]
gini_data$codmun7 <- factor( gini_data$codmun7 , levels = unique( gini_data$codmun7 ) )

dist_plot <- ggplot( rbind( head( gini_data , 20 ) , tail( gini_data , 20 ) ) , aes( codmun7, gini_est , color = gini_cv ) ) +
  geom_point( stat = "identity" , size = .5 ) +
  geom_errorbar( aes( ymin=ci_l, ymax=ci_u ) , width=.1 , alpha = .5 ) +
  # geom_histogram( stat = "identity" , color = "transparent" ) + 
  scale_y_continuous( expand = c(0,0) , breaks = seq( 0, .9, .1 ) , labels = sprintf("%0.3f", seq( 0, .9, .1 ) ) ) +
  coord_cartesian(ylim=c( 0 , 1) ) +
  scale_color_continuous( low = "blue" , high = "red" ) +
  labs( x = "Municipalities" , y = "Gini Index" , 
        title = "Gini index estimates and 95% CIs" ,
        subtitle ="40 municipalities of with the largest or lowest estimates." , 
        caption = "Source: Brazil 2010 Census Sample. Microdata." ) +
  guides( fill = FALSE , color = FALSE ) +
  theme_dark() +
  theme( axis.text.x=element_blank(), axis.ticks.x=element_blank() , 
         panel.background = element_rect( fill = "white" ) , 
         panel.grid.minor.y = element_blank() ,
         panel.grid.minor.x = element_blank() ,
         panel.grid.major.x = element_blank() , 
         plot.title = element_text(lineheight=.8, face="bold") ) 

gini_data <- gini_data[ order( -gini_data$gini_cv ) , ]
gini_data$codmun7 <- factor( gini_data$codmun7 , levels = unique( gini_data$codmun7 ) )

# nobs <- 100
# ggplot( rbind( head( gini_data , nobs ) , tail( gini_data , nobs ) ), aes( codmun7, gini_cv , fill = gini_cv ) ) +
cv_plot <- ggplot( gini_data , aes( codmun7, gini_cv , fill = gini_cv ) ) +
  geom_bar( stat = "identity" , color = "transparent" ) +
  # geom_histogram( stat = "identity" , color = "transparent" ) + 
  scale_y_continuous( labels = scales::percent , expand = c(0,0) ) +
  coord_cartesian(ylim=c(0 , .275) ) +
  scale_fill_continuous( low = "blue" , high = "red" ) +
  labs( x = "Municipalities" , y = "Coefficient of Variation" , 
        title = "Gini Index" ,
        subtitle ="For each 5565 municipalities in the Census." , 
        caption = "Source: Brazil 2010 Census Sample. Microdata." ) +
  theme_dark() +
  theme( axis.text.x=element_blank(), axis.ticks.x=element_blank() , 
         panel.background = element_rect( fill = "white" ) , 
         panel.grid.minor.x = element_blank() ,
         panel.grid.major.x = element_blank() , 
         plot.title = element_text(lineheight=.8, face="bold") ) 

ggiraph( code = {print(dist_plot)} , zoom_max = 5 , width = 1 , tooltip_opacity = .7 , width_svg = 8, height_svg= 4 )


## ----gini_map_initial , echo=FALSE, message=FALSE, warning=FALSE  , cache = TRUE----
library(maptools)
library(rgeos)
library(rgdal)
library(Cairo)
library(ggmap)
library(scales)
library(RColorBrewer)

# get municipal map from 2010:
mapfolder <- "/Volumes/Trabalho/Mapas/2010"
if ( !dir.exists( mapfolder ) ) {
  all.maps.cpct <- list.files( "/Volumes/DataVault/GeoIBGE/geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_2010" , recursive = TRUE , full.names = TRUE )
  all.maps.cpct <- grep( "zip$" , all.maps.cpct , ignore.case = TRUE , value = TRUE )
  for ( this_zip in all.maps.cpct ) {
    unzip( this_zip , exdir = mapfolder )
  }
}

# read and stack maps:
map_list <- list.files( mapfolder , pattern = "shp" , full.names = TRUE )
map_list <- grep( "MUE" , map_list , value = TRUE , ignore.case = TRUE )
# map_list <- map_list[ grep( "^(1|21|51)" , basename( map_list ) , ignore.case = TRUE ) ] # force subset
# map_list <- map_list[ grep( "^15" , basename( map_list ) , ignore.case = TRUE ) ] # force subset
map_list <- parallel::mclapply( map_list , function( this_map ) { readOGR( this_map , verbose = FALSE , stringsAsFactors = FALSE ) } )
mapobj <- do.call( rbind , map_list )
rm( map_list , gini_list )

# standardize names
names(mapobj) <- tolower( names(mapobj) )
mapobj$nm_municip <- iconv( mapobj$nm_municip , from = "latin1" , to = "utf8" )
mapobj$nm_municip <- stringi::stri_trans_totitle( mapobj$nm_municip )

# fortify data
mapobj.f <- fortify ( mapobj , region = "cd_geocodm" )

# create high_cv identifier
gini_data$high_cv <- factor( 1*( gini_data$gini_cv > .05 ) , labels = c( "baixo" , "alto" ) )

# merge two datasets and create final plot data
gini_data$codmun7 <- as.character( gini_data$codmun7 )
final.data <- merge( mapobj.f , gini_data , by.x = "id" , by.y = "codmun7" , all.x = TRUE , all.y = FALSE )
final.data <- final.data[ order( final.data$order ) , ]

# plot map
gini_map_init <- 
  ggplot ( final.data , aes( x = long, y = lat, group = group , fill = gini_est , alpha = high_cv ) ) + 
  geom_polygon( color = "transparent", size = 0.25 ) + 
  coord_map() +
  scale_fill_continuous( name="Gini Index", low = "blue" , high = "red" , limits = c( .2 , 1 ) ) +
  scale_alpha_discrete( range = c( .5 , 1 ) ) +
  # scale_alpha_discrete( range = c( 1 , .5 ) ) +
  guides( alpha = FALSE ) +
  theme_dark() +
  theme( panel.background = element_rect( fill = "white" ) , 
         axis.text=element_blank(), 
         axis.ticks=element_blank() , 
         axis.title = element_blank() ,
         panel.grid = element_blank() ,
         plot.title = element_text(lineheight=.8, face="bold") ) +
  labs( title = "Gini Index Choropleth Map" ,
        subtitle ="Low-precision areas are highlighted." , 
        caption = "Source: Brazil 2010 Census Sample. Microdata." )

# ggsave( normalizePath( file.path( "~/Documents/GitHub/website-source" , "static/img/gini_map_init.pdf" ) , mustWork = FALSE ) , gini_map_init , width = 15 , height = 20 )
ggsave( normalizePath( file.path( "~/Documents/GitHub/website-source" , "static/img/gini_map_init.png" ) , mustWork = FALSE ) , gini_map_init , width = 15 , height = 20 , dpi = 100 )

## ----train_rf, echo=FALSE, message=FALSE, warning=FALSE , cache = TRUE----
rm( gini_map_init )

# if IDHM dataset doesn't exist, download it:
idhm.mun_path <- file.path( "~/Documents/GitHub/website-source/" , "static/datasets/MunIDHM.Rds" )
idhm.mun_path <- normalizePath( idhm.mun_path , mustWork = FALSE )
if ( !file.exists( idhm.mun_path ) ){
  tf <- tempfile()
  download.file( "http://www.atlasbrasil.org.br/2013/data/rawData/atlas2013_dadosbrutos_pt.xlsx" , tf )
  
  idhm.mun <- readxl::read_xlsx( tf , sheet = 2 )
  
  saveRDS( idhm.mun , file = idhm.mun_path , compress = TRUE )
  
} else {
  idhm.mun <- readRDS( idhm.mun_path )
}

# standardize column names
colnames(idhm.mun) <- tolower(colnames(idhm.mun))
colnames(idhm.mun) <- iconv( colnames(idhm.mun) , to = "ASCII" , sub = "" )
idhm.mun[, grep( "cod" , colnames(idhm.mun) ) ] <- apply( idhm.mun[, grep( "cod" , colnames(idhm.mun) ) ] , 2 , as.character )

# subset for related data
idhm.mun <- subset( idhm.mun , ano == 2010 )

# drop unused columns
idhm.mun <- idhm.mun[ , !grepl( "codmun6|munic|gini|idhm|peso|^mulh|^homem|^t_fund|^ren|trab|pob|^rdpc[0-9]|^esp" , colnames(idhm.mun) ) ]

# merge datasets
idhm.mun <- merge( idhm.mun , gini_data[ , c( "codmun7" , "gini_est" , "high_cv" , "gini_var" ) ] , by = "codmun7" )

# get high-cv obs
high_cv <- idhm.mun$high_cv
idhm.mun$high_cv <- NULL

# get saompling variance estimates
smp_var <- idhm.mun$gini_var
idhm.mun$gini_var <- NULL

# set seed of random processes
set.seed(123)

# prepare training and test dataset
library(caret)
inTrain <- createDataPartition( as.factor( idhm.mun$uf ) , p = .6 , list = FALSE )

# remove high_cv from inTrain
# inTrain <- inTrain[ inTrain %in% seq_along(high_cv)[ high_cv == "alto" ] ]

# train Random Forests
library(randomForest)
sae.rf <- randomForest(gini_est ~ ., data = idhm.mun[ inTrain, ], keep.inbag= TRUE , sampsize=100 , replace=TRUE , ntree=800 )

# predict
gini.result <- predict( sae.rf, idhm.mun[ -inTrain, ], type="response", predict.all=FALSE, proximity=FALSE )

# mean squared error
cat( "MSE:" , round( sqrt( mean( ( gini.result - idhm.mun[ -inTrain, "gini_est" ] )^2 ) ) , 5 ) , "\n" )

# median absolute deviation
cat( "MAD:" , round( median( abs( gini.result - idhm.mun[ -inTrain, "gini_est" ] ) ) , 5 ) , "\n" )

# get variance estimation
# library(RFinfer)
# var.results <- rfPredVar( sae.rf, idhm.mun[ inTrain, ] , pred.data = idhm.mun[ -inTrain, ],  CI = FALSE, tree.type = "rf", prog.bar = TRUE )
# sqrt(var.results$pred.ij.var)

library(randomForestCI)
rfm_var <- randomForestInfJack(sae.rf , idhm.mun , calibrate = TRUE)
# sqrt( rfm_var[ , 2]) / rfm_var[ , 1]

# balance estimates
gamma_est <- rfm_var[ , 2 ] / ( smp_var + rfm_var[ , 2 ] )

# get final estimates
gini_fh <- gamma_est*idhm.mun$gini_est + (1 - gamma_est) * rfm_var[ , 1 ]
gini_fh_var <- gamma_est * idhm.mun$gini_est + (1 - gamma_est)^2 * rfm_var[ , 2 ]

# save them to a dataset
gini_fh <- data.frame( codmun7 = idhm.mun$codmun7 , gini_smp = idhm.mun$gini_est , gini_rfm = rfm_var[ , 1 ] , gini_fh = gini_fh , gini_fh_var = gini_fh_var )


## ----gini_map_final , echo=FALSE, message=FALSE, warning=FALSE , cache = TRUE----
# create high_cv identifier
gini_fh$high_cv <- factor( 1*( ( sqrt(gini_fh$gini_fh_var) / gini_fh$gini_fh ) > .05 ) , labels = c( "baixo" , "alto" ) )

# merge two datasets and create final plot data
final.data <- merge( mapobj.f , gini_fh , by.x = "id" , by.y = "codmun7" , all.x = TRUE , all.y = FALSE )
final.data <- final.data[ order( final.data$order ) , ]

# plot map
gini_map_final <- 
  ggplot ( final.data , aes( x = long, y = lat, group = group , fill = gini_fh ) ) + 
  geom_polygon( color = "transparent", size = 0.25 ) + 
  coord_map() +
  scale_fill_continuous( name="Gini Index", low = "blue" , high = "red" , limits = c( .2 , 1 ) ) +
  guides( alpha = FALSE ) +
  theme_dark() +
  theme( panel.background = element_rect( fill = "white" ) , 
         axis.text=element_blank(), 
         axis.ticks=element_blank() , 
         axis.title = element_blank() ,
         panel.grid = element_blank() ,
         plot.title = element_text(lineheight=.8, face="bold") ) +
  labs( title = "Gini Index Choropleth Map" ,
        subtitle ="Results from a Random Forest Small Area Estimation model." , 
        caption = "Source: Brazil 2010 Census Sample. Microdata." )

# ggsave( normalizePath( file.path( "~/Documents/GitHub/website-source" , "static/img/gini_map_final.pdf" ) , mustWork = FALSE ) , gini_map_final , width = 15 , height = 20 )
ggsave( normalizePath( file.path( "~/Documents/GitHub/website-source" , "static/img/gini_map_final.png" ) , mustWork = FALSE ) , gini_map_final , width = 15 , height = 20 , dpi = 100 )

