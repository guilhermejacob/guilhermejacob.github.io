# load libraries
library(ggplot2)
library(maptools)
library(rgeos)
library(rgdal)
library(ggthemes)
library(data.table)
library(fst)
library(future.apply)

# plot parameters
ethnicities <- c( "White" , "Black" , "Yellow" , "Brown" , "Indigenous" )
ethnicities <- c( "Branca" , "Preta" , "Amarela" , "Parda" , "Indígena" )
pal <- c("#8dd3c7", "#ffffb3", "#fb8072", "#bebada", "#80b1d3" )
pal <- c("#ac39ac", "#00e600", "#fcec52", "#0099ff", "#ff5733" )
stopifnot( length(ethnicities) == length( pal ) ) 

# set parallelization configs
plan( multiprocess )

# set folders
mapsdir <- "/Volumes/Trabalho/Mapas/Setores Censitários/2010"
mapsdir <- "/home/guilherme/Bases/Mapas/Setores Censitários/2010"
datadir <- "/Volumes/Trabalho/ASC/Censo 2010/"
datadir <- "/home/guilherme/Bases/ASCenso/2010"

# set.seed
set.seed(123)

# read datafile
datafile <- list.files( datadir , full.names = TRUE , pattern = "fst" )
datafile <- datafile[ grepl( "^pessoa03" , basename( datafile ) , ignore.case = TRUE ) ]
select_cols <- c( "cod_setor" , paste0( "v00" , 2:6 ) )
dt <- lapply( datafile , read.fst , columns = select_cols , as.data.table = TRUE )
dt <- rbindlist( dt , use.names = TRUE)

# recodes
num_cols <- grep( "^v" , colnames( dt ) , value = TRUE )
dt[ , (num_cols) := lapply( .SD , as.numeric ) , .SDcols = num_cols ]
colnames( dt )[ colnames( dt ) %in% paste0( "v00" , 2:6 ) ] <- ethnicities

# list maps
these_mapfiles <- list.files( mapsdir , full.names = TRUE , recursive = TRUE , pattern = "shp" ) # list shapefiles in `mapsdir`
these_mapfiles <- these_mapfiles[ grepl( "SEE" , basename( these_mapfiles ) , ignore.case = TRUE ) ] # keep census tracts maps







### SP

# select maps
mapfile <- these_mapfiles[ grepl( "^35" , basename( these_mapfiles ) , ignore.case = TRUE ) ] # keep SP only

# read map
rgmap <- readOGR( dsn = mapfile , verbose = FALSE , stringsAsFactors = FALSE , encoding = "latin1" )

# fix column names
colnames( rgmap@data ) <- tolower( colnames( rgmap@data ) )

# keep São Paulo (city) only
rgmap <- subset( rgmap , cd_geocodm == '3550308' )

# keep urban areas only
# rgmap <- subset( rgmap , tipo == 'URBANO' )

# apply standardized projection
rgmap <- spTransform( rgmap , CRS("+proj=longlat +datum=WGS84") )


# subset data
# keep only census tracts in map
rgdata <- dt[ cod_setor %in% unique( rgmap$cd_geocodi ) , ]

# drop "parda"
# rgdata$parda <- 0

# merge map with data
mapdata <- merge( rgmap , rgdata , by.x = "cd_geocodi" , by.y = "cod_setor" , all.x = TRUE , all.y = FALSE )

# create neighborhood maps
munbmap <- unionSpatialPolygons( rgmap , IDs = rgmap$cd_geocodm )
blocmap <- unionSpatialPolygons( rgmap , IDs = rgmap$cd_geocodd )
# blocmap <- unionSpatialPolygons( rgmap , IDs = rgmap$cd_geocods )

# parallelized code to assign dots to polygons
rep.weight <- 125 # use this to calibrate representativeness in plot

sp.dfs <- future_lapply( names( mapdata@data[ , ethnicities ] ) , function( x ) { 
  mapdata@data[ is.na( mapdata@data[, x ] ) , x ] <- 0 
  if ( sum( as.integer( mapdata@data[ , x ] / rep.weight ) ) == 0 ) return(NULL)
  dotsInPolys( mapdata , as.integer( mapdata@data[ , x ] / rep.weight ), f="random" ) 
} )

# for each sp.df, scrape out the coordinates of each dot and store as a regular dataframe
dfs <- lapply(sp.dfs, function(x) {
  if( is.null( x ) ) { return(NULL ) }
  data.frame(coordinates(x)[,1:2])
})

# we're going to bind these dataframes together but first we need to add an ethnicity
# variable to allow for categorising data by colour after binding
# the double square brackets [[]] are used to select the dataframes held within the list
for (i in 1:length(ethnicities)) {
  dfs[[i]]$Ethnicity <- ethnicities[i]
}

# final bit of data prep: bind all dataframes into one then set factor levels
# the factor level will dictate the order in which dots are plotted
# we want the category with most dots to be plotted first and vice versa, 
# so that categories with the most dots don't mask categories with fewer dots
dots.final <- do.call( rbind , dfs)
dots.final$Ethnicity <- factor(dots.final$Ethnicity, levels = ethnicities )

# reorder factors
dots.final$Ethnicity <- factor(dots.final$Ethnicity, levels = ethnicities[ c(4,1,3,2,5)] )

# plot map!
spplot <- ggplot(mapdata) +
  geom_point(data = dots.final, aes(x, y, colour = Ethnicity), size = .125 , alpha = .25 ) +
  # geom_path(aes(long, lat, group = group), colour = "#d3d3d3") +
  geom_path(data = blocmap , aes(long, lat, group = group), colour = "#d3d3d3" , size = .6 , alpha = .1 ) +
  geom_path(data = munbmap , aes(long, lat, group = group), colour = "#d3d3d3" , size = .6 , alpha = .5 ) +
  scale_colour_manual(values = pal) +
  guides( color = guide_legend( title = "Cor/Raça" , override.aes = list( size = 1 , alpha = 1 ) ) ) +
  theme_map() +
  labs( 
    title = "Ethnic composition of São Paulo (city)" ,
    subtitle = paste0("Each point represents approx. " , rep.weight , " individuals" ) ,
    caption = "Source: Aggregates from Brazil 2010 Census Tracts." ) +
  theme( plot.background = element_rect(fill = "black") ,
         plot.title = element_text( color = "white" , size = 20 , family = "Times" ) ,
         plot.subtitle = element_text( color = "white" , size = 15 , family = "Times" ) ,
         plot.caption = element_text( color = "white" , size = 10 , hjust = 1 , family = "Times" ) ,
         legend.position = "right" , 
         legend.background = element_rect( fill = "transparent" , colour = "transparent" ) ,
         legend.key = element_rect( fill = "transparent" , color = "transparent" ) ,
         legend.title = element_text( color = "white" , size = 15 , face = "bold" , family = "Times" ) ,
         legend.text = element_text( color = "white" , size = 13 , family = "Times" ) ) +
  coord_map()




### RJ

# select maps
mapfile <- these_mapfiles[ grepl( "^33" , basename( these_mapfiles ) , ignore.case = TRUE ) ] # keep SP only

# read map
rgmap <- readOGR( dsn = mapfile , verbose = FALSE , stringsAsFactors = FALSE , encoding = "latin1" )

# fix column names
colnames( rgmap@data ) <- tolower( colnames( rgmap@data ) )

# keep Rio de Janeiro (city) only
# rgmap <- subset( rgmap , cd_geocodm %in% c( '3304557' ) )
rgmap <- subset( rgmap , cd_geocodm %in% c( '3304557' , '3303302' ) ) # And Niterói

# keep urban areas only
# rgmap <- subset( rgmap , tipo == 'URBANO' )

# apply standardized projection
rgmap <- spTransform( rgmap , CRS("+proj=longlat +datum=WGS84") )


# subset data
# keep only census tracts in map
rgdata <- dt[ cod_setor %in% rgmap$cd_geocodi ,]

# drop "parda"
# rgdata$parda <- 0

# merge map with data
mapdata <- merge( rgmap , rgdata , by.x = "cd_geocodi" , by.y = "cod_setor" , all.x = TRUE , all.y = FALSE )

# create neighborhood maps
munbmap <- unionSpatialPolygons( rgmap , IDs = rgmap$cd_geocodm )
blocmap <- unionSpatialPolygons( rgmap , IDs = rgmap$cd_geocodd )
# blocmap <- unionSpatialPolygons( rgmap , IDs = rgmap$cd_geocods )

# parallelized code to assign dots to polygons
rep.weight <- 100 # use this to calibrate representativeness in plot

sp.dfs <- 
  mclapply( 
    names( mapdata@data[ , ethnicities ] ) , 
    function( x ) { 
      mapdata@data[ is.na( mapdata@data[, x ] ) , x ] <- 0 
      if ( sum( as.integer( mapdata@data[ , x ] / rep.weight ) ) == 0 ) { return(NULL) }
      dotsInPolys( mapdata , as.integer( mapdata@data[ , x ] / rep.weight ), f="random" ) } ,
    mc.set.seed = TRUE, mc.silent = FALSE , mc.cleanup = TRUE )

# for each sp.df, scrape out the coordinates of each dot and store as a regular dataframe
dfs <- lapply(sp.dfs, function(x) {
  if( is.null( x ) ) { return(NULL ) }
  data.frame(coordinates(x)[,1:2])
})

# we're going to bind these dataframes together but first we need to add an ethnicity
# variable to allow for categorising data by colour after binding
# the double square brackets [[]] are used to select the dataframes held within the list
for (i in 1:length(ethnicities)) { dfs[[i]]$Ethnicity <- ethnicities[i] }

# final bit of data prep: bind all dataframes into one then set factor levels
# the factor level will dictate the order in which dots are plotted
# we want the category with most dots to be plotted first and vice versa, 
# so that categories with the most dots don't mask categories with fewer dots
dots.final <- dplyr::bind_rows(dfs)
dots.final$Ethnicity <- factor(dots.final$Ethnicity, levels = ethnicities )

# reorder factors
dots.final$Ethnicity <- factor(dots.final$Ethnicity, levels = ethnicities[ c(4,1,3,2,5)] )

# plot map!
rjplot <- ggplot(mapdata) +
  geom_point(data = dots.final, aes(x, y, colour = Ethnicity), size = .125 , alpha = .2 ) +
  # geom_path(aes(long, lat, group = group), colour = "#d3d3d3") +
  geom_path(data = blocmap , aes(long, lat, group = group), colour = "#d3d3d3" , size = .6 , alpha = .1 ) +
  geom_path(data = munbmap , aes(long, lat, group = group), colour = "#d3d3d3" , size = .6 , alpha = .5 ) +
  scale_colour_manual(values = pal) +
  guides( color = guide_legend( title = "Cor/Raça" , override.aes = list( size = 1 , alpha = 1 ) ) ) +
  theme_map() +
  labs( 
    title = "Ethnic composition of Rio de Janeiro (city) and Niterói" ,
    subtitle = paste0("Each point represents approx. " , rep.weight , " individuals" ) ,
    caption = "Source: Aggregates from Brazil 2010 Census Tracts." ) +
  theme( plot.background = element_rect(fill = "black") ,
         plot.title = element_text( color = "white" , size = 20 , family = "Times" ) ,
         plot.subtitle = element_text( color = "white" , size = 15 , family = "Times" ) ,
         plot.caption = element_text( color = "white" , size = 10 , hjust = 1 , family = "Times" ) ,
         legend.position = "right" , 
         legend.background = element_rect( fill = "transparent" , colour = "transparent" ) ,
         legend.key = element_rect( fill = "transparent" , color = "transparent" ) ,
         legend.title = element_text( color = "white" , size = 15 , face = "bold" , family = "Times" ) ,
         legend.text = element_text( color = "white" , size = 13 , family = "Times" ) ) +
  coord_map()




### BSB

# select maps
mapfile <- these_mapfiles[ grepl( "^53" , basename( these_mapfiles ) , ignore.case = TRUE ) ] 

# read map
rgmap <- readOGR( dsn = mapfile , verbose = FALSE , stringsAsFactors = FALSE , encoding = "latin1" )

# fix column names
colnames( rgmap@data ) <- tolower( colnames( rgmap@data ) )

# keep urban areas only
# rgmap <- subset( rgmap , tipo == 'URBANO' )

# apply standardized projection
rgmap <- spTransform( rgmap , CRS("+proj=longlat +datum=WGS84") )

# subset data
# keep only census tracts in map
rgdata <- dt[ cod_setor %in% rgmap$cd_geocodi ,]

# merge map with data
mapdata <- merge( rgmap , rgdata , by.x = "cd_geocodi" , by.y = "cod_setor" , all.x = TRUE , all.y = FALSE )

# create neighborhood maps
munbmap <- unionSpatialPolygons( mapdata , IDs = mapdata$cd_geocodm )
blocmapA <- unionSpatialPolygons( mapdata , IDs = mapdata$cd_geocodd )
blocmapB <- unionSpatialPolygons( mapdata , IDs = mapdata$cd_geocodi )

# parallelized code to assign dots to polygons
rep.weight <- 100 # use this to calibrate representativeness in plot

sp.dfs <- 
  mclapply( 
    names( mapdata@data[ , ethnicities ] ) , 
    function( x ) { 
      mapdata@data[ is.na( mapdata@data[, x ] ) , x ] <- 0 
      if ( sum( as.integer( mapdata@data[ , x ] / rep.weight ) ) == 0 ) { return(NULL) }
      dotsInPolys( mapdata , as.integer( mapdata@data[ , x ] / rep.weight ), f="random" ) } ,
    mc.set.seed = TRUE, mc.silent = FALSE , mc.cleanup = TRUE )

# for each sp.df, scrape out the coordinates of each dot and store as a regular dataframe
dfs <- lapply(sp.dfs, function(x) {
  if( is.null( x ) ) { return(NULL ) }
  data.frame(coordinates(x)[,1:2])
})

# we're going to bind these dataframes together but first we need to add an ethnicity
# variable to allow for categorising data by colour after binding
# the double square brackets [[]] are used to select the dataframes held within the list
for (i in 1:length(ethnicities)) {
  dfs[[i]]$Ethnicity <- ethnicities[i]
}

# final bit of data prep: bind all dataframes into one then set factor levels
# the factor level will dictate the order in which dots are plotted
# we want the category with most dots to be plotted first and vice versa, 
# so that categories with the most dots don't mask categories with fewer dots
dots.final <- dplyr::bind_rows(dfs)
dots.final$Ethnicity <- factor(dots.final$Ethnicity, levels = ethnicities )

# reorder factors
dots.final$Ethnicity <- factor(dots.final$Ethnicity, levels = ethnicities[ c(4,1,3,2,5)] )

# plot map!
bsbplot <- ggplot(mapdata) +
  geom_point(data = dots.final, aes(x, y, colour = Ethnicity), size = .125 , alpha = .2 ) +
  # geom_path(aes(long, lat, group = group), colour = "#d3d3d3") +
  geom_path(data = munbmap , aes(long, lat, group = group), colour = "#d3d3d3" , size = .6 , alpha = .5 ) +
  geom_path(data = blocmapA , aes(long, lat, group = group), colour = "#d3d3d3" , size = .6 , alpha = .2 ) +
  geom_path(data = blocmapB , aes(long, lat, group = group), colour = "#d3d3d3" , size = .6 , alpha = .075 ) +
  scale_colour_manual(values = pal) +
  guides( color = guide_legend( title = "Cor/Raça" , override.aes = list( size = 1 , alpha = 1 ) ) ) +
  theme_map() +
  labs( 
    title = "Ethnic composition of Brasília" ,
    subtitle = paste0("Each point represents approx. " , rep.weight , " individuals" ) ,
    caption = "Source: Aggregates from Brazil 2010 Census Tracts." ) +
  theme( plot.background = element_rect(fill = "black") ,
         plot.title = element_text( color = "white" , size = 20 , family = "Times" ) ,
         plot.subtitle = element_text( color = "white" , size = 15 , family = "Times" ) ,
         plot.caption = element_text( color = "white" , size = 10 , hjust = 1 , family = "Times" ) ,
         legend.position = "right" , 
         legend.background = element_rect( fill = "transparent" , colour = "transparent" ) ,
         legend.key = element_rect( fill = "transparent" , color = "transparent" ) ,
         legend.title = element_text( color = "white" , size = 15 , face = "bold" , family = "Times" ) , 
         legend.text = element_text( color = "white" , size = 13 , family = "Times" ) ) +
  coord_map()

# clean workspace
rm( list = c( "munbmap", "blocmap" , "dfs", "sp.dfs", "rgdata", "rgmap" , "dots.final", "mapdata" ) ) ; gc()




### REC

# select maps
mapfile <- these_mapfiles[ grepl( "^26" , basename( these_mapfiles ) , ignore.case = TRUE ) ] 

# read map
rgmap <- readOGR( dsn = mapfile , verbose = FALSE , stringsAsFactors = FALSE , encoding = "latin1" )

# fix column names
colnames( rgmap@data ) <- tolower( colnames( rgmap@data ) )

# keep Recife only
# rgmap <- subset( rgmap , cd_geocodm %in% c( '2611606' ) ) 
rgmap <- subset( rgmap , cd_geocodm %in% c( '2611606' , "2607901" , "2609600" ) ) # 3 largest cities Metropolitan Region

# apply standardized projection
rgmap <- spTransform( rgmap , CRS("+proj=longlat +datum=WGS84") )


# subset data
# keep only census tracts in map
rgdata <- dt[ cod_setor %in% rgmap$cd_geocodi ,]

# merge map with data
mapdata <- merge( rgmap , rgdata , by.x = "cd_geocodi" , by.y = "cod_setor" , all.x = TRUE , all.y = FALSE )

# create neighborhood maps
munbmap <- unionSpatialPolygons( rgmap , IDs = rgmap$cd_geocodm )
blocmap <- unionSpatialPolygons( rgmap , IDs = rgmap$cd_geocodd )
# blocmap <- unionSpatialPolygons( rgmap , IDs = rgmap$cd_geocods )

# parallelized code to assign dots to polygons
rep.weight <- 60 # use this to calibrate representativeness in plot

sp.dfs <- 
  mclapply( 
    names( mapdata@data[ , ethnicities ] ) , 
    function( x ) { 
      mapdata@data[ is.na( mapdata@data[, x ] ) , x ] <- 0 
      if ( sum( as.integer( mapdata@data[ , x ] / rep.weight ) ) == 0 ) { return(NULL) }
      dotsInPolys( mapdata , as.integer( mapdata@data[ , x ] / rep.weight ), f="random" ) } ,
    mc.set.seed = TRUE, mc.silent = FALSE , mc.cleanup = TRUE )

# for each sp.df, scrape out the coordinates of each dot and store as a regular dataframe
dfs <- lapply(sp.dfs, function(x) {
  if( is.null( x ) ) { return(NULL ) }
  data.frame(coordinates(x)[,1:2])
})

# we're going to bind these dataframes together but first we need to add an ethnicity
# variable to allow for categorising data by colour after binding
# the double square brackets [[]] are used to select the dataframes held within the list
for (i in 1:length(ethnicities)) {
  dfs[[i]]$Ethnicity <- ethnicities[i]
}

# final bit of data prep: bind all dataframes into one then set factor levels
# the factor level will dictate the order in which dots are plotted
# we want the category with most dots to be plotted first and vice versa, 
# so that categories with the most dots don't mask categories with fewer dots
dots.final <- dplyr::bind_rows(dfs)
dots.final$Ethnicity <- factor(dots.final$Ethnicity, levels = ethnicities )

# reorder factors
dots.final$Ethnicity <- factor(dots.final$Ethnicity, levels = ethnicities[ c(4,1,3,2,5)] )

# plot map!
recplot <- ggplot(mapdata) +
  geom_point(data = dots.final, aes(x, y, colour = Ethnicity), size = .125 , alpha = .2 ) +
  # geom_path(aes(long, lat, group = group), colour = "#d3d3d3") +
  geom_path(data = blocmap , aes(long, lat, group = group), colour = "#d3d3d3" , size = .6 , alpha = .1 ) +
  geom_path(data = munbmap , aes(long, lat, group = group), colour = "#d3d3d3" , size = .6 , alpha = .5 ) +
  scale_colour_manual(values = pal) +
  guides( color = guide_legend( title = "Cor/Raça" , override.aes = list( size = 1 , alpha = 1 ) ) ) +
  theme_map() +
  labs( 
    title = "Ethnic composition of Recife, Jaboatão dos Guararapes, and Olinda" ,
    subtitle = paste0("Each point represents approx. " , rep.weight , " individuals" ) ,
    caption = "Source: Aggregates from Brazil 2010 Census Tracts." ) +
  theme( plot.background = element_rect(fill = "black") ,
         plot.title = element_text( color = "white" , size = 20 , family = "Times" ) ,
         plot.subtitle = element_text( color = "white" , size = 15 , family = "Times" ) ,
         plot.caption = element_text( color = "white" , size = 10 , hjust = 1 , family = "Times" ) ,
         legend.position = "right" , 
         legend.background = element_rect( fill = "transparent" , colour = "transparent" ) ,
         legend.key = element_rect( fill = "transparent" , color = "transparent" ) ,
         legend.title = element_text( color = "white" , size = 15 , face = "bold" , family = "Times" ) , 
         legend.text = element_text( color = "white" , size = 13 , family = "Times" ) ) +
  coord_map()

# clean workspace
rm( list = c( "munbmap", "blocmap" , "dfs", "sp.dfs", "rgdata", "rgmap" , "dots.final", "mapdata" ) ) ; gc()




### SAL

# select maps
mapfile <- these_mapfiles[ grepl( "^29" , basename( these_mapfiles ) , ignore.case = TRUE ) ] 

# read map
rgmap <- readOGR( dsn = mapfile , verbose = FALSE , stringsAsFactors = FALSE , encoding = "latin1" )

# fix column names
colnames( rgmap@data ) <- tolower( colnames( rgmap@data ) )

# keep Salvador only
rgmap <- subset( rgmap , cd_geocodm %in% c( "2927408" ) ) # Salvador
# rgmap <- subset( rgmap , nm_micro %in% "SALVADOR" ) # microrregião

# apply standardized projection
rgmap <- spTransform( rgmap , CRS("+proj=longlat +datum=WGS84") )

# subset data
# keep only census tracts in map
rgdata <- dt[ cod_setor %in% rgmap$cd_geocodi ,]

# merge map with data
mapdata <- merge( rgmap , rgdata , by.x = "cd_geocodi" , by.y = "cod_setor" , all.x = TRUE , all.y = FALSE )

# create neighborhood maps
munbmap <- unionSpatialPolygons( rgmap , IDs = rgmap$cd_geocodm )
blocmap <- unionSpatialPolygons( rgmap , IDs = rgmap$cd_geocodd )
# blocmap <- unionSpatialPolygons( rgmap , IDs = rgmap$cd_geocods )

# parallelized code to assign dots to polygons
rep.weight <- 80 # use this to calibrate representativeness in plot

sp.dfs <- 
  mclapply( 
    names( mapdata@data[ , ethnicities ] ) , 
    function( x ) { 
      mapdata@data[ is.na( mapdata@data[, x ] ) , x ] <- 0 
      if ( sum( as.integer( mapdata@data[ , x ] / rep.weight ) ) == 0 ) { return(NULL) }
      dotsInPolys( mapdata , as.integer( mapdata@data[ , x ] / rep.weight ), f="random" ) } ,
    mc.set.seed = TRUE, mc.silent = FALSE , mc.cleanup = TRUE )

# for each sp.df, scrape out the coordinates of each dot and store as a regular dataframe
dfs <- lapply(sp.dfs, function(x) {
  if( is.null( x ) ) { return(NULL ) }
  data.frame(coordinates(x)[,1:2])
})

# we're going to bind these dataframes together but first we need to add an ethnicity
# variable to allow for categorising data by colour after binding
# the double square brackets [[]] are used to select the dataframes held within the list
for (i in 1:length(ethnicities)) {
  dfs[[i]]$Ethnicity <- ethnicities[i]
}

# final bit of data prep: bind all dataframes into one then set factor levels
# the factor level will dictate the order in which dots are plotted
# we want the category with most dots to be plotted first and vice versa, 
# so that categories with the most dots don't mask categories with fewer dots
dots.final <- dplyr::bind_rows(dfs)
dots.final$Ethnicity <- factor(dots.final$Ethnicity, levels = ethnicities )

# reorder factors
dots.final$Ethnicity <- factor(dots.final$Ethnicity, levels = ethnicities[ c(4,1,3,2,5)] )

# plot map!
salplot <- ggplot(mapdata) +
  geom_point(data = dots.final, aes(x, y, colour = Ethnicity), size = .125 , alpha = .25 ) +
  # geom_path(aes(long, lat, group = group), colour = "#d3d3d3") +
  geom_path(data = blocmap , aes(long, lat, group = group), colour = "#d3d3d3" , size = .6 , alpha = .25 ) +
  geom_path(data = munbmap , aes(long, lat, group = group), colour = "#d3d3d3" , size = .6 , alpha = .5 ) +
  scale_colour_manual(values = pal) +
  guides( color = guide_legend( title = "Cor/Raça" , override.aes = list( size = 1 , alpha = 1 ) ) ) +
  theme_map() +
  labs( 
    title = "Ethnic composition of Salvador" ,
    subtitle = paste0("Each point represents approx. " , rep.weight , " individuals" ) ,
    caption = "Source: Aggregates from Brazil 2010 Census Tracts." ) +
  theme( plot.background = element_rect(fill = "black") ,
         plot.title = element_text( color = "white" , size = 20 , family = "Times" ) ,
         plot.subtitle = element_text( color = "white" , size = 15 , family = "Times" ) ,
         plot.caption = element_text( color = "white" , size = 10 , hjust = 1 , family = "Times" ) ,
         legend.position = "right" , 
         legend.background = element_rect( fill = "transparent" , colour = "transparent" ) ,
         legend.key = element_rect( fill = "transparent" , color = "transparent" ) ,
         legend.title = element_text( color = "white" , size = 15 , face = "bold" , family = "Times" ) ,
         legend.text = element_text( color = "white" , size = 13 , family = "Times" ) ) +
  coord_map()




### MAO

# select maps
mapfile <- these_mapfiles[ grepl( "^13" , basename( these_mapfiles ) , ignore.case = TRUE ) ] 

# read map
rgmap <- readOGR( dsn = mapfile , verbose = FALSE , stringsAsFactors = FALSE , encoding = "latin1" )

# fix column names
colnames( rgmap@data ) <- tolower( colnames( rgmap@data ) )

# keep Manaus only
rgmap <- subset( rgmap , cd_geocodm == '1302603' )

# keep urban areas only
rgmap <- subset( rgmap , tipo == 'URBANO' )

# apply standardized projection
rgmap <- spTransform( rgmap , CRS("+proj=longlat +datum=WGS84") )

# subset data
# keep only census tracts in map
rgdata <- dt[ cod_setor %in% rgmap$cd_geocodi ,]

# merge map with data
mapdata <- merge( rgmap , rgdata , by.x = "cd_geocodi" , by.y = "cod_setor" , all.x = TRUE , all.y = FALSE )

# create neighborhood maps
munbmap <- unionSpatialPolygons( rgmap , IDs = rgmap$cd_geocodm )
blocmapA <- unionSpatialPolygons( rgmap , IDs = rgmap$cd_geocodb )
blocmapB <- unionSpatialPolygons( rgmap , IDs = rgmap$cd_geocodi )

# parallelized code to assign dots to polygons
rep.weight <- 100 # use this to calibrate representativeness in plot

sp.dfs <- 
  mclapply( 
    names( mapdata@data[ , ethnicities ] ) , 
    function( x ) { 
      mapdata@data[ is.na( mapdata@data[, x ] ) , x ] <- 0 
      if ( sum( as.integer( mapdata@data[ , x ] / rep.weight ) ) == 0 ) { return(NULL) }
      dotsInPolys( mapdata , as.integer( mapdata@data[ , x ] / rep.weight ), f="random" ) } ,
    mc.set.seed = TRUE, mc.silent = FALSE , mc.cleanup = TRUE )

# for each sp.df, scrape out the coordinates of each dot and store as a regular dataframe
dfs <- lapply(sp.dfs, function(x) {
  if( is.null( x ) ) { return(NULL ) }
  data.frame(coordinates(x)[,1:2])
})

# we're going to bind these dataframes together but first we need to add an ethnicity
# variable to allow for categorising data by colour after binding
# the double square brackets [[]] are used to select the dataframes held within the list
for (i in 1:length(ethnicities)) {
  dfs[[i]]$Ethnicity <- ethnicities[i]
}

# final bit of data prep: bind all dataframes into one then set factor levels
# the factor level will dictate the order in which dots are plotted
# we want the category with most dots to be plotted first and vice versa, 
# so that categories with the most dots don't mask categories with fewer dots
dots.final <- dplyr::bind_rows(dfs)
dots.final$Ethnicity <- factor(dots.final$Ethnicity, levels = ethnicities )

# reorder factors
dots.final$Ethnicity <- factor(dots.final$Ethnicity, levels = ethnicities[ c(4,1,3,2,5)] )

# plot map!
maoplot <- ggplot(mapdata) +
  geom_point(data = dots.final, aes(x, y, colour = Ethnicity), size = .125 , alpha = .2 ) +
  # geom_path(aes(long, lat, group = group), colour = "#d3d3d3") +
  geom_path(data = munbmap , aes(long, lat, group = group), colour = "#d3d3d3" , size = .6 , alpha = .5 ) +
  geom_path(data = blocmapA , aes(long, lat, group = group), colour = "#d3d3d3" , size = .6 , alpha = .2 ) +
  geom_path(data = blocmapB , aes(long, lat, group = group), colour = "#d3d3d3" , size = .6 , alpha = .075 ) +
  scale_colour_manual(values = pal) +
  guides( color = guide_legend( title = "Cor/Raça" , override.aes = list( size = 1 , alpha = 1 ) ) ) +
  theme_map() +
  labs( 
    title = "Ethnic composition of Manaus' urban areas" ,
    subtitle = paste0("Each point represents approx. " , rep.weight , " individuals" ) ,
    caption = "Source: Aggregates from Brazil 2010 Census Tracts." ) +
  theme( plot.background = element_rect(fill = "black") ,
         plot.title = element_text( color = "white" , size = 20 , family = "Times" ) ,
         plot.subtitle = element_text( color = "white" , size = 15 , family = "Times" ) ,
         plot.caption = element_text( color = "white" , size = 10 , hjust = 1 , family = "Times" ) ,
         legend.position = "right" , 
         legend.background = element_rect( fill = "transparent" , colour = "transparent" ) ,
         legend.key = element_rect( fill = "transparent" , color = "transparent" ) ,
         legend.title = element_text( color = "white" , size = 15 , face = "bold" , family = "Times" ) , 
         legend.text = element_text( color = "white" , size = 13 , family = "Times" ) ) +
  coord_map()




# stop cluster
stopCluster(cl)

##### save maps
plotlist <- grep( "plot$" , ls() , value = TRUE )
lapply( plotlist , function( this_plot ) {
  ggsave( normalizePath( paste0( "~/Documents/GitHub/website-source/static/img/ethnic_" , this_plot , ".pdf" ) , mustWork = FALSE ) , 
          get( this_plot ) , 
          width = 14 , height = 14 , 
          dpi = 300 )
  cat( this_plot , "saved\n")
} )

##### converts pdf to png
for ( this_plot in plotlist ) {
  
  this_pdf <- normalizePath( paste0( "~/Documents/GitHub/website-source/static/img/ethnic_" , this_plot , ".pdf" ) , mustWork = FALSE )
  these_png_files <- paste0( gsub( "\\.pdf$" , "" , this_pdf ) , c("_highres" , "_lowres" ) , ".png" )
  
  pdftools::pdf_convert( pdf = this_pdf , filenames = these_png_files[1] , dpi = 300 ) 
  pdftools::pdf_convert( pdf = this_pdf , filenames = these_png_files[2] , dpi = 100 )
  
}
