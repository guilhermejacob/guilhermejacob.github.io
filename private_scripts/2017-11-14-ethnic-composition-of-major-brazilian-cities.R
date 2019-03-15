## ----post_setup, message=FALSE, warning=FALSE, include=FALSE-------------
mapsdir <- "/Volumes/Trabalho/Mapas/Setores Censitários/2010"
datadir <- "/Volumes/Trabalho/ASCenso/Censos/2010"

## ----mapa_sp, echo=TRUE, fig.height=12, fig.width=10, message=FALSE, warning=FALSE, cache=FALSE----
# load maps

# load libraries
library(ggplot2)
library(maptools)
library(rgeos)
library(rgdal)
library(ggthemes)
# library(ggiraph)

# list and select maps
mapfile <- list.files( mapsdir , full.names = TRUE , recursive = TRUE , pattern = "shp" ) # list shapefiles in `mapsdir`
mapfile <- mapfile[ grepl( "SEE" , basename( mapfile ) , ignore.case = TRUE ) ] # keep census tracts maps
mapfile <- mapfile[ grepl( "^35" , basename( mapfile ) , ignore.case = TRUE ) ] # keep SP only

# read map
spmap <- readOGR( dsn = mapfile , verbose = FALSE , stringsAsFactors = FALSE , encoding = "latin1" )

# fix column names
colnames( spmap@data ) <- tolower( colnames( spmap@data ) )

# keep São Paulo (city) only
spmap <- subset( spmap , cd_geocodm == '3550308' )

# keep urban areas only
# spmap <- subset( spmap , tipo == 'URBANO' )

# apply standardized projection
spmap <- spTransform( spmap , CRS("+proj=longlat +datum=WGS84") )



# load data
datafile <- list.files( datadir , full.names = TRUE , recursive = TRUE , pattern = "Rds" ) # list csv in `datadir`
datafile <- datafile[ grepl( "^pessoa03" , basename( datafile ) , ignore.case = TRUE ) ] # keep "pessoa03" table only
datafile <- datafile[ grepl( "_sp" , basename( datafile ) , ignore.case = TRUE ) ] # keep SP only

# read and stack data
spdata <- lapply( datafile , function( x ) readRDS( x )  )
spdata <- do.call( rbind , spdata )

# fix column names
colnames( spdata ) <- tolower( colnames( spdata ) )

# fix column formats
spdata[ , grep( "^cod|setor" , colnames( spdata ) ) ] <- apply( spdata[ , grep( "^cod|setor" , colnames( spdata ) ) ] , 2 , as.character )
spdata[ , grep( "^v" , colnames( spdata ) ) ] <- apply( spdata[ , grep( "^v" , colnames( spdata ) ) ], 2 , function( x ) suppressWarnings( as.numeric( x ) ) )

# set names
# ethnicities <- c( "branca" , "preta" , "amarela" , "parda" , "indígena" )
ethnicities <- c( "Branca" , "Preta" , "Amarela" , "Parda" , "Indígena" )
colnames( spdata )[ colnames( spdata ) %in% paste0( "v00" , 2:6 ) ] <- ethnicities

# drop "parda"
# spdata$parda <- 0

# keep only census tracts in map
spdata <- spdata[ spdata$cod_setor %in% spmap$cd_geocodi ,]


# merge map with data
mapdata <- merge( spmap , spdata , by.x = "cd_geocodi" , by.y = "cod_setor" , all.x = TRUE , all.y = FALSE )

# create neighborhood maps
munbmap <- unionSpatialPolygons( spmap , IDs = spmap$cd_geocodm )
blocmap <- unionSpatialPolygons( spmap , IDs = spmap$cd_geocodd )
# blocmap <- unionSpatialPolygons( spmap , IDs = spmap$cd_geocods )

# parallelized code to assign dots to polygons
library(parallel)

set.seed(123)

rep.weight <- 125 # use this to calibrate representativeness in plot

sp.dfs <- 
  mclapply( 
    names( mapdata@data[ , ethnicities ] ) , 
    function( x ) { 
      mapdata@data[ is.na( mapdata@data[, x ] ) , x ] <- 0 ; 
      if( sum( as.integer( mapdata@data[ , x ] / rep.weight ) ) == 0 ) {return(NULL)} ; 
      dotsInPolys( mapdata , as.integer( mapdata@data[ , x ] / rep.weight ), f="random" ) } , 
    mc.set.seed = TRUE, mc.silent = FALSE , mc.cleanup = TRUE )

# define a palette
pal <- c("#8dd3c7", "#ffffb3", "#fb8072", "#bebada", "#80b1d3" )
stopifnot( length(ethnicities) == length( pal ) ) 

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
    title = "Ethnical composition of São Paulo (city)" ,
    subtitle = paste0("Each point represents approx. " , rep.weight , " individuals" ) ,
    caption = "Source: Aggregates from Brazil 2010 Census Tracts." ) +
  theme( plot.background = element_rect(fill = "black") ,
         plot.title = element_text( color = "white" , size = 20 ) ,
         plot.subtitle = element_text( color = "white" , size = 15 ) ,
         plot.caption = element_text( color = "white" , size = 10 , hjust = 1 ) ,
         legend.position = "right" , 
         legend.background = element_rect( fill = "transparent" , colour = "transparent" ) ,
         legend.key = element_rect( fill = "transparent" , color = "transparent" ) ,
         legend.title = element_text( color = "white" , size = 15 , face = "bold" ) ,
         legend.text = element_text( color = "white" , size = 13 ) ) +
  coord_map()

## ----mapa_rj, echo=FALSE , fig.height=12, fig.width=20, message=FALSE, warning=FALSE, cache = FALSE----
# load maps

# load libraries
library(ggplot2)
library(maptools)
library(rgeos)
library(rgdal)
library(ggthemes)
# library(ggiraph)

# list and select maps
mapfile <- list.files( mapsdir , full.names = TRUE , recursive = TRUE , pattern = "shp" ) # list shapefiles in `mapsdir`
mapfile <- mapfile[ grepl( "SEE" , basename( mapfile ) , ignore.case = TRUE ) ] # keep census tracts maps
mapfile <- mapfile[ grepl( "^33" , basename( mapfile ) , ignore.case = TRUE ) ] # keep RJ only

# read map
rjmap <- readOGR( dsn = mapfile , verbose = FALSE , stringsAsFactors = FALSE , encoding = "latin1" )

# fix column names
colnames( rjmap@data ) <- tolower( colnames( rjmap@data ) )

# keep Rio de Janeiro (city) only
# rjmap <- subset( rjmap , cd_geocodm %in% c( '3304557' ) )
rjmap <- subset( rjmap , cd_geocodm %in% c( '3304557' , '3303302' ) ) # And Niterói

# keep urban areas only
rjmap <- subset( rjmap , tipo == 'URBANO' )

# apply standardized projection
rjmap <- spTransform( rjmap , CRS("+proj=longlat +datum=WGS84") )



# load data
datafile <- list.files( datadir , full.names = TRUE , recursive = TRUE , pattern = "Rds" ) # list csv in `datadir`
datafile <- datafile[ grepl( "^pessoa03" , basename( datafile ) , ignore.case = TRUE ) ] # keep "pessoa03" table only
datafile <- datafile[ grepl( "_rj" , basename( datafile ) , ignore.case = TRUE ) ] # keep RJ only

# read and stack data
rjdata <- lapply( datafile , function( x ) readRds( x )  )
rjdata <- do.call( rbind , rjdata )

# fix column names
colnames( rjdata ) <- tolower( colnames( rjdata ) )

# fix column formats
rjdata[ , grep( "^cod|setor" , colnames( rjdata ) ) ] <- apply( rjdata[ , grep( "^cod|setor" , colnames( rjdata ) ) ] , 2 , as.character )
rjdata[ , grep( "^v" , colnames( rjdata ) ) ] <- apply( rjdata[ , grep( "^v" , colnames( rjdata ) ) ], 2 , function( x ) suppressWarnings( as.numeric( x ) ) )

# set names
# ethnicities <- c( "branca" , "preta" , "amarela" , "parda" , "indígena" )
ethnicities <- c( "Branca" , "Preta" , "Amarela" , "Parda" , "Indígena" )
colnames( rjdata )[ colnames( rjdata ) %in% paste0( "v00" , 2:6 ) ] <- ethnicities

# drop "parda"
# rjdata$parda <- 0

# keep only census tracts in map
rjdata <- rjdata[ rjdata$cod_setor %in% rjmap$cd_geocodi ,]


# merge map with data
mapdata <- merge( rjmap , rjdata , by.x = "cd_geocodi" , by.y = "cod_setor" , all.x = TRUE , all.y = FALSE )

# create neighborhood maps
munbmap <- unionSpatialPolygons( rjmap , IDs = rjmap$cd_geocodm )
blocmap <- unionSpatialPolygons( rjmap , IDs = rjmap$cd_geocodb )
# blocmap <- unionSpatialPolygons( rjmap , IDs = rjmap$cd_geocodd )
# blocmap <- unionSpatialPolygons( rjmap , IDs = rjmap$cd_geocods )

# parallelized code to assign dots to polygons
library(parallel)

set.seed(123)

rep.weight <- 80 # use this to calibrate representativeness in plot

sp.dfs <- 
  mclapply( 
    names( mapdata@data[ , ethnicities ] ) , 
    function( x ) { 
      mapdata@data[ is.na( mapdata@data[, x ] ) , x ] <- 0 ; 
      if( sum( as.integer( mapdata@data[ , x ] / rep.weight ) ) == 0 ) {return(NULL)} ; 
      dotsInPolys( mapdata , as.integer( mapdata@data[ , x ] / rep.weight ), f="random" ) } , 
    mc.set.seed = TRUE, mc.silent = FALSE , mc.cleanup = TRUE )

# define a palette
pal <- c("#8dd3c7", "#ffffb3", "#fb8072", "#bebada", "#80b1d3" )
stopifnot( length(ethnicities) == length( pal ) ) 

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

# plot map!
rjplot <- ggplot(mapdata) +
  geom_point(data = dots.final, aes(x, y, colour = Ethnicity), size = .125 , alpha = .25 ) +
  # geom_path(aes(long, lat, group = group), colour = "#d3d3d3") +
  geom_path(data = blocmap , aes(long, lat, group = group), colour = "#d3d3d3" , size = .6 , alpha = .1 ) +
  geom_path(data = munbmap , aes(long, lat, group = group), colour = "#d3d3d3" , size = .6 , alpha = .5 ) +
  scale_colour_manual(values = pal) +
  guides( color = guide_legend( title = "Cor/Raça" , override.aes = list( size = 1 , alpha = 1 ) ) ) +
  theme_map() +
  labs( 
    title = "Ethnical composition of Rio de Janeiro (city) and Niterói" ,
    subtitle = paste0("Each point represents approx. " , rep.weight , " individuals" ) ,
    caption = "Source: Aggregates from Brazil 2010 Census Tracts." ) +
  theme( plot.background = element_rect(fill = "black") ,
         plot.title = element_text( color = "white" , size = 20 ) ,
         plot.subtitle = element_text( color = "white" , size = 15 ) ,
         plot.caption = element_text( color = "white" , size = 10 , hjust = 1 ) ,
         legend.position = "right" , 
         legend.background = element_rect( fill = "transparent" , colour = "transparent" ) ,
         legend.key = element_rect( fill = "transparent" , color = "transparent" ) ,
         legend.title = element_text( color = "white" , size = 15 , face = "bold" ) ,
         legend.text = element_text( color = "white" , size = 13 ) ) +
  coord_map()


## ----mapa_bsb, echo=FALSE , fig.height=12, fig.width=20, message=FALSE, warning=FALSE, cache = FALSE----
# load maps

# load libraries
library(ggplot2)
library(maptools)
library(rgeos)
library(rgdal)
library(ggthemes)
# library(ggiraph)

# list and select maps
mapfile <- list.files( mapsdir , full.names = TRUE , recursive = TRUE , pattern = "shp" ) # list shapefiles in `mapsdir`
mapfile <- mapfile[ grepl( "SEE" , basename( mapfile ) , ignore.case = TRUE ) ] # keep census tracts maps
mapfile <- mapfile[ grepl( "^53" , basename( mapfile ) , ignore.case = TRUE ) ] # keep RJ only

# read map
bsbmap <- readOGR( dsn = mapfile , verbose = FALSE , stringsAsFactors = FALSE , encoding = "latin1" )

# fix column names
colnames( bsbmap@data ) <- tolower( colnames( bsbmap@data ) )

# keep urban areas only
# bsbmap <- subset( bsbmap , tipo == 'URBANO' )

# apply standardized projection
bsbmap <- spTransform( bsbmap , CRS("+proj=longlat +datum=WGS84") )



# load data
datafile <- list.files( datadir , full.names = TRUE , recursive = TRUE , pattern = "Rds" ) # list csv in `datadir`
datafile <- datafile[ grepl( "^pessoa03" , basename( datafile ) , ignore.case = TRUE ) ] # keep "pessoa03" table only
datafile <- datafile[ grepl( "_df" , basename( datafile ) , ignore.case = TRUE ) ] # keep DF only

# read and stack data
bsbdata <- lapply( datafile , function( x ) readRds( x )  )
bsbdata <- do.call( rbind , bsbdata )

# fix column names
colnames( bsbdata ) <- tolower( colnames( bsbdata ) )

# fix column formats
bsbdata[ , grep( "^cod|setor" , colnames( bsbdata ) ) ] <- apply( bsbdata[ , grep( "^cod|setor" , colnames( bsbdata ) ) ] , 2 , as.character )
bsbdata[ , grep( "^v" , colnames( bsbdata ) ) ] <- apply( bsbdata[ , grep( "^v" , colnames( bsbdata ) ) ], 2 , function( x ) suppressWarnings( as.numeric( x ) ) )

# set names
# ethnicities <- c( "branca" , "preta" , "amarela" , "parda" , "indígena" )
ethnicities <- c( "Branca" , "Preta" , "Amarela" , "Parda" , "Indígena" )
colnames( bsbdata )[ colnames( bsbdata ) %in% paste0( "v00" , 2:6 ) ] <- ethnicities

# drop "parda"
# bsbdata$parda <- 0

# keep only census tracts in map
bsbdata <- bsbdata[ bsbdata$cod_setor %in% bsbmap$cd_geocodi ,]


# merge map with data
mapdata <- merge( bsbmap , bsbdata , by.x = "cd_geocodi" , by.y = "cod_setor" , all.x = TRUE , all.y = FALSE )

# create neighborhood maps
# create neighborhood maps
munbmap <- unionSpatialPolygons( bsbmap , IDs = bsbmap$cd_geocodm )
blocmapA <- unionSpatialPolygons( bsbmap , IDs = bsbmap$cd_geocodd )
blocmapB <- unionSpatialPolygons( bsbmap , IDs = bsbmap$cd_geocodi )

# parallelized code to assign dots to polygons
library(parallel)

set.seed(123)

rep.weight <- 50 # use this to calibrate representativeness in plot

sp.dfs <- 
  mclapply( 
    names( mapdata@data[ , ethnicities ] ) , 
    function( x ) { 
      mapdata@data[ is.na( mapdata@data[, x ] ) , x ] <- 0 ; 
      if( sum( as.integer( mapdata@data[ , x ] / rep.weight ) ) == 0 ) {return(NULL)} ; 
      dotsInPolys( mapdata , as.integer( mapdata@data[ , x ] / rep.weight ), f="random" ) } , 
    mc.set.seed = TRUE, mc.silent = FALSE , mc.cleanup = TRUE )

# define a palette
pal <- c("#8dd3c7", "#ffffb3", "#fb8072", "#bebada", "#80b1d3" )
stopifnot( length(ethnicities) == length( pal ) ) 

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

# plot map!
bsbplot <- ggplot(mapdata) +
  geom_point(data = dots.final, aes(x, y, colour = Ethnicity), size = .125 , alpha = .25 ) +
  # geom_path(aes(long, lat, group = group), colour = "#d3d3d3") +
  geom_path(data = munbmap , aes(long, lat, group = group), colour = "#d3d3d3" , size = .6 , alpha = .5 ) +
  geom_path(data = blocmapA , aes(long, lat, group = group), colour = "#d3d3d3" , size = .6 , alpha = .2 ) +
  geom_path(data = blocmapB , aes(long, lat, group = group), colour = "#d3d3d3" , size = .6 , alpha = .075 ) +
  scale_colour_manual(values = pal) +
  guides( color = guide_legend( title = "Cor/Raça" , override.aes = list( size = 1 , alpha = 1 ) ) ) +
  theme_map() +
  labs( 
    title = "Ethnical composition of Brasília" ,
    subtitle = paste0("Each point represents approx. " , rep.weight , " individuals" ) ,
    caption = "Source: Aggregates from Brazil 2010 Census Tracts." ) +
  theme( plot.background = element_rect(fill = "black") ,
         plot.title = element_text( color = "white" , size = 20 ) ,
         plot.subtitle = element_text( color = "white" , size = 15 ) ,
         plot.caption = element_text( color = "white" , size = 10 , hjust = 1 ) ,
         legend.position = "right" , 
         legend.background = element_rect( fill = "transparent" , colour = "transparent" ) ,
         legend.key = element_rect( fill = "transparent" , color = "transparent" ) ,
         legend.title = element_text( color = "white" , size = 15 , face = "bold" ) ,
         legend.text = element_text( color = "white" , size = 13 ) ) +
  coord_map()

## ----mapa_rec, echo=FALSE , fig.height=15, fig.width=20, message=FALSE, warning=FALSE, cache = FALSE----
# load maps

# load libraries
library(ggplot2)
library(maptools)
library(rgeos)
library(rgdal)
library(ggthemes)
# library(ggiraph)

# list and select maps
mapfile <- list.files( mapsdir , full.names = TRUE , recursive = TRUE , pattern = "shp" ) # list shapefiles in `mapsdir`
mapfile <- mapfile[ grepl( "SEE" , basename( mapfile ) , ignore.case = TRUE ) ] # keep census tracts maps
mapfile <- mapfile[ grepl( "^26" , basename( mapfile ) , ignore.case = TRUE ) ] # keep RJ only

# read map
recmap <- readOGR( dsn = mapfile , verbose = FALSE , stringsAsFactors = FALSE , encoding = "latin1" )

# fix column names
colnames( recmap@data ) <- tolower( colnames( recmap@data ) )

# keep Recife only
# recmap <- subset( recmap , cd_geocodm %in% c( '2611606' ) ) 
recmap <- subset( recmap , cd_geocodm %in% c( '2611606' , "2607901" , "2609600" ) ) # 3 largest cities Metropolitan Region

# keep urban areas only
# recmap <- subset( recmap , tipo == 'URBANO' )

# apply standardized projection
recmap <- spTransform( recmap , CRS("+proj=longlat +datum=WGS84") )



# load data
datafile <- list.files( datadir , full.names = TRUE , recursive = TRUE , pattern = "Rds" ) # list csv in `datadir`
datafile <- datafile[ grepl( "^pessoa03" , basename( datafile ) , ignore.case = TRUE ) ] # keep "pessoa03" table only
datafile <- datafile[ grepl( "_pe" , basename( datafile ) , ignore.case = TRUE ) ] # keep PE only

# read and stack data
recdata <- lapply( datafile , function( x ) readRds( x )  )
recdata <- do.call( rbind , recdata )

# fix column names
colnames( recdata ) <- tolower( colnames( recdata ) )

# fix column formats
recdata[ , grep( "^cod|setor" , colnames( recdata ) ) ] <- apply( recdata[ , grep( "^cod|setor" , colnames( recdata ) ) ] , 2 , as.character )
recdata[ , grep( "^v" , colnames( recdata ) ) ] <- apply( recdata[ , grep( "^v" , colnames( recdata ) ) ], 2 , function( x ) suppressWarnings( as.numeric( x ) ) )

# set names
# ethnicities <- c( "branca" , "preta" , "amarela" , "parda" , "indígena" )
ethnicities <- c( "Branca" , "Preta" , "Amarela" , "Parda" , "Indígena" )
colnames( recdata )[ colnames( recdata ) %in% paste0( "v00" , 2:6 ) ] <- ethnicities

# drop "parda"
# recdata$parda <- 0

# keep only census tracts in map
recdata <- recdata[ recdata$cod_setor %in% recmap$cd_geocodi ,]


# merge map with data
mapdata <- merge( recmap , recdata , by.x = "cd_geocodi" , by.y = "cod_setor" , all.x = TRUE , all.y = FALSE )

# create neighborhood maps
munbmap <- unionSpatialPolygons( recmap , IDs = recmap$cd_geocodm )
# blocmap <- unionSpatialPolygons( recmap , IDs = recmap$cd_geocodb )
blocmap <- unionSpatialPolygons( recmap , IDs = recmap$cd_geocodd )
# blocmap <- unionSpatialPolygons( recmap , IDs = recmap$cd_geocods )

# parallelized code to assign dots to polygons
library(parallel)

set.seed(123)

rep.weight <- 40 # use this to calibrate representativeness in plot

sp.dfs <- 
  mclapply( 
    names( mapdata@data[ , ethnicities ] ) , 
    function( x ) { 
      mapdata@data[ is.na( mapdata@data[, x ] ) , x ] <- 0 ; 
      if( sum( as.integer( mapdata@data[ , x ] / rep.weight ) ) == 0 ) {return(NULL)} ; 
      dotsInPolys( mapdata , as.integer( mapdata@data[ , x ] / rep.weight ), f="random" ) } , 
    mc.set.seed = TRUE, mc.silent = FALSE , mc.cleanup = TRUE )

# define a palette
pal <- c("#8dd3c7", "#ffffb3", "#fb8072", "#bebada", "#80b1d3" )
stopifnot( length(ethnicities) == length( pal ) ) 

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

# plot map!
recplot <- ggplot(mapdata) +
  geom_point(data = dots.final, aes(x, y, colour = Ethnicity), size = .125 , alpha = .4 ) +
  # geom_path(aes(long, lat, group = group), colour = "#d3d3d3") +
  geom_path(data = blocmap , aes(long, lat, group = group), colour = "#d3d3d3" , size = .6 , alpha = .1 ) +
  geom_path(data = munbmap , aes(long, lat, group = group), colour = "#d3d3d3" , size = .6 , alpha = .5 ) +
  scale_colour_manual(values = pal) +
  guides( color = guide_legend( title = "Cor/Raça" , override.aes = list( size = 1 , alpha = 1 ) ) ) +
  theme_map() +
  labs( 
    title = "Ethnical composition of Recife, Jaboatão dos Guararapes, and Olinda" ,
    subtitle = paste0("Each point represents approx. " , rep.weight , " individuals" ) ,
    caption = "Source: Aggregates from Brazil 2010 Census Tracts." ) +
  theme( plot.background = element_rect(fill = "black") ,
         plot.title = element_text( color = "white" , size = 20 ) ,
         plot.subtitle = element_text( color = "white" , size = 15 ) ,
         plot.caption = element_text( color = "white" , size = 10 , hjust = 1 ) ,
         legend.position = "right" , 
         legend.background = element_rect( fill = "transparent" , colour = "transparent" ) ,
         legend.key = element_rect( fill = "transparent" , color = "transparent" ) ,
         legend.title = element_text( color = "white" , size = 15 , face = "bold" ) ,
         legend.text = element_text( color = "white" , size = 13 ) ) +
  coord_map()

## ----mapa_sal, echo=FALSE , fig.height=12, fig.width=20, message=FALSE, warning=FALSE, cache = FALSE----
# load maps

# load libraries
library(ggplot2)
library(maptools)
library(rgeos)
library(rgdal)
library(ggthemes)
# library(ggiraph)

# list and select maps
mapfile <- list.files( mapsdir , full.names = TRUE , recursive = TRUE , pattern = "shp" ) # list shapefiles in `mapsdir`
mapfile <- mapfile[ grepl( "SEE" , basename( mapfile ) , ignore.case = TRUE ) ] # keep census tracts maps
mapfile <- mapfile[ grepl( "^29" , basename( mapfile ) , ignore.case = TRUE ) ] # keep RJ only

# read map
salmap <- readOGR( dsn = mapfile , verbose = FALSE , stringsAsFactors = FALSE , encoding = "latin1" )

# fix column names
colnames( salmap@data ) <- tolower( colnames( salmap@data ) )

# keep Salvador only
salmap <- subset( salmap , cd_geocodm %in% c( "2927408" ) ) # Salvador
# salmap <- subset( salmap , nm_micro %in% "SALVADOR" ) # microrregião

# keep urban areas only
# salmap <- subset( salmap , tipo == 'URBANO' )

# apply standardized projection
salmap <- spTransform( salmap , CRS("+proj=longlat +datum=WGS84") )



# load data
datafile <- list.files( datadir , full.names = TRUE , recursive = TRUE , pattern = "Rds" ) # list csv in `datadir`
datafile <- datafile[ grepl( "^pessoa03" , basename( datafile ) , ignore.case = TRUE ) ] # keep "pessoa03" table only
datafile <- datafile[ grepl( "_ba" , basename( datafile ) , ignore.case = TRUE ) ] # keep PE only

# read and stack data
saldata <- lapply( datafile , function( x ) readRds( x )  )
saldata <- do.call( rbind , saldata )

# fix column names
colnames( saldata ) <- tolower( colnames( saldata ) )

# fix column formats
saldata[ , grep( "^cod|setor" , colnames( saldata ) ) ] <- apply( saldata[ , grep( "^cod|setor" , colnames( saldata ) ) ] , 2 , as.character )
saldata[ , grep( "^v" , colnames( saldata ) ) ] <- apply( saldata[ , grep( "^v" , colnames( saldata ) ) ], 2 , function( x ) suppressWarnings( as.numeric( x ) ) )

# set names
# ethnicities <- c( "branca" , "preta" , "amarela" , "parda" , "indígena" )
ethnicities <- c( "Branca" , "Preta" , "Amarela" , "Parda" , "Indígena" )
colnames( saldata )[ colnames( saldata ) %in% paste0( "v00" , 2:6 ) ] <- ethnicities

# drop "parda"
# saldata$parda <- 0

# keep only census tracts in map
saldata <- saldata[ saldata$cod_setor %in% salmap$cd_geocodi ,]


# merge map with data
mapdata <- merge( salmap , saldata , by.x = "cd_geocodi" , by.y = "cod_setor" , all.x = TRUE , all.y = FALSE )

# create neighborhood maps
munbmap <- unionSpatialPolygons( salmap , IDs = salmap$cd_geocodm )
# blocmap <- unionSpatialPolygons( salmap , IDs = salmap$cd_geocodb )
# blocmap <- unionSpatialPolygons( salmap , IDs = salmap$cd_geocodd )
blocmap <- unionSpatialPolygons( salmap , IDs = salmap$cd_geocods )

# parallelized code to assign dots to polygons
library(parallel)

set.seed(123)

rep.weight <- 80 # use this to calibrate representativeness in plot

sp.dfs <- 
  mclapply( 
    names( mapdata@data[ , ethnicities ] ) , 
    function( x ) { 
      mapdata@data[ is.na( mapdata@data[, x ] ) , x ] <- 0 ; 
      if( sum( as.integer( mapdata@data[ , x ] / rep.weight ) ) == 0 ) {return(NULL)} ; 
      dotsInPolys( mapdata , as.integer( mapdata@data[ , x ] / rep.weight ), f="random" ) } , 
    mc.set.seed = TRUE, mc.silent = FALSE , mc.cleanup = TRUE )

# define a palette
pal <- c("#8dd3c7", "#ffffb3", "#fb8072", "#bebada", "#80b1d3" )
stopifnot( length(ethnicities) == length( pal ) ) 

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

# plot map!
salplot <- ggplot(mapdata) +
  geom_point(data = dots.final, aes(x, y, colour = Ethnicity), size = .125 , alpha = .4 ) +
  # geom_path(aes(long, lat, group = group), colour = "#d3d3d3") +
  geom_path(data = blocmap , aes(long, lat, group = group), colour = "#d3d3d3" , size = .6 , alpha = .25 ) +
  geom_path(data = munbmap , aes(long, lat, group = group), colour = "#d3d3d3" , size = .6 , alpha = .5 ) +
  scale_colour_manual(values = pal) +
  guides( color = guide_legend( title = "Cor/Raça" , override.aes = list( size = 1 , alpha = 1 ) ) ) +
  theme_map() +
  labs( 
    title = "Ethnical composition of Salvador" ,
    subtitle = paste0("Each point represents approx. " , rep.weight , " individuals" ) ,
    caption = "Source: Aggregates from Brazil 2010 Census Tracts." ) +
  theme( plot.background = element_rect(fill = "black") ,
         plot.title = element_text( color = "white" , size = 20 ) ,
         plot.subtitle = element_text( color = "white" , size = 15 ) ,
         plot.caption = element_text( color = "white" , size = 10 , hjust = 1 ) ,
         legend.position = "right" , 
         legend.background = element_rect( fill = "transparent" , colour = "transparent" ) ,
         legend.key = element_rect( fill = "transparent" , color = "transparent" ) ,
         legend.title = element_text( color = "white" , size = 15 , face = "bold" ) ,
         legend.text = element_text( color = "white" , size = 13 ) ) +
  coord_map()

## ----mapa_mao, echo=FALSE, fig.height=15, fig.width=15, message=FALSE, warning=FALSE, cache = FALSE----
# drop unnecessary objects from environment
# rm( grep( "dir" , ls() , invert = TRUE ) )

# load maps

# load libraries
library(ggplot2)
library(maptools)
library(rgeos)
library(rgdal)
library(ggthemes)
# library(ggiraph)

# list and select maps
mapfile <- list.files( mapsdir , full.names = TRUE , recursive = TRUE , pattern = "shp" ) # list shapefiles in `mapsdir`
mapfile <- mapfile[ grepl( "SEE" , basename( mapfile ) , ignore.case = TRUE ) ] # keep census tracts maps
mapfile <- mapfile[ grepl( "^13" , basename( mapfile ) , ignore.case = TRUE ) ] # keep AM only

# read map
maomap <- readOGR( dsn = mapfile , verbose = FALSE , stringsAsFactors = FALSE , encoding = "latin1" )

# fix column names
colnames( maomap@data ) <- tolower( colnames( maomap@data ) )

# keep Manaus only
maomap <- subset( maomap , cd_geocodm == '1302603' )

# keep urban areas only
maomap <- subset( maomap , tipo == 'URBANO' )

# apply standardized projection
maomap <- spTransform( maomap , CRS("+proj=longlat +datum=WGS84") )



# load data
datafile <- list.files( datadir , full.names = TRUE , recursive = TRUE , pattern = "Rds" ) # list csv in `datadir`
datafile <- datafile[ grepl( "^pessoa03" , basename( datafile ) , ignore.case = TRUE ) ] # keep "pessoa03" table only
datafile <- datafile[ grepl( "_am" , basename( datafile ) , ignore.case = TRUE ) ] # keep AM only

# read and stack data
maodata <- lapply( datafile , function( x ) readRds( x )  )
maodata <- do.call( rbind , maodata )

# fix column names
colnames( maodata ) <- tolower( colnames( maodata ) )

# fix column formats
maodata[ , grep( "^cod|setor" , colnames( maodata ) ) ] <- apply( maodata[ , grep( "^cod|setor" , colnames( maodata ) ) ] , 2 , as.character )
maodata[ , grep( "^v" , colnames( maodata ) ) ] <- apply( maodata[ , grep( "^v" , colnames( maodata ) ) ], 2 , function( x ) suppressWarnings( as.numeric( x ) ) )

# set names
# ethnicities <- c( "branca" , "preta" , "amarela" , "parda" , "indígena" )
ethnicities <- c( "Branca" , "Preta" , "Amarela" , "Parda" , "Indígena" )
colnames( maodata )[ colnames( maodata ) %in% paste0( "v00" , 2:6 ) ] <- ethnicities

# drop "parda"
# maodata$parda <- 0

# keep only census tracts in map
maodata <- maodata[ maodata$cod_setor %in% maomap$cd_geocodi ,]


# merge map with data
mapdata <- merge( maomap , maodata , by.x = "cd_geocodi" , by.y = "cod_setor" , all.x = TRUE , all.y = FALSE )

# create neighborhood maps
munbmap <- unionSpatialPolygons( maomap , IDs = maomap$cd_geocodm )
blocmapA <- unionSpatialPolygons( maomap , IDs = maomap$cd_geocodb )
blocmapB <- unionSpatialPolygons( maomap , IDs = maomap$cd_geocodi )

# parallelized code to assign dots to polygons
library(parallel)

set.seed(123)

sp.dfs <- 
  mclapply( 
    names( mapdata@data[ , ethnicities ] ) , 
    function( x ) { 
      mapdata@data[ is.na( mapdata@data[, x ] ) , x ] <- 0 ; 
      if( sum(mapdata@data[ , x ]) == 0 ) {return(NULL)} ; 
      dotsInPolys( mapdata , as.integer( mapdata@data[, x ] / 80 ), f="random" ) } , # use the division to calibrate plot
    mc.set.seed = TRUE, mc.silent = FALSE , mc.cleanup = TRUE )


# define a palette
pal <- c("#8dd3c7", "#ffffb3", "#fb8072", "#bebada", "#80b1d3" )
stopifnot( length(ethnicities) == length( pal ) ) 

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

# plot map!
maoplot <- ggplot(mapdata) +
  geom_point(data = dots.final, aes(x, y, colour = Ethnicity), size = .125 , alpha = .25 ) +
  # geom_path(aes(long, lat, group = group), colour = "#d3d3d3") +
  geom_path(data = munbmap , aes(long, lat, group = group), colour = "#d3d3d3" , size = .6 , alpha = .5 ) +
  geom_path(data = blocmapA , aes(long, lat, group = group), colour = "#d3d3d3" , size = .6 , alpha = .2 ) +
  geom_path(data = blocmapB , aes(long, lat, group = group), colour = "#d3d3d3" , size = .6 , alpha = .075 ) +
  scale_colour_manual(values = pal) +
  guides( color = guide_legend( title = "Cor/Raça" , override.aes = list( size = 1 , alpha = 1 ) ) ) +
  theme_map() +
  labs( 
    title = "Ethnical composition of Manaus' urban areas" ,
    subtitle = "Each point represents approx. 80 individuals" , 
    caption = "Source: Aggregates from Brazil 2010 Census Tracts." ) +
  theme( plot.background = element_rect(fill = "black") ,
         plot.title = element_text( color = "white" , size = 20 ) ,
         plot.subtitle = element_text( color = "white" , size = 15 ) ,
         plot.caption = element_text( color = "white" , size = 10 , hjust = 1 ) ,
         legend.position = "right" , 
         legend.background = element_rect( fill = "transparent" , colour = "transparent" ) ,
         legend.key = element_rect( fill = "transparent" , color = "transparent" ) ,
         legend.title = element_text( color = "white" , size = 15 , face = "bold" ) ,
         legend.text = element_text( color = "white" , size = 13 ) ) +
  coord_map()


##### save maps

plotlist <- grep( "plot$" , ls() , value = TRUE )
lapply( plotlist , function( this_plot ) {
  ggsave( normalizePath( file.path( "~/Documents/GitLab/src-website" , paste0("static/img/ethnic_" , this_plot , "_highres.png") ) , mustWork = FALSE ) , get( this_plot ) , width = 15 , height = 15 , dpi = 300 )
  ggsave( normalizePath( file.path( "~/Documents/GitLab/src-website" , paste0("static/img/ethnic_" , this_plot , "_lowres.png") ) , mustWork = FALSE ) , get( this_plot ) , width = 15 , height = 15 , dpi = 100 ) # lower res
  cat( this_plot , "saved\n")
} )

# plotlist.obj <- lapply( grep( "plot$" , ls() , value = TRUE ) , get )
# lapply( seq_along(plotlist.obj) , function( this_plot ) {
#   ggsave( normalizePath( file.path( "~/Documents/GitLab/src-website" , paste0("static/img/ethnic_" , this_plot , ".png") ) , mustWork = FALSE ) , plot = plotlist.obj[[this_plot]] , width = 15 , height = 15 )
#   cat( this_plot , "saved\n")
# } )

