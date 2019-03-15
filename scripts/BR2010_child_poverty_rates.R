# estimate child poverty for brazilian municipalities

# set parameters
setwd("/Users/guilhermejacob/Documents/GitLab/src-website")
output_dir <- "/Volumes/Trabalho/Censo"
poverty.line = 510/4
subset.area = FALSE

# load libraries and set options
library(DBI)
library(MonetDBLite)
library(survey)
library(convey)

options( survey.lonely.psu = "adjust" )

# access survey design object
cat("connecting to database\r")
censo_design <- readRDS( file.path( output_dir , "pes 2010 design.rds" ) )
censo_design <- open( censo_design , driver = MonetDBLite() )
cat("connecting to database\tdone!\n")

# create 7-digits municipality code:
censo_design <- update( censo_design , codmun7 = paste0( v0001 , v0002 ) )

# if `subset.area` is TRUE, then subset to Amazonas State
if ( subset.area ) censo_design <- subset( censo_design , v0001 %in% c(11:17,21,51) )
# if ( subset.area ) censo_design <- subset( censo_design , v0001 == 13 )
# if ( subset.area ) censo_design <- subset( censo_design , codmun7 %in% c( '1302603' , '1301902' , '1302306' ) )

# test for selected totals
# svytotal( ~one , censo_design , na.rm = TRUE )
svytotal( ~one , subset( censo_design , codmun7 == '1302306' ) , na.rm = TRUE )

# set up convey database
# suppressWarnings( censo_design <- convey_prep( censo_design ) )

# estimate inequality indices for each municipality
cat("calculating gini indices\n")
gini_list <- svyby( ~v6531 , ~codmun7 , suppressWarnings( convey_prep( censo_design ) ) , svygini , na.rm = TRUE , multicore = TRUE )
cat("calculating gini indices\tdone!\n")

# # subset to people under 18 y.o.
# censo_design <- subset( censo_design , v6036 < 18 )

# estimate poverty indices for each municipality
# using convey
cat("calculating poverty indices\n")
fgt_list <- NULL
# fgt_list <- lapply( c(0,2) , function( this_g ) {
#   cat("calculating poverty indices: g =" , this_g , "\n")
#   svyby( ~v6531 , ~codmun7 , suppressWarnings( convey_prep( censo_design ) ) , svyfgt , g = this_g , abs_thresh=poverty.line , na.rm = TRUE , verbose = TRUE , multicore = TRUE )
#   cat("calculating poverty indices: g =" , this_g , "\tdone!\n")
# } )
# workaround:
# fgt_list <- NULL
# fgt_list <- lapply( c(0) , function( this_g ) { 
#   cat("calculating poverty indices: g =" , this_g , "\n")
#   svyby( as.formula( paste0( "~I( ifelse( v6531 < " , poverty.line , " , ( 1 - v6531 / ", poverty.line , " )^" , this_g , " , 0 ) ) " ) ), ~codmun7 , censo_design , svymean , na.rm = TRUE , verbose = TRUE , multicore = TRUE ) 
#   cat("calculating poverty indices: g =" , this_g , "\tdone!\n")
# } )

# close database connection
cat("disconnecting from database\n")
close( censo_design , shutdown = TRUE )

# save results
dest_file <- file.path( getwd() , "static/datasets" , "BR2010_PovEsts.Rdata" )
cat("saving files to" , dest_file , "\n")
if( file.exists( dest_file ) ) file.remove( dest_file )
save( list = grep( "_list$" , ls() , value = TRUE ) , file = dest_file )

cat("done!\n")

