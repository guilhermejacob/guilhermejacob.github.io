library(survey)
library(data.table)
library(fst)

hh <- read_fst("/home/guilherme/Bases/MICS/MICS5/West and Central Africa/Guinea/2016/hh.fst" , as.data.table = TRUE )[ , c( "hh1" , "hh2" , "stratum" , "psu" )]
wm <- read_fst("/home/guilherme/Bases/MICS/MICS5/West and Central Africa/Guinea/2016/wm.fst" , as.data.table = TRUE )

mics <- merge(hh,wm , by = c("hh1","hh2"))
rm( hh,wm)

# 
mics[ , id_alpha := NULL ]
mics[ !is.na(wb7) & wb7 %in% 1:3 , id_alpha := ifelse( wb7 %in% 1:2 , 0 , 1) ]
# mics[ !is.na(wb7) , id_alpha := ifelse( wb7 %in% 1:2 , 0 , 1) ] 
mics[ wb4 %in% c(2:6) , id_alpha := 1 ] 
# mics[ wb4 == 1 , id_alpha := ifelse( wb7 %in% 1:2 , 0 , 1) ] 

mics_d <- svydesign(ids = ~psu , strata = ~stratum , weights = ~wmweight , data = mics , check.strata = TRUE ) ; rm(mics)
mics_alpha <- subset(mics_d, wb2 >= 15 & wb2 <= 24)


unwtd.count( ~factor( id_alpha ) , mics_alpha , na.rm = TRUE )
svymean( ~I( id_alpha) , mics_alpha , na.rm = TRUE ,vartype="ci")

unwtd.count( ~factor( id_alpha ) , subset(mics_alpha, wb2 <= 19) , na.rm = TRUE )
svymean( ~I( id_alpha) , subset(mics_alpha, wb2 <= 19) , na.rm = TRUE )

unwtd.count( ~factor( id_alpha ) , subset(mics_alpha, wb2 >= 20) , na.rm = TRUE )
svymean( ~I( id_alpha) , subset(mics_alpha, wb2 >= 20) , na.rm = TRUE )

svyby( ~I( id_alpha) , ~hh6 , mics_alpha , svymean , na.rm = TRUE )

svyby( ~I( id_alpha) , ~hh7 , mics_alpha , svymean , na.rm = TRUE , vartype="ci")


lapply( c("logit", "likelihood", "asin", "beta", "mean","xlogit") , function(this_method) confint( svyciprop( ~I(id_alpha==1) , mics_alpha , na.rm = TRUE , method = this_method ) ) )

?svyciprop