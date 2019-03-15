library(data.table)
library(fst)
library(magrittr)

cnae_secoes <- list( 1:3 , 5:9 , 10:33 , 35 , 36:39 , 41:43 , 45:47 ,
                     49:53 , 55:56 , 58:63 , 64:66 , 68 , 69:75 , 77:82 ,
                     84 , 85 , 86:88 , 90:93 , 94:96 , 97 , 99 )
cnae_secoes <- lapply( seq_along(cnae_secoes) , function( i ) { names( cnae_secoes[[i]] ) <- rep( LETTERS[i] , length(cnae_secoes[[i]]) ) ; cnae_secoes[[i]] })
cnae_secoes <- unlist(cnae_secoes)
cnae_secoes <- data.table( 
  cnae_2_0_secao = names( cnae_secoes ) , 
  cnae_2_0_divisao = stringr::str_pad( cnae_secoes , 2 , "left" , "0" ) )

### COLLECT DATA
dtfull <- read_fst( "/Users/guilhermejacob/Documents/GitLab/src-website/static/datasets/serie_caged.fst" , as.data.table = TRUE )




### RECODES
dtfull[ , total := as.double( total ) ]
dtfull[ , uf := as.numeric( uf ) ]
dtfull[ admitidos_desligados == "01" , admitidos_desligados := "Admitidos" ]
dtfull[ admitidos_desligados == "02" , admitidos_desligados := "Desligados" ]
dtfull[ , cnae_2_0_divisao := substr( cnae_2_0_classe , 1 , 2 ) ]
dtfull[ , ano := substr( competencia_declarada , 1 , 4 ) ]
dtfull[ , mes := substr( competencia_declarada , 5 , 6 ) ]
dtfull[ , p_date := paste0( ano , "-" , mes , "-01" ) ]
dtfull[ , p_date := as.IDate( p_date , format = "%Y-%m-%d" ) ]

dtfull <- merge( dtfull , cnae_secoes , by = "cnae_2_0_divisao" , all.x = TRUE , all.y = FALSE )
# dtfull <- dtfull[ year(p_date) >= 2008 ]

# example
ex1 <- dcast( dtfull[ , .( total = sum( total ) ) , by = .( ano , mes , admitidos_desligados ) ] , ano + mes ~ admitidos_desligados , value.var = "total" , fill = NA , drop = FALSE )
ex1[ , Saldo := Admitidos - Desligados ]
dcast( ex1 , mes ~ ano  , value.var = "Saldo" , fill = NA , drop = FALSE )





### SUMMARIZE
dt_saldo <- dcast( dtfull[ , .( total = sum( total ) ) , by = .( p_date , admitidos_desligados ) ] , p_date ~ admitidos_desligados , value.var = "total" , fill = 0 , drop = FALSE )
dt_saldo[ , Saldo := Admitidos - Desligados ]
dt_saldo <- melt( dt_saldo , id.vars = c( "p_date" ) , variable.name = "tipo_mov" , value.name = "total" )

dt_saldo_sec <- dtfull[ , .( total = sum( total ) ) , by = .(p_date , cnae_2_0_secao , admitidos_desligados ) ] %>% dcast( . , p_date + cnae_2_0_secao ~ admitidos_desligados , value.var = "total" , fill = 0 , drop = FALSE )
dt_saldo_sec[ , Saldo := Admitidos - Desligados ]
dt_saldo_sec <- melt( dt_saldo_sec , id.vars = c( "p_date" , "cnae_2_0_secao" ) , variable.name = "tipo_mov" , value.name = "total" )

dt_saldo_uf <- dcast( dtfull[ , .( total = sum( total ) ) , by = .( p_date , uf , cnae_2_0_secao , admitidos_desligados ) ] , p_date + uf + cnae_2_0_secao ~ admitidos_desligados , value.var = "total" , fill = 0 , drop = F )
dt_saldo_uf[ , Saldo := Admitidos - Desligados ]
dt_saldo_uf <- melt( dt_saldo_uf , id.vars = c( "p_date" , "uf" , "cnae_2_0_secao" ) , variable.name = "tipo_mov" , value.name = "total" )





### DESEASONALIZE
dt_saldo[ , rolling_total := Reduce(`+`, shift( total , 0:11 ) )/12 , by = .( tipo_mov ) ]

dt_saldo_sec <- subset( dt_saldo_sec , !is.na( cnae_2_0_secao ) )
dt_saldo_sec[ , rolling_total := Reduce(`+`, shift( total , 0:11 ) )/12 , by = .( tipo_mov , cnae_2_0_secao ) ]

dt_saldo_uf <- subset( dt_saldo_uf , !is.na( uf ) )
dt_saldo_uf <- subset( dt_saldo_uf , !is.na( cnae_2_0_secao ) )
dt_saldo_uf[ , rolling_total := Reduce(`+`, shift( total , 0:11 ) )/12 , by = .( tipo_mov , uf , cnae_2_0_secao ) ]





# RESHAPE
these_mvars <- c( "total" , "rolling_total" )
dt_saldo <- melt( dt_saldo , id.vars = c( "p_date" , "tipo_mov" ) , measure.vars = these_mvars )
dt_saldo_sec <- melt( dt_saldo_sec , id.vars = c( "p_date" , "tipo_mov" , "cnae_2_0_secao" ) , measure.vars = these_mvars )
dt_saldo_uf <- melt( dt_saldo_uf , id.vars = c( "p_date" , "tipo_mov" , "uf" ,  "cnae_2_0_secao" ) , measure.vars = these_mvars )

these_labels <- c( "Original" , "Média (12 meses)" )
dt_saldo[ , new_variable := as.factor( variable ) ]
levels(dt_saldo$new_variable) <- these_labels
dt_saldo_sec[ , new_variable := as.factor( variable ) ]
levels(dt_saldo_sec$new_variable) <- these_labels
dt_saldo_uf[ , new_variable := as.factor( variable ) ]
levels(dt_saldo_uf$new_variable) <- these_labels




### PLOTTING
library(ggplot2)
library(scales) 

# plot subsets and recodes
dt_saldo <- dt_saldo[ p_date <= "2018-06-01" ]
dt_saldo_sec <- dt_saldo_sec[ p_date <= "2018-06-01" ]
dt_saldo_uf <- dt_saldo_uf[ p_date <= "2018-06-01" ]

for ( this_dt in list( dt_saldo_sec , dt_saldo_uf ) ) { 
  this_dt[ cnae_2_0_secao == "A" , new_sec := "Agricultura" ]
  this_dt[ cnae_2_0_secao == "B" , new_sec := "Ind. Extração" ]
  this_dt[ cnae_2_0_secao == "C" , new_sec := "Ind. Transformação" ]
  this_dt[ cnae_2_0_secao == "D" , new_sec := "Eletricidade e gás" ]
  this_dt[ cnae_2_0_secao == "E" , new_sec := "Água, esgoto, atividades de gestão de resíduos e descontaminação" ]
  this_dt[ cnae_2_0_secao == "F" , new_sec := "Construção" ]
  this_dt[ cnae_2_0_secao == "G" , new_sec := "Comércio" ]
}

# produce plots
p1 <- dt_saldo %>% 
  .[ tipo_mov %in% c( "Admitidos" , "Desligados" ) ] %>% 
  .[ variable %in% c( "total" , "rolling_total" ) ] %>% 
  ggplot( ., aes( x = p_date , y = value , color = tipo_mov ) ) +
  facet_wrap( ~new_variable , ncol = 1 , scales = "fixed" ) +
  geom_line() +
  geom_hline( yintercept = 0 , color = "black" ) +
  scale_colour_manual( values = c( "Admitidos" = "#0066ff" , "Desligados" = "#c70039" ) ) +
  scale_y_continuous( labels = function(x) format(x, big.mark = " ", scientific = FALSE ) , expand = c(0,10^5) ) +
  scale_x_date( labels = date_format("%Y-%m") , date_breaks = "3 month" , expand = c(0,0) ) +
  scale_linetype_manual( breaks = c( "total_trend" , "total_d" , "total" ) , values = 3:1 ) +
  scale_alpha_manual( breaks = c( "total_trend" , "total_d" , "total" ) , values = c(.4,.4,1) ) +
  guides( linetype = NULL , alpha = FALSE ) +
  labs( x = NULL , y = "Postos" , color = "Movimentação" ) +
  theme( 
    # panel.grid.minor.x = element_blank() , 
    strip.background = element_rect( fill = "white" ) ,
    strip.text = element_text( face = "bold" ) ,
    panel.grid.minor.x = element_line( color = "lightgray" , linetype = 2 , size = .125/2 ) ,
    panel.grid.major.x = element_line( color = "lightgray" , linetype = 2 , size = .125 ) ,
    panel.grid.major.y = element_line( color = "lightgray" , linetype = 3 , size = .25 ) ,
    panel.background = element_blank() ,
    axis.text.x = element_text( angle = 90 , vjust = .5 ),
    plot.title = element_text(lineheight=.8, face="bold") 
  )

p2 <- dt_saldo %>% 
  .[ tipo_mov %in% c( "Saldo" ) ] %>% 
  .[ variable %in% c( "total" , "rolling_total" ) ] %>% 
  ggplot( ., aes( x = p_date , y = value ) ) +
  facet_wrap( ~new_variable , ncol = 1 , scales = "fixed" ) +
  geom_line( color = "#6ab04c" ) +
  geom_hline( yintercept = 0 , color = "black" ) +
  scale_y_continuous( labels = function(x) format(x, big.mark = " ", scientific = FALSE ) , expand = c(0,10^5) ) +
  scale_x_date( labels = date_format("%Y-%m") , date_breaks = "3 month" , expand = c(0,0) ) +
  scale_linetype_manual( breaks = c( "total_trend" , "total_d" , "total" ) , values = 3:1 ) +
  scale_alpha_manual( breaks = c( "total_trend" , "total_d" , "total" ) , values = c(.4,.4,1) ) +
  guides( linetype = FALSE , alpha = FALSE ) +
  labs( x = NULL , y = "Postos" , color = "Movimentação" ) +
  theme( 
    # panel.grid.minor.x = element_blank() , 
    strip.background = element_rect( fill = "white" ) ,
    strip.text = element_text( face = "bold" ) ,
    panel.grid.minor.x = element_line( color = "lightgray" , linetype = 2 , size = .125/2 ) ,
    panel.grid.major.x = element_line( color = "lightgray" , linetype = 2 , size = .125 ) ,
    panel.grid.major.y = element_line( color = "lightgray" , linetype = 3 , size = .25 ) ,
    panel.background = element_blank() ,
    axis.text.x = element_text( angle = 90 , vjust = .5 ),
    plot.title = element_text(lineheight=.8, face="bold") 
  )

p3 <- dt_saldo_sec %>% 
  .[ cnae_2_0_secao %in% c( "A" , "C" , "F" ) ] %>% 
  .[ tipo_mov %in% c( "Saldo" ) ] %>% 
  .[ variable %in% c( "total" , "rolling_total" ) ] %>% 
  # .[ variable %in% c( "total_d" , "total_trend" , "total_season" ) ] %>% 
  ggplot( ., aes( x = p_date , y = value , color = new_sec , group = cnae_2_0_secao ) ) +
  facet_wrap( ~new_variable , ncol = 1 , scales = "fixed" ) +
  geom_hline( yintercept = 0 , color = "black" ) +
  geom_line( ) +
  scale_colour_brewer( type = "qual" , palette = 6 ) +
  scale_y_continuous( labels = function(x) format(x, big.mark = " ", scientific = FALSE ) , expand = c(0,10^5) ) +
  scale_x_date( labels = date_format("%Y-%m") , date_breaks = "12 month" , expand = c(0,0) ) +
  scale_linetype_manual( breaks = c( "total_trend" , "total_d" , "total" ) , values = 3:1 ) +
  scale_alpha_manual( breaks = c( "total_trend" , "total_d" , "total" ) , values = c(.4,.4,1) ) +
  guides( linetype = FALSE , alpha = FALSE ) +
  labs( x = NULL , y = "Postos" , color = "Setores" ) +
  theme( 
    # panel.grid.minor.x = element_blank() , 
    strip.background = element_rect( fill = "lightblue" ) ,
    strip.text = element_text( face = "bold" ) ,
    panel.grid.minor.x = element_line( color = "lightgray" , linetype = 2 , size = .125/2 ) ,
    panel.grid.major.x = element_line( color = "lightgray" , linetype = 2 , size = .125 ) ,
    panel.grid.major.y = element_line( color = "lightgray" , linetype = 3 , size = .25 ) ,
    panel.background = element_blank() ,
    legend.key = element_blank() ,
    legend.background = element_rect( fill = "white" ) ,
    axis.text.x = element_text( angle = 90 , vjust = .5 ),
    plot.title = element_text(lineheight=.8, face="bold") 
  )


p4 <- dt_saldo_uf %>% 
  .[ uf %in% 13 ] %>% 
  .[ cnae_2_0_secao %in% c( "A" , "C" , "F" ) ] %>% 
  .[ tipo_mov %in% c( "Saldo" ) ] %>% 
  .[ variable %in% c( "total" , "rolling_total" ) ] %>% 
  # .[ variable %in% c( "total_d" , "total_trend" , "total_season" ) ] %>% 
  ggplot( ., aes( x = p_date , y = value , color = new_sec , group = cnae_2_0_secao ) ) +
  facet_wrap( ~new_variable , ncol = 1 , scales = "fixed" ) +
  geom_hline( yintercept = 0 , color = "black" ) +
  geom_line( ) +
  scale_colour_brewer( type = "qual" , palette = 6 ) +
  scale_y_continuous( labels = function(x) format(x, big.mark = " ", scientific = FALSE ) , expand = c(0,10^3) ) +
  scale_x_date( labels = date_format("%Y-%m") , date_breaks = "3 month" , expand = c(0,0) ) +
  scale_linetype_manual( breaks = c( "total_trend" , "total_d" , "total" ) , values = 3:1 ) +
  scale_alpha_manual( breaks = c( "total_trend" , "total_d" , "total" ) , values = c(.4,.4,1) ) +
  guides( linetype = FALSE , alpha = FALSE ) +
  labs( x = NULL , y = "Postos" , color = "Setores" ) +
  theme( 
    # panel.grid.minor.x = element_blank() , 
    strip.background = element_rect( fill = "lightblue" ) ,
    strip.text = element_text( face = "bold" ) ,
    panel.grid.minor.x = element_line( color = "lightgray" , linetype = 2 , size = .125/2 ) ,
    panel.grid.major.x = element_line( color = "lightgray" , linetype = 2 , size = .125 ) ,
    panel.grid.major.y = element_line( color = "lightgray" , linetype = 3 , size = .25 ) ,
    panel.background = element_blank() ,
    legend.key = element_blank() ,
    legend.background = element_rect( fill = "white" ) ,
    axis.text.x = element_text( angle = 90 , vjust = .5 ),
    plot.title = element_text(lineheight=.8, face="bold") 
  )




### TABLES

# total
dt_saldo[ , c( "ano" , "mes") := list( year( p_date ) , month( p_date ) ) ]
dt_saldo[ , r_value := round( value , 0 ) ]
ex1 <- dcast( dt_saldo , ano + mes + tipo_mov ~ variable , value.var = "r_value" , fill = NA , drop = FALSE )
ex1 <- dcast( ex1 , ano + mes ~ tipo_mov , value.var = "total" , fill = NA , drop = FALSE )
ex1 <- ex1[ ano >= 2008 ]
dcast( ex1 , mes ~ ano  , value.var = "Saldo" , fill = NA , drop = FALSE )

ex2 <- dcast( dt_saldo , ano + mes + tipo_mov ~ variable , value.var = "r_value" , fill = NA , drop = FALSE )
ex2 <- dcast( ex2 , ano + mes ~ tipo_mov , value.var = "rolling_total" , fill = NA , drop = FALSE )
ex2 <- ex2[ ano >= 2008 ]
dcast( ex2 , mes ~ ano  , value.var = "Saldo" , fill = NA , drop = FALSE )

# sector
dt_saldo_sec[ , c( "ano" , "mes") := list( year( p_date ) , month( p_date ) ) ]
dt_saldo_sec[ , r_value := round( value , 0 ) ]
these_sectors <- c( "A" , "C" , "F" )
lapply( these_sectors , function(y) {
  ex <- dcast( dt_saldo_sec[ cnae_2_0_secao == y ] , ano + mes + tipo_mov ~ variable , value.var = "r_value" , fill = NA , drop = FALSE )
  ex <- dcast( ex , ano + mes ~ tipo_mov , value.var = "rolling_total" , fill = NA , drop = FALSE )
  ex <- ex[ ano >= 2008 ]
  dcast( ex , mes ~ ano  , value.var = "Saldo" , fill = NA , drop = FALSE )
} )

# am
# sector
dt_saldo_uf[ , c( "ano" , "mes") := list( year( p_date ) , month( p_date ) ) ]
dt_saldo_uf[ , r_value := round( value , 0 ) ]
lapply( these_sectors , function(y) {
  ex <- dcast( dt_saldo_uf[ uf == 13 & cnae_2_0_secao == y ] , ano + mes + tipo_mov ~ variable , value.var = "r_value" , fill = NA , drop = FALSE )
  ex <- dcast( ex , ano + mes ~ tipo_mov , value.var = "total" , fill = NA , drop = FALSE )
  ex <- ex[ ano >= 2008 ]
  dcast( ex , mes ~ ano  , value.var = "Saldo" , fill = NA , drop = FALSE )
} )

lapply( these_sectors , function(y) {
  ex <- dcast( dt_saldo_uf[ uf == 13 & cnae_2_0_secao == y ] , ano + mes + tipo_mov ~ variable , value.var = "r_value" , fill = NA , drop = FALSE )
  ex <- dcast( ex , ano + mes ~ tipo_mov , value.var = "rolling_total" , fill = NA , drop = FALSE )
  ex <- ex[ ano >= 2008 ]
  dcast( ex , mes ~ ano  , value.var = "Saldo" , fill = NA , drop = FALSE )
} )