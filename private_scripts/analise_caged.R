data_folder <- "/Volumes/Trabalho/MTPS/caged/"
these_files <- list.files( data_folder , full.names = TRUE , recursive = TRUE )

these_files <- these_files[ grepl( "[0-1][0-9]201[5-7].fst$" , basename( these_files ) , ignore.case = TRUE ) ]
# this_file <- these_files[[1]]

# library(doParallel)
# nodes <- detectCores()
# cl <- makeCluster(nodes)
# registerDoParallel(cl)

library(plyr)
library(data.table)
library(fst)

cnae_secoes <- list( 1:3 , 5:9 , 10:33 , 35 , 36:39 , 41:43 , 45:47 ,
                     49:53 , 55:56 , 58:63 , 64:66 , 68 , 69:75 , 77:82 ,
                     84 , 85 , 86:88 , 90:93 , 94:96 , 97 , 99 )
cnae_secoes <- lapply( seq_along(cnae_secoes) , function( i ) { names( cnae_secoes[[i]] ) <- rep( LETTERS[i] , length(cnae_secoes[[i]]) ) ; cnae_secoes[[i]] })
cnae_secoes <- unlist(cnae_secoes)
cnae_secoes <- data.table( 
  cnae_2_0_secao = names( cnae_secoes ) , 
  cnae_2_0_divisao = stringr::str_pad( cnae_secoes , 2 , "left" , "0" ) )

dtfull <- llply( these_files , function( this_file ) {
  
  # read data from file
  select_cols <- c( "admitidos_desligados" , "competencia_declarada" , "cnae_2_0_classe" , "salario_mensal" )
  # x <- read_fst( this_file , as.data.table = TRUE )
  x <- read_fst( this_file , as.data.table = TRUE , columns = select_cols )
  
  # recodes
  x[ , admitidos_desligados := as.character( admitidos_desligados ) ]
  x[ admitidos_desligados == "1" , admitidos_desligados := "admitidos" ]
  x[ admitidos_desligados == "2" , admitidos_desligados := "desligados" ]
  # x[ , ano := substr( competencia_declarada , 1 , 4 ) ]
  # x[ , mes := substr( competencia_declarada , 5 , 6 ) ]
  x[ salario_mensal > 0 , sm := salario_mensal ]
  x[ , cnae_2_0_classe := stringr::str_pad( cnae_2_0_classe , 5 , "left" , "0" ) ]
  x[ , cnae_2_0_divisao := substr( cnae_2_0_classe , 1 , 2 ) ]
  x <- merge( x , cnae_secoes , by = "cnae_2_0_divisao" , all.x = TRUE , all.y = FALSE )
  
  # summary
  x[ , .( total = .N , sal_medio = mean( sm , na.rm = TRUE ) ), by = .( competencia_declarada , admitidos_desligados , cnae_2_0_secao ) ]
  
} , .progress = "text" )

dtfull <- rbindlist( dtfull , use.names = TRUE )
dtfull[ , ano := substr( competencia_declarada , 1 , 4 ) ]
dtfull[ , mes := substr( competencia_declarada , 5 , 6 ) ]
dtfull[ , p_date := as.Date( paste0( ano , "-" , mes , "-01" ) ) ]
dtfull[ , ano := as.numeric( ano ) ]
dtfull[ , mes := as.numeric( mes ) ]

dt_saldo <- dcast( dtfull[ , .( total = sum( total ) ) , by = .( p_date , admitidos_desligados ) ] , p_date ~ admitidos_desligados , value.var = "total" , fill = 0 )
dt_saldo[ , dt_saldo := admitidos - desligados ]

dt_saldo_mes <- dcast( dtfull , p_date + cnae_2_0_secao ~ admitidos_desligados , value.var = "total" , fill = 0 )
dt_saldo_mes[ , dt_saldo := admitidos - desligados ]





# plotting
library(ggplot2)
library(scales) 

ggplot( dt_saldo_mes[ !is.na( cnae_2_0_secao) ] , aes( x = p_date , y = dt_saldo , color = cnae_2_0_secao ) ) +
  geom_line() + 
  scale_x_date( labels = date_format("%Y-%B") ) +
  guides( color = FALSE ) +
  theme_classic()


ggplot( dt_saldo , aes( x = p_date , y = dt_saldo ) ) +
  geom_line() + 
  scale_x_date( labels = date_format("%Y-%B") , date_breaks = "12 month" ) +
  guides( color = FALSE ) +
  theme_classic()


