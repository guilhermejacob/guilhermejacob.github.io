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

### COLLECT DATA
caged_files <- list.files( "/Volumes/Trabalho/MTPS/caged" , recursive = TRUE , full.names = TRUE )
select_cols <- c( "competencia_declarada" , "uf" , "cnae_2_0_classe" , "admitidos_desligados" , "salario_mensal" )

dt_list <- plyr::llply( caged_files , function ( this_file ) {
  
  # read data from file
  x <- read_fst( this_file , columns = select_cols, as.data.table = TRUE )
  
  # weight salário
  x[ salario_mensal > 0 , s_weight := 1 ] ; x[ salario_mensal <= 0 , s_weight := 0 ]
  
  # calculate aggregates
  x[ , .( total = .N , avg_salario = weighted.mean( salario_mensal , s_weight ) ) , by = .( competencia_declarada , uf , cnae_2_0_classe , admitidos_desligados ) ]
  
} , .progress = "text" )

# stack data.tables
dtfull <- rbindlist( dt_list , use.names = TRUE )

# remove list
rm( dt_list , caged_files , select_cols ) ; gc()

# save results
write.fst( dtfull , "/Users/guilhermejacob/Documents/GitLab/src-website/static/datasets/serie_caged.fst" , compress = 100 )
