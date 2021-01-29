# adiciona a tabela de deflatores na base de dados

# carrega libraries
library(DBI)
library(MonetDBLite)
library(readxl)
library(data.table)

### processa deflatores

# lê arquivo do excel
deflat.tab <- read_xls( "/home/guilherme/DataVault/PNAD Contínua/Trimestral/Documentacao/deflator_PNADC_2020_trimestral_070809.xls" , sheet = 1 )

# converte para data.table
deflat.tab <- as.data.table( deflat.tab )

# ajusta nomes
colnames( deflat.tab ) <- tolower( colnames( deflat.tab ) )

# define trimestre de referência para deflação
deflat.tab[ , trimestre := as.numeric( substr( trim , 7 , 8 ) ) / 3 ]
deflat.tab <- deflat.tab[ ( deflat.tab$trimestre %% 1 ) == 0 , ]

# ajusta formatos
deflat.tab[ , ano := as.numeric( ano ) ]
deflat.tab[ , uf := as.numeric( uf ) ]
deflat.tab[ , trim := NULL ]

# transforma em data.frame
deflat.tab <- data.frame( deflat.tab , stringsAsFactors = FALSE )

### adiciona tabela à base de dados

# define pasta de base de dados
db.folder <- "/home/guilherme/Bases/Pareamento PNAD/MonetDB" 

# conecta à base de dados
db <- dbConnect( MonetDBLite() , db.folder )

# lista tabelas
dbListTables( db )

# lê tabela
dbWriteTable( db , "deflatores" , deflat.tab )

# desconecta da base de dados
dbDisconnect( db , shutdown = TRUE )
