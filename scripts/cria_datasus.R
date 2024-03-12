# cria todas as bases do datasus

# define diretórios
output_dir <- "/home/guilherme/Bases/DataSUS"
datavault_dir <- "/home/guilherme/DataVault/DataSUS"

# SIASUS

# lê funções
source("/home/guilherme/GitHub/website-source/content/private_scripts/siasus.R")

# unlink( file.path( output_dir , "siasus" ), recursive = TRUE)

catalog_init <- catalog_siasus( output_dir = output_dir )
catalog_dv <- datavault_siasus( catalog = catalog_init , datavault_dir = datavault_dir )
catalog_final <- build_siasus( catalog = catalog_dv )
monetdb_siasus( catalog = catalog_final , skipExist = FALSE )

# SINAN

# lê funções
source("/home/guilherme/GitHub/website-source/content/private_scripts/sinan.R")

# unlink( file.path( output_dir , "sinan" ), recursive = TRUE)

catalog_init <- catalog_sinan( output_dir = output_dir )
catalog_dv <- datavault_sinan( catalog = catalog_init , datavault_dir = datavault_dir )
catalog_final <- build_sinan( catalog = catalog_dv )
monetdb_sinan( catalog = catalog_final , skipExist = FALSE )

# PNI

# lê funções
source("/home/guilherme/GitHub/website-source/content/private_scripts/pni.R")

# unlink( file.path( output_dir , "pni" ), recursive = TRUE)

catalog_init <- catalog_pni( output_dir = output_dir )
catalog <- catalog_dv <- datavault_pni( catalog = catalog_init , datavault_dir = datavault_dir )
catalog_final <- build_pni( catalog = catalog_dv )
monetdb_pni( catalog = catalog_final )

# SIM, SINASC, SISPRENATAL

# lê funções
source("/home/guilherme/GitHub/website-source/content/private_scripts/datasus.R")

# unlink( file.path( output_dir , c( "sim" , "sinasc" , "sisprenatal" ) ) , recursive = TRUE)

catalog_init <- catalog_datasus( output_dir = output_dir )
catalog <- catalog_dv <- datavault_datasus( catalog = catalog_init , datavault_dir = datavault_dir )
catalog_final <- build_datasus( catalog = catalog_dv[ catalog$year >= 2000 , ] )
monetdb_datasus( catalog = catalog_final )