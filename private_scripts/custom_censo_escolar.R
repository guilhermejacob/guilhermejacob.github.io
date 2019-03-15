source("/home/guilherme/GitLab/src-website/content/private_scripts/censo_escolar.R" , echo = FALSE )


# define paths
output_dir <- "/Volumes/Trabalho/Censo Escolar"
# unlink( output_dir , recursive = TRUE )
datavault_dir <- "/Volumes/DataVault/Censo Escolar"

# read previously stored catalog
if ( file.exists( file.path( output_dir , "censo_escolar.Rds" ) ) ) { 
  prev_catalog <- readRDS( file.path( output_dir , "censo_escolar.Rds" ) )
} else {
  prev_catalog <- NULL
}

# colelct catalog and build datavault
catalog_init <- catalog_censo_escolar( output_dir = output_dir )
catalog_dv <- datavault_censo_escolar( catalog = catalog_init , datavault_dir = datavault_dir )
catalog <- catalog_dv

# subset
catalog <- catalog[ catalog$year >= 2017 , ]

# build database
built_catalog <- build_censo_escolar( catalog = catalog )

# save catalog
built_catalog <- rbind( prev_catalog , built_catalog )
saveRDS( built_catalog , file = file.path( output_dir , "censo_escolar.Rds" ) , compress = TRUE )

# exit R
quit("no")

