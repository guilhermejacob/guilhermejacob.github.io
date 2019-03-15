required_packages <- c( "DBI" , "MonetDBLite" , "survey" , "read.dbc" , "archive" , "readxl" , "RCurl" , "httr" , "rvest" , "xml2" , "gesis" , "mitools" , "devtools" )
for ( this_package in required_packages ) {
  if ( !requireNamespace( this_package , quietly = TRUE ) ) { install.packages( this_package ) }
}
if ( !requireNamespace( "lodown" , quietly = TRUE ) ) { devtools::install_github( "ajdamico/lodown" ) }