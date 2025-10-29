#-----------------------------------------------------------------------------------
#INSTALACION DE PAQUETES
#-----------------------------------------------------------------------------------

install.packages(c(
  "dplyr",
  "ggplot2",
  "lubridate",
  "stringr",
  "readxl",
  "readr",
  "writexl",
  "highcharter",
  "tidyr",
  "mapview",
  "leaflet",
  "geoAr",
  "gt",
  "htmltools"
))

install.packages("devtools")

devtools::install_github("krassowski/complex-upset")
