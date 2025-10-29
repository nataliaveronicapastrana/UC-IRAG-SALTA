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

#-----------------------------------------------------------------------------------
#ACTIVACION DE LIBRERIAS
#-----------------------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(readxl)
library(readr)
library(writexl)
library(readr)
library(highcharter)
library(tidyr)
library(mapview)
library(leaflet)
library(geoAr)
library(gt)
library(htmltools)
library(tidyverse)
library(ComplexUpset)
