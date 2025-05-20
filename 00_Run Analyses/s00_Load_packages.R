## Set the locale to avoid Encoding problems when read.csv files
Sys.getlocale()
#Sys.setlocale(category = "LC_ALL", locale = "French_France.1252")
#Sys.setlocale(category = "LC_ALL", locale = "French_France.utf8")

# loading the required packages
library(sf)
library(tidyverse)
library(units)
library(gplots)
library(LPCM)
library(IPMbook)

library(magrittr)
library(reshape)
library(reshape2)
library(popbio)
library(RColorBrewer)

library(raster)
library(mapview)
library(gdistance)


library(rjags)
library(jagsUI)



