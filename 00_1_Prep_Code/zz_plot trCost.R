rm(list = ls(all.names = TRUE))

# Load packages and data
source("s00_Load_packages.R")
source("s01_Load_Data.R")

## Make colonies00 as Lambert 93
colonies00_sf <- st_as_sf(colonies00, coords = c("Lon", "Lat"), 
                          crs = 4326, agr = "constant")
colonies00_L93 <- colonies00_sf %>% st_transform(2154)

## Build Parc dataset ##### 
source("s06_Buid_Parc_Dataset.R")

## Load the Cost Surface (land/sea) raster
# source("i01_Create_Raster_Cost_LandSea.R")
system.time( load(file = "../000_Inputs/trCost.rda") )

par(mar = c(0,0,0,0))
plot(rast_cost)

