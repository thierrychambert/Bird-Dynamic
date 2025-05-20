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

## Calculate Euclidean Distance
eucl_dist <- (st_distance(x = colonies00_L93, y = parcs_L93)) %>% set_units(., km)

## Calculate shortest path between colonies and wind farms
shortPath <- list()
goal <- st_coordinates(st_centroid(parcs_L93)) %>% suppressWarnings()

# Object to store shortest path distances between each colony and each parc
shpa_dist <- eucl_dist
shpa_dist[] <- NA

for(i in 1:nrow(colonies00_L93)){
  print(i)
  flush.console()
  origin <- st_coordinates(colonies00_L93)[i,] %>% t()
  
  shortPath[[i]] <- shortestPath(x = trCost,
                                 origin = origin, 
                                 goal = goal, 
                                 output = "SpatialLines") %>% suppressWarnings()
  #crs(shortPath) <- CRS("+init=epsg:2154") %>% suppressWarnings()
  
  shpa_dist[i,] <- st_length(st_as_sf(shortPath[[i]]), which = "Euclidean") 
} # i

# Add parc_name & colonie_code
rownames(shpa_dist) <- rownames(eucl_dist) <- colonies00_L93$code_colonie
colnames(shpa_dist) <- colnames(eucl_dist) <- parcs_L93$NAME

head(eucl_dist)
head(shpa_dist)
tail(shpa_dist)
dim(shpa_dist)

## Save the 2 disatnce objects as rda
save(list = c("eucl_dist","shpa_dist"), 
     file = "../000_Inputs/distances_colonies_parcs.rda"
)
