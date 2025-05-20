rm(list = ls(all.names = TRUE))

# Load packages and data
source("s00_Load_packages.R")
source("s01_Load_Data.R")
source("s06_Buid_Parc_Dataset.R")


## Make colonies00 as Lambert 93
colonies00_sf <- st_as_sf(colonies00, coords = c("Lon", "Lat"), 
                          crs = 4326, agr = "constant")
colonies00_L93 <- colonies00_sf %>% st_transform(2154)

### Shapefile with countries
World_map <- st_read("ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")
France <- World_map[World_map$NAME_EN == "France", ]

# Transform in L93 projection
France_L93 <- st_transform(France, crs = 2154)

# Take a subset of the raster based on the extent of colonies and parcs locations
ext_col <- rbind(colonies00_L93[,"geometry"], parcs_L93[,"geometry"]) %>% st_bbox
#st_bbox(colonies00_L93)

ext_col <- extent(c(
  ext_col$xmin-5000,
  ext_col$xmax+5000,
  ext_col$ymin-5000,
  ext_col$ymax+5000))
ext_col

# create a raster with this extent which will serve as the cost surface
rast_cost <- raster(ext_col)

# pixel size (m) : 1000 m.
res(rast_cost) <- 1000

# transfer the coordinate system to the raster
crs(rast_cost) <- CRS("+init=epsg:2154") %>% suppressWarnings()

# Now you can add data to the cells in your raster to mark the ones that fall within your polygon.
(rast_cost[as_Spatial(France_L93),] <- 10000) %>% suppressWarnings()

## Create cost surface : "land" = high cost, sea = low cost
#rast_cost[rast_cost == 1] <- 10000
rast_cost[is.na(rast_cost)] <- 1
unique(rast_cost)
# plot(rast_cost)

## Produce transition matrices, and correct because 8 directions
# system.time( trCost <- transition(1/rast_cost, mean, directions=8) ) # 16sec
# system.time( trCost <- geoCorrection(trCost, type="c") ) # 3 sec
# save(list = c("trCost_1000m","rast_cost"), file = "trCost.rda")


