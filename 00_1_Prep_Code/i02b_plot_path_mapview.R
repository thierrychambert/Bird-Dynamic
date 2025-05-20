rm(list = ls(all.names = TRUE))
source("s01_Load_Pacakges_and_Data.R")

## Choose species
sel_esp = 4
BD_list$espece_BD[sel_esp]

## Data filtering
min_year = 2006      # Remove years before... 
rem_na_colo = TRUE   # Remove data (colonies) with NAs & 0 only ; YES/NO ?
min_Lat = 0          # Minimum Latitude (remove colonies south of XX°)

## Build Spp dataset and Define Foraging Range 
source("s02_Build_Spp_Data_And_Foraging_Range.R")
esp ; esp2
dim(spp)

## Build colonies dataset ##### 
source("s03_Buid_Colonies_Dataset.R")
dim(colonies_L93)

## Build Parc dataset ##### 
source("s04_Buid_Parc_Dataset.R")

## Load COST-PATH data
load(file = "trCost.rda")
load(file = "./distances_colonies_parcs.rda")


###############################################
## Select
i = 1   # a colony
j = 1     # a wind farm
parcs_L93[j,"NAME"]

## Calculate shortest path between colonies and wind farms
#shortPath <- list() 
goal <- st_coordinates(st_centroid(parcs_L93)) %>% suppressWarnings()
origin <- st_coordinates(colonies_L93)[i,] %>% t()

shortPath <- shortestPath(x = trCost,
                          origin = origin, 
                          goal = goal, 
                          output = "SpatialLines") %>% suppressWarnings()


## Plot shortest path
#plot(rast_cost)
#plot(SpatialPoints(goal)[j], add=TRUE, pch=15, cex = 3, col="black")
#plot(SpatialPoints(origin), add=TRUE, pch=20, cex = 3, col="red")
#plot(shortPath[j], add=TRUE, col = "blue", lwd = 2)


## Look on mapview
mapview(colonies_L93[i,],
        col = NULL,
        col.regions = 2,
        alpha.regions = 1, 
        legend = FALSE, 
        cex = 3
) +
  
  mapview(shortPath[j]) + 
  
  mapview(parcs_L93[j,], grid = FALSE,
          col = 1,
          alpha = 1,
          col.regions = "green",
          legend = FALSE, 
          cex = NULL
  )



(eucl_dist)[i,j]
(shpa_dist)[i,j]
