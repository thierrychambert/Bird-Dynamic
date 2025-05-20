rm(list = ls(all.names = TRUE))
getwd()

# Load stuff
source("s00_Load_packages.R")
source("s01_Load_Data.R")

### shapefile france
shape_france <- st_read("../000_Inputs/France_shp/regions-20180101.shp")

# on rassemble les régions pour avoir la france en entier dans le mm shape
system.time( france <- st_union(shape_france) )

# en L93
system.time(france_L93 <- st_transform(france, 2154) )

## Data filtering
source("s02_Data_filtering.R")

#########################################################################
# Boucle sur toutes les espèces                      		      ###
# Chaque espèce à la fois : requis car dépend du foraging range	      ###
#########################################################################
list_esp <- BD_list$espece_BD[(1:22)]

## ATTENTION: Guiffette noire pas dispo dans le tableau de Woodward 2019 
## Exclude species without foranging range data
spp_exclude <- c("Guifette noire", "Puffin des Baléares")
"%ni%" <- Negate("%in%")
list_esp <- list_esp[list_esp %ni% spp_exclude]
list_esp
length(list_esp)


ct <- 0
esp <- list_esp[4]
esp

for(esp in list_esp){
  
  ct <- ct + 1 
  print("######################")
  
  ## Build Spp dataset and Define Foraging Range 
  source("s03_Build_Spp_Data_And_Foraging_Range.R")
  print(paste(ct, esp))
  
  
  ## Build colonies dataset ##### 
  source("s04_Buid_Colonies_Dataset.R")
  print( paste("nb colonies :", nrow(colonies_L93)) )
  print("######################")
  
  # buffer de X km autour de tes points 
  buf <- st_buffer(colonies_L93, set_units(max_foraging_range_km, "km"))
  buf$id <- 1:nrow(buf)
  buf$buffer_area <- st_area(buf)
  
  # aire de "terre = france" dans chaque buffer qui recoupe la "terre"
  system.time( 
    intersect_buf <- st_intersection(st_as_sf(france_L93), buf) %>% 
      mutate(intersect_area = st_area(.))   # create new column with shape area
  )
  
  ### Pourcentage #####
  
  # on récupère les info des chevauchements des buffers avec la terre
  st_geometry(buf) <- NULL # on retire la géométrie
  st_geometry(intersect_buf) <- NULL # on retire la géométrie
  intersect_buf <- intersect_buf[,c("id","intersect_area")]
  buf_ok <- merge(buf, intersect_buf, by = "id", all.x = T) # on merge
  
  # on remplace NA dans colonne "intersect_buf" par 0
  buf_ok$intersect_area[is.na(buf_ok$intersect_area)] <- 0
  
  # on calcule le pourcentage de terre
  buf_ok$pct_terre <- as.numeric(buf_ok$intersect_area)/as.numeric(buf_ok$buffer_area) # entre 0 et 1
  buf_ok$pct_terre
  
  # pourcentage de mer
  sea_area <- (1 - buf_ok$pct_terre)
  names(sea_area) <- colonies_L93$code_colonie
  
  ## Save the file
  save(sea_area, file = paste0("../000_Inputs/sea_area_rda/sea_area_", esp, ".rda"))
  print("######################")
  
} # End loop over species





