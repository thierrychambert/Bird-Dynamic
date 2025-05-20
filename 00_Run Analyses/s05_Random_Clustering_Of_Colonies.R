## Clustering using Mean Shift Algorithm
# more info here : https://towardsdatascience.com/the-5-clustering-algorithms-data-scientists-need-to-know-a36d136ef68

## Define range of Lat/Lon values
rg <- (colonies[,c("Lon", "Lat")]) %>% apply(., 2, range)

## Convert the bandwith in terms of % of Lat/Lon range extent 
# Latitude: 1 deg = 110.574 km. 
h_Y <- h_km / (diff(rg[,"Lat"]) * 110.574) 

# Longitude: 1 deg = 111.320*cos(latitude) km.
deg2rad <- function(deg) {(deg * pi) / (180)}
h_X <- h_km / (diff(rg[,"Lon"]) * cos(deg2rad(mean(rg[,"Lat"]))) * 111.320) 

## Apply the Mean Shift Algorithm (from LPCM package)
if(h_km > 0){
  ms_res <- st_coordinates(colonies_L93) %>% ms(., h = c(h_X, h_Y), plot = FALSE)
  colonies$group <- ms_res$cluster.label
}else{
  colonies$group <- 1:nrow(colonies)
}

## Make "group number" as factor 
colonies$group <- as.factor(colonies$group)

## Add info "group number" to colonies DF
colonies_L93$group <- colonies$group

# check_order(colonies$code_colonie, colonies_L93$code_colonie)

## Number of groups
n_group <- nlevels(colonies_L93$group)
# length(unique(colonies_L93$group))


### ATTENTION, ne pas reordonner les colonies ; sinon pb avec object shpa_dist


rm(rg)
rm(h_Y)
rm(h_X)
