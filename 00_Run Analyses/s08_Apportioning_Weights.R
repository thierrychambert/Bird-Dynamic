### Create Relative Weights (AW) for Apportioning fatalities
## Get Shortest Path Distances
# source("../00_1_Prep_Code/i02_Calculate_ShortestPath.R")

## Load Distances (Shortest Path by Sea and Euclidian)
load(file = "../000_Inputs/distances_colonies_parcs.rda")
dim(shpa_dist) ; dim(eucl_dist) ; dim(colonies00)

# Distances colonies - parcs : on utilise la distance par la mer pour la plupart des espèces ; mais pour les espèces aux moeurs terrestres on utilise la distance euclidienne
moeurs_terrestres <- c("Goéland marin", "Goéland brun", "Goéland argenté", "Grand cormoran", "Mouette mélanocéphale", 
                       "Mouette rieuse", "Goéland cendré", "Sterne caugek", "Sterne pierregarin", "Guifette noire", 
                       "Goéland leucophée", "Mouette pygmée")

if(esp %in% moeurs_terrestres) tbl_dist <- eucl_dist else tbl_dist <- shpa_dist

## Filter colonies
tbl_dist <- tbl_dist[which(rownames(tbl_dist) %in% colonies$code_colonie), ]

## Ensure it is ordered by code_colonie
tbl_dist <- tbl_dist[order(row.names(tbl_dist)), ]

## Table for Apportioning
tbl_app <- as.data.frame(tbl_dist)

## Add info colony size
tbl_app$size <- colonie_counts[row.names(tbl_dist), "avg"]
#tbl_app$size <- colonie_counts[row.names(tbl_dist), "last"]

## Add info sea_area (Attention : info non disponible pour la Guifette noire)
if(esp != "Guifette noire") load(paste0("../000_Inputs/sea_area_rda/sea_area_", esp, ".rda"))
if(esp == "Guifette noire"){
  sea_area <- rep(1, nrow(colonies))  
  names(sea_area) <- row.names(tbl_dist)
} # 

tbl_app$sea_area <- sea_area[row.names(tbl_dist)]

## Avoid negative values for sea area
tbl_app$sea_area[tbl_app$sea_area < 0] <- min(tbl_app$sea_area[tbl_app$sea_area > 0])

# check: which(!( (names(sea_area)) %in% ((colonies$code_colonie))) )
# check: cbind((names(sea_area)), (colonies$code_colonie) )


## Define function to get relative weights
# rel_weight <- function(x) if(sum(x) == 0) 0 else x/sum(x)
rel_weight <- function(x) x/sum(x)

## Relation "risque relatif" et distance (d²)
#d <- seq(10, 100, length.out = 100)
#rr <- (1/rel_weight(d^2)) %>% rel_weight
# plot(x = d, y = rr, type = "l", ylab = "Risque relatif", xlab = "Distance (km)")




#####################################
#### COLONY SCALE
#####################################

## Make table of Absolute Weights
AW_dist <- AW_size <- AW_area <- as.data.frame(matrix(NA, nrow = nrow(tbl_app), ncol = n_parc, dimnames = list(rownames(tbl_app), colnames(shpa_dist))))

## Relative weight including or not pop size and sea area
for(i in 1:n_parc){
  AW_dist[,i] <- as.numeric(1/rel_weight((tbl_app[,i]^2))) 
  AW_size[,i] <- rel_weight(tbl_app$size)
  AW_area[,i] <- rel_weight(1/tbl_app$sea_area)
} # i
AW_dist %>% colSums 
AW_size %>% colSums
AW_area %>% colSums

(AW_dist * AW_size * AW_area) %>% colSums


## Exclude distances beyond foraging range(==> AW = 0)
lim_dist_km <- max_foraging_range_km
AW_dist[tbl_dist > set_units(lim_dist_km, km)] <- 0
AW_dist %>% colSums

## Total AW
AW_colo <- AW_dist
if(incl_pop_size) AW_colo <- AW_colo * AW_size
if(incl_sea_area) AW_colo <- AW_colo * AW_area
AW_colo %>% colSums

    
## Rescale AW : RELATIVE WEIGHT
RW_colo <- sapply(AW_colo, rel_weight) 
for(j in 1:ncol(RW_colo)) RW_colo[,j][is.nan(RW_colo[,j])] <- 0
rownames(RW_colo) <- rownames(tbl_app)
RW_colo %>% colSums

## Map it
# source("z02_make_map_apportioning.R")
# j = 5
# make_map_appo_colonies(RW = RW_colo, j = j, black = TRUE)



#####################################
#### CLUSTER/GROUP SCALE
#####################################
group_dist <- as.data.frame(matrix(NA, nrow = n_group, ncol = n_parc, dimnames = list(levels(colonies$group), colnames(shpa_dist))))
group_size <- group_area <- c()

for(gg in 1:n_group){

  ## Distance moyenne au group pondérée par la taille de colonie
  group_dist[gg,] <- 
    (rel_weight(tbl_app[colonies$code_colonie[colonies$group == gg], "size"]) * 
     tbl_app[colonies$code_colonie[colonies$group == gg], 1:n_parc]
    ) %>% 
    colSums
  
  ## Relative weight Mean pop size of the group
  group_size[gg] <- sum(tbl_app[colonies$code_colonie[colonies$group == gg], "size"])
  
  ## Mean sea area of the group, weighted by colony size
  group_area[gg] <- 
  (rel_weight(tbl_app[colonies$code_colonie[colonies$group == gg], "size"]) *
      tbl_app[colonies$code_colonie[colonies$group == gg], "sea_area"]
   ) %>% 
    sum
  
} # gg
rm(gg)

sum(group_dist)
sum(group_size)
sum(group_area)


## Make table of Absolute Weights
AW_dist <- AW_size <- AW_area <- as.data.frame(matrix(NA, nrow = n_group, ncol = n_parc, dimnames = list(levels(colonies$group), colnames(shpa_dist))))
dim(AW_size)

for(i in 1:n_parc){
  AW_dist[,i] <- as.numeric(1/rel_weight((group_dist[,i]^2)))
  AW_size[,i] <- rel_weight(group_size)
  AW_area[,i] <- rel_weight(1/group_area)
} # i

AW_dist %>% colSums
AW_size %>% colSums
AW_area %>% colSums

(AW_dist * AW_size * AW_area) %>% colSums

## Exclude distances beyond foraging range(==> AW = 0)
lim_dist_km <- max_foraging_range_km
AW_dist[group_dist > lim_dist_km] <- 0
AW_dist %>% colSums

## Total AW
AW_group <- AW_dist
if(incl_pop_size) AW_group <- AW_group * AW_size
if(incl_sea_area) AW_group <- AW_group * AW_area
AW_group %>% colSums

## Rescale AW : RELATIVE WEIGHT
RW_group <- sapply(AW_group, rel_weight) 
for(j in 1:ncol(RW_group)) RW_group[,j][is.nan(RW_group[,j])] <- 0
rownames(RW_group) <- sort(unique(colonies$group))

## Ensure proper order by group number
RW_group <- RW_group[order(as.numeric(rownames(RW_group))),]
RW_group %>% colSums

## Map it
# source("z02_make_map_apportioning.R")
# j = 5
# make_map_appo_groups(RW = RW_group, j = j, black = TRUE)


# map_groups()
