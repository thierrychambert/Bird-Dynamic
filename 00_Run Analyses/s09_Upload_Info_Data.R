### Données de COMPTAGE et données SPATIALES #####

# Load General Data Stuff
source("s01_Load_Data.R")

## Data filtering
source("s02_Data_filtering.R")

## Build Spp dataset and Define Foraging Range 
source("s03_Build_Spp_Data_And_Foraging_Range.R")
esp
str(spp)

## Build colonies dataset ##### 
source("s04_Buid_Colonies_Dataset.R")
dim(colonies_L93)

## Filtre à propager : ne garder que les colonies ayant un effectif max > 0 
# (ce filtre a été appliqué lors de la création de l'objet colonies)
keep <- which(spp$code_colonie %in% colonies$code_colonie)
spp <- spp[keep, ]
rm(keep)

## Add buffer #####
buff_L93 <- st_buffer(colonies_L93, set_units(max_foraging_range_km, "km"))

## Clustering : Random clustering of Colonies (small spatial scale : 5km)
source("s05_Random_Clustering_Of_Colonies.R")
n_group

## Regrouping of siolated cluster (small spatial scale : 15km) 
source("s05b_Regrouping_Isolated_Clusters.R")
n_group

## Build Parc dataset ##### 
source("s06_Buid_Parc_Dataset.R")

## Get count data
source("s07_comptages_colonies.R")


### Comptages annuels #####
count_data <- group_counts
dim(count_data)
gp_data <- as.numeric(rownames(count_data))

# Proportion des colonies suivies par groupe, chaque année
PI <- ppa_yr

# Ajouter des NA's pour les projections démo
ny_proj <- 30

ny_data <- ncol(count_data)
ny_full <- ny_data + ny_proj
count_data[, (ny_data+(1:ny_proj))] <- NA
colnames(count_data)[ny_data+(1:ny_proj)] <- paste0("X", as.numeric(gsub("X", "", names(count_data)[ny_data])) + (1:ny_proj))
PI[, (ny_data+1:ny_proj)] <- 1


### VITAL RATES & SAD #####
# Select vital rates for that species
surv <- vital_rates[vital_rates$Espèce == esp, "Survie"]
fec <- vital_rates[vital_rates$Espèce == esp, "Fecund"]
pr <- prop_repro[prop_repro$Espèce == BD_list$Nom_latin[sel_esp], "PropRepro"]

# Get SAD factor "g"
source("f02_fn_build_Leslie.R")
g <- sum(pop_vector(nb_pair = 1000, s = surv, f = fec, pr = pr)[-1])/1000 ; g
