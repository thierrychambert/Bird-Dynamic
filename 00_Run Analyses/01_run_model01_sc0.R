options(digits = 4, scipen = 999)
rm(list = ls(all.names = TRUE))

# Load stuff
source("s00_Load_packages.R")
source("s01_Load_Data.R")
dim(effec00)

## Choose species
sel_esp = 4
BD_list$espece_BD[sel_esp]

### Données de COMPTAGE et données SPATIALES #####
## Data filtering
source("s02_Data_filtering.R")

## Build Spp dataset and Define Foraging Range 
source("s03_Build_Spp_Data_And_Foraging_Range.R")
esp

## Build colonies dataset ##### 
source("s04_Buid_Colonies_Dataset.R")
n_colo

## Add buffer #####
buff_L93 <- st_buffer(colonies_L93, set_units(max_foraging_range_km, "km"))

## Clustering : Random clustering of Colonies (small spatial scale : 5km)
source("s05_Random_Clustering_Of_Colonies.R")

## Regrouping of siolated cluster (small spatial scale : 15km) 
source("s05b_Regrouping_Isolated_Clusters.R")
n_group


## Build Parc dataset ##### 
source("s06_Buid_Parc_Dataset.R")

## Map colony groups
source("f01_fn_map_groups.R")
gp_col <- make_gp_col()
colonies_L93$gp_col <- gp_col[as.numeric(colonies_L93$group)]
colonies$gp_col <- gp_col[as.numeric(colonies$group)]
map_groups()

## Get count data
source("s07_comptages_colonies.R")
head(group_counts)
(colonie_counts)

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

### BAYESIAN ANALYSIS #####
# Define max.N0 for the model
max.N0 <- round(sum(count_data[,1]/(PI[,1]), na.rm = TRUE)*1.5) ; max.N0

# Data bundle
jags.data <- list(y = count_data, I = nrow(count_data), T = ny_full, ny_data = ny_data, 
                  ny_proj = ny_proj, max.N0 = max.N0, PI=PI, g=g)


# Write JAGS model file ####
source("m01_model_no_mortality _ all age classes.R")

# Initial values
inits <- function(){
  list(
    N = round(sum(count_data[,1], na.rm = TRUE)*(1+runif(1,0,0.5))),
    n = round(count_data/PI)
  )
}

# Parameters monitored
parameters <- c("mu.lam_0", "b0", "b1", "N", "n", "n_TOT", "N_TOT", "gamma", "sig.y", 
                "growth_i_data", "growth_i_proj", "growth_i_full", 
                "growth_N_data", "growth_N_proj","growth_N_full")

# MCMC settings
na <- 5000
nb <- 1000
ni <- 20000 + nb
nc <- 3
nt <- 5

# Call JAGS
system.time(
  out <- jags(data = jags.data, inits = NULL, 
              parameters.to.save = parameters, 
              model.file = "model_01.txt", 
              n.chains = nc, n.iter = ni, 
              n.burnin = nb, n.adapt = na,
              n.thin = nt, DIC = FALSE, 
              parallel = FALSE
  ) # close jags fn
) # clos time fn


### Outputs #####
# traceplot(out, parameters = c("mu.lam_0", "b0", "b1"))
dim(out$summary)
str(out$sims.list)
out$summary[, c(1,3,7,8:9)] %>% round(.,3) %>% head(., 10)

## Rhat & Neff summary
out$summary[, "Rhat"] %>% summary
out$summary[, "n.eff"] %>% summary


### Save run
 save(list = ls(), file = paste0("../save_rda/results_", sprintf("%02d", sel_esp), "_", esp, ".rda"))

