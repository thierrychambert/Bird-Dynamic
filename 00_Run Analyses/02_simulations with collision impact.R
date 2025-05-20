options(digits = 4, scipen = 999)
rm(list = ls(all.names = TRUE))
source("s00_Load_packages.R")
graphics.off()

## Define how many simulations (iterations) to run
n_iter <- 1e3

## Choose species
sel_esp = 4
BD_list <- read.csv2("../000_Inputs/liste_especes_BD.csv", na.strings = c("NA")) # encoding = "latin1")
esp <- BD_list$espece_BD[sel_esp] ; esp

## PRINT SPECIES
print(esp)

## load jags output
load(file = paste0("../save_rda/results_", sprintf("%02d", sel_esp), "_", esp, ".rda"))

## Upload the rest of the info (CLEAN)
source("s09_Upload_Info_Data.R")
max_foraging_range_km 
if(esp == "Guifette noire") max_foraging_range_km <- 100
if(esp == "Sterne de Dougall") max_foraging_range_km <- 110
if(esp == "Mouette mélanocéphale") max_foraging_range_km <- max_foraging_range_km*4
if(esp == "Mouette rieuse") max_foraging_range_km <- max_foraging_range_km*4

## Map colony groups
source("f01_fn_map_groups.R")
gp_col <- make_gp_col()
colonies_L93$gp_col <- gp_col[as.numeric(colonies_L93$group)]
colonies$gp_col <- gp_col[as.numeric(colonies$group)]
map_groups(parcs = FALSE)


### APPORTIONNING ###
## Get Relative Weights for Apportioning
incl_pop_size <- TRUE
incl_sea_area <- TRUE
source("s08_Apportioning_Weights.R")

## Map Apportioning
source("z02_make_map_apportioning.R")

j = 4
parcs_L93$NAME[j]
make_map_appo_groups(RW = RW_group, j = j, black = FALSE)      # by group
# make_map_appo_colonies(RW = RW_colo, j = j, black = TRUE)   # by colony
RW_group[RW_group > 0]

###############################################################################
#### IMPACT SIMULATIONS                                                   #####
#### WITH PARAMETER UNCERTAINTY and DEMOGRAPHIC STOCHASTICITY             #####
##############################################################################
## 1. Extract full posterior from UNIMPATED demographic analyses (Sc0 trajectories) 
fullpost <- rbind(out$samples[[1]], out$samples[[2]], out$samples[[3]])
dim(fullpost)

## 2. Select posterior of target parameters
# Initial subpop size : prospective period
paste0("n_TOT[", 1:n_group, ",", ny_data+1, "]")
sel <- which(colnames(fullpost) %in% paste0("n_TOT[", 1:n_group, ",", ny_data+1, "]"))
post_n0 <- fullpost[,sel]

# Annual subpop growth rate  : prospective period
sel <- which(colnames(fullpost) %in% paste0("growth_i_proj[", 1:n_group,"]"))
post_lam <- fullpost[,sel]

## Draw values of initial subpop sizes
#n0_distri <- apply(post_n0, 2, sample, size = n_iter, replace = TRUE)

## 3. Extract results from Bird Risk
saison <- read.csv2(file = paste0("../000_Inputs/Saison_Especes.csv"))
sais <- saison[saison$Espèce == esp, -1]


source("s10_Extract_Collision_Risk.R")
colMeans(morta_distri)
head(morta_iter_gp)
(morta_iter_parc) %>% colMeans


## Buil object to store subpop sizes (under both scenarios)
n_sc0 <- array(NA, dim = c(n_group, ny_proj, n_iter))
morta_rate <- array(NA, dim = c(n_group, n_iter))

## Fill initial subpop sizes
spl <- sample(nrow(post_n0), size = n_iter, replace = TRUE) 
#n_sc0[,1,] <- round(t(post_n0[spl,]))
n_sc0[,1,] <- apply( round(t(post_n0[spl,])) , c(1,2), max, 1)
n_sc1 <- n_sc0
dim(n_sc0)

## Draw iterations for subpop growth rates
#### ATTENTION : DO NOT resample 'spl" here ; needs to keep correlation structure ; otherwise UNCERTAINTY is underestimated
lam <- t(post_lam[spl,])
dim(lam)
# lam[,1]

##### Run #####-
# mu_m <- apply(morta_iter_gp, 2, median) # without uncertainty on mortality
source("f05_fn_predict_impact.R")

### Loop over iterations
time_sim <- system.time( 
  pred_imp <- predict_impact() 
) # time_sim
time_sim

## Extract output
n_sc0 <- pred_imp$n_sc0
n_sc1 <- pred_imp$n_sc1
morta_rate <- pred_imp$morta_rate



### Save run
save(list = ls(), file = paste0("../save_rda/results_IMPACT_", sprintf("%02d", sel_esp), "_", esp, ".rda"))

rm(pred_imp)
