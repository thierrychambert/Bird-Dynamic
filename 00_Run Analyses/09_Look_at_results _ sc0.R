## Load existing results ?
options(digits = 4, scipen = 999)
rm(list = ls(all.names = TRUE))
source("s00_Load_packages.R")

## Choose species
BD_list <- read.csv2("../000_Inputs/liste_especes_BD.csv", na.strings = c("NA"))
sel_esp = 3
esp <- BD_list$espece_BD[sel_esp]
esp

## Load results
load(file = paste0("../save_rda/results_", esp, ".rda"))

## Infos espèce
esp
n_colo
n_group


## Plot results
source("f03_fn_plot_traj.R")

## Total population (toutes classes d'âge)
x11()
plot_traj(est_n = out$mean$n_TOT, lci_n = out$q2.5$n_TOT, uci_n = out$q97.5$n_TOT,
          group_col = 1:n_group, title = esp)

## Tendance nationale : toutes classes d'âge
x11()
plot_traj(est_n = t(out$mean$N_TOT), lci_n = t(out$q2.5$N_TOT), uci_n = t(out$q97.5$N_TOT),
          group_col = rgb(0,0,0), title = esp)



## Nb breeding pairs
x11()
plot_traj(est_n = out$mean$n, lci_n = out$q2.5$n, uci_n = out$q97.5$n,
          group_col = 1:n_group, title = esp)

## Tendance nationale : nb breeding pairs
x11()
plot_traj(est_n = t(out$mean$N), lci_n = t(out$q2.5$N), uci_n = t(out$q97.5$N),
          group_col = rgb(0,0,0), title = esp)


## Taux de croissance annuel (national)
### Estimated lambda at national scale (from the run)
out$mean$growth_N_full

### Estimated lambda from Leslie matrix
source("f02_fn_build_Leslie.R")
build_Leslie(s = surv, f = fec) %>% lambda

## Infos espèce
esp
max_foraging_range_km
nrow(colonies)
n_group
(last_year + ny_proj)
