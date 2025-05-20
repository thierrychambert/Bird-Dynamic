########################
#### Look up Results
########################
rm(list = ls(all.names = TRUE))
source("s00_Load_packages.R")
# source("s04_Buid_Colonies_Dataset.R")
graphics.off()


## Choose species
BD_list <- read.csv2("../000_Inputs/liste_especes_BD.csv", na.strings = c("NA")) #, encoding = "latin1" / "utf8)
sel_esp = 4
esp <- BD_list$espece_BD[sel_esp] ; esp

esp
BD_list$Nom_latin[sel_esp]

## Load results
# load(file = paste0("../save_rda/AR95_results_IMPACT_", sprintf("%02d", sel_esp), "_", esp, ".rda"))
load(file = paste0("../save_rda/results_IMPACT_", sprintf("%02d", sel_esp), "_", esp, ".rda"))


## Define function to calculate the mode of a distribution
estimate_mode <- function(x) {
  d <- density(x, na.rm = TRUE)
  d$x[which.max(d$y)]
}



## Summary mortalities NATIONAL : median nb collisions annuelles
morta_Nat <- (morta_iter_gp %>% apply(., 1, sum))
round(median(morta_Nat), 2)

## Taille de pop Nationale
post_N0 <- post_n0 %>% apply(., 1, sum)
round(median(post_N0), -1)

## National RATE of collision (full distribution)
morta_Rate_Nat <- (median(morta_Nat)/median(post_N0))
paste0( round(median(morta_Rate_Nat), 5)*100 , "%")






## Summary mortalities SOUS-POPULATIONS: median nb collisions annuelles
mmm <- (apply(morta_iter_gp, 2, quantile, probs = 0.5)) %>% round(.,4)

# rate
mmr <- (apply(morta_iter_gp, 2, quantile, probs = 0.5) / apply(post_n0, 2, quantile, probs = 0.5)) %>% round(.,5)



## Max among pop
sel <- which.max(mmr)
morta_max <- as.vector(round(apply(morta_iter_gp, 2, quantile, probs = 0.5), 3)[sel])
morta_max

# Effectif pop (max morta)
round(apply(post_n0, 2, quantile, probs = 0.5)[sel], -0) %>% as.vector 

# Taux de morta (pop avec max morta)
(mmr[sel]*100) %>%
  round(., 4) %>% 
  paste0(., "%")




## Min among pop
min(mmm)
min(mmr)

## Nb pop avec 0 mortalités
length(which(mmm == 0))

## Min among pop (other than 0)
sel <- which.min(mmr)
as.vector(round(apply(morta_iter_gp, 2, quantile, probs = 0.5), 3)[sel])

# Effectif pop (min morta)
round(apply(post_n0, 2, quantile, probs = 0.5)[sel], -1) %>% as.vector 

# Taux de morta (pop avec min morta)
(mmr[sel]*100) %>%
  round(., 4) %>% 
  paste0(., "%")





## Load results
loc <- paste0("../output/2.2_Tables_Analyses IMPACT_détail par espèce/", sprintf("%02d", sel_esp), "_", esp)
#loc <- paste0("../output/2.2_Tables_Analyses IMPACT_détail par espèce/AR95_", sprintf("%02d", sel_esp), "_", esp)

tb0 <- read.csv2(file = paste0(loc, "/table_full_", esp, ".csv"), encoding = "latin1")
tb1 <- read.csv2(file = paste0(loc, "/table0_Summary Impact_", esp, ".csv"), encoding = "latin1")


## Cas fou de bassan
if(esp == "Fou de Bassan"){
  tb0[tb0$Pop == "National",] <- tb0[tb0$Pop == 2,]
  tb1[tb1$Pop == "National",] <- tb1[tb1$Pop == 2,]
tb0[3,"Pop"] <- tb1[3,"Pop"] <- "National"
}






### Impact NATIONAL
## QT 95%
as.numeric(tb0$QT095_impact[tb0$Pop == "National"])

## Median + CI
tb1$rel_impact[tb0$Pop == "National"]

## Proba d'extinction
as.numeric(tb0$PrExt_sc0[tb0$Pop == "National"])
as.numeric(tb0$PrExt_sc1[tb0$Pop == "National"])
as.numeric(tb0$RelDiff_PrExt[tb0$Pop == "National"])





### Impact SOUS-POPULATIONS
qt95 <- as.numeric(tb0$QT095_impact[tb0$Pop != "National"])

# Minimum impact
min(qt95)
sum(qt95 == min(qt95))

# Maximum impact
max(qt95)
sum(qt95 == max(qt95))

summary(qt95)




## Proba d'extinction
# Sc0
p_ext_sc0 <- as.numeric(tb0$PrExt_sc0[tb0$Pop != "National"])
sum(p_ext_sc0 > 0.05)

# Sc1
p_ext_sc1 <- as.numeric(tb0$PrExt_sc1[tb0$Pop != "National"])
sum(p_ext_sc1 > 0.05)


## Augmentation moyenne du risque d'extinction
diff_ext <- as.numeric(tb0$RelDiff_PrExt[tb0$Pop != "National"])
((mean(diff_ext[p_ext_sc1 > 0.05 & p_ext_sc0 > 0])) %>% round(.,4))*100






###################################################
### Impact National : distribution de probabilité
###################################################
N_sc0 <- apply(n_sc0, c(2,3), sum)
N_sc1 <- apply(n_sc1, c(2,3), sum)
dim(N_sc0)

rel_impact_Nat <- (N_sc0[ny_proj,] - N_sc1[ny_proj,])/N_sc0[ny_proj,]
length(rel_impact_Nat)

par(cex.lab = 1.7, cex.main = 1.5)
plot(density(rel_impact_Nat, na.rm = TRUE), lwd = 5,
     xlim = c(0.0, 1.0), 
     yaxt = 'n',
     main = paste0(esp, "\n(impact relatif national estimé)"),
     xlab = "Impact relatif", ylab = "")
mtext(text = "Densité de probabilité", side = 2, cex = 1.7, line = 2)


quantile(rel_impact_Nat, 0.95)
quantile(rel_impact_Nat, 0.5)
estimate_mode(rel_impact_Nat)
