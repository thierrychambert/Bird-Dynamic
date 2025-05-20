### Cas des espèces SANS résultats BIRD RISK : se baser sur une espèce PROXY
if(sel_esp > 17){
  
  ### Load ratio mortalité de l'espèce proxy 
  if(esp == "Goéland leucophée") load(file = paste0("../000_Inputs/ratio_morta_", sprintf("%02d", which(BD_list$espece_BD == "Goéland argenté")), "_", "Goéland argenté", ".rda") )
  if(esp == "Macareux moine") load(file = paste0("../000_Inputs/ratio_morta_", sprintf("%02d", which(BD_list$espece_BD == "Pingouin torda")), "_", "Pingouin torda", ".rda") )
  head(ratio_morta)
  
  
  ## Corriger le ratio de risque individuel annualisé d'après la durée d'exposition (nb mois) par an 
  if(esp == "Goéland leucophée") sais_proxy <- saison[saison$Espèce == "Goéland argenté", -1]
  if(esp == "Macareux moine") sais_proxy <- saison[saison$Espèce == "Pingouin torda", -1]
  fact_corr <- length(which(sais %in% c("B","R","T","M"))) / length(which(sais_proxy %in% c("B","R","T","M")))
  ratio_morta <- fact_corr*ratio_morta
  
  ## Apply the ratio to estimate missing mortalities 
  morta_distri <- matrix(NA, nrow = n_iter, ncol = n_parc, dimnames = list(NULL, parcs_L93$NAME))

  for(k in (1:n_parc)){
    morta_distri[,k] <- (ratio_morta[,k] * (sum(group_size[which(AW_group[k] > 0)]) * parcs_L93$nb_eol[k])) %>% sample(., size = n_iter)
  } # k
  
  ### PROXY SPECIES ##################################-
  # Océanite tempête > puffin des Baléares
  # Goéland leucophée > Goéland argenté
  # Fulmar boréal >  puffin des Baléares
  # Puffin des Anglais > puffin des Baléares
  # Macareux moine > Pingouin Torda (ou Guillemot)
  ####################################################-
  
  
  
} else {
  ### Cas des espèces AVEC résultats BIRD RISK
  
  ### 1. SPECIES : Select Collision Risk for a given species (load)
  collision <- read.csv(file = paste0("../000_Inputs/data mortality/", sprintf("%02d", sel_esp), "_",  "CollisionRisk_", esp,".csv"))
  
  ### Make the table to store distribution of collision risk for that species (Iter x Parc)
  #morta_distri <- matrix(NA, nrow = max(collision$iter), ncol = n_parc, dimnames = list(NULL, parcs_L93$NAME))
  morta_distri <- matrix(NA, nrow = n_iter, ncol = n_parc, dimnames = list(NULL, parcs_L93$NAME))
  
  
  ######################################################################-
  ### 2. PARC : Loop over windfarms (Parc)
  for(kk in 1:n_parc){
    
    sel <- which(collision$Parc == parcs_L93$NAME[kk])
    
    if(length(sel) == 0){} else{
      cr <- collision[collision$Parc == parcs_L93$NAME[kk],]
      
      ### 3. OPTION : select the option to keep
      tmp <- aggregate(Estimate ~ Option, data = cr, mean)
      sel_option <- tmp$Option[which.max(tmp$Estimate)]
      cr <- cr[cr$Option == sel_option,]

      ## Months when only local individuals are present
      # cr_sel <- cr[cr$Month %in% months_breeding,]
      sel_local <- which(sais %in% c("B", "R"))
      cr_sel <- cr[cr$Month %in% sel_local,]
      cr_local <- aggregate(Estimate ~ iter, data = cr_sel, sum) # sum over the Breeding season
      cr_local <- (cr_local$Estimate * rep(1, n_iter))
      rm(cr_sel) ; rm(sel_local)
      
      ## Months when both local and migrants are present (X% affects local populations)
      # cr_sel <- cr[cr$Month %in% months_winter,]
      sel_mix <- which(sais %in% c("M", "T"))
      if(length(sel_mix > 0)){
        cr_sel <- cr[cr$Month %in% sel_mix,]
        cr_mix <- aggregate(Estimate ~ iter, data = cr_sel, sum) # sum over the Breeding season
        # Incertitude sur la proportion des collisions concernant les populations locales
        min_PROP_LOCAL <- min((mean(cr_local)/mean(cr_mix$Estimate)), 1, na.rm = TRUE) # si il y a moins de collisions en période hors-repro qu'en repro, on considère que hors période repro les collisions affectent à 100% les individus locaux  
        max_PROP_LOCAL <- 1
        cr_mix <- (cr_mix$Estimate * runif(n_iter, min_PROP_LOCAL, max_PROP_LOCAL))
      } else {
        cr_mix <- 0
      } # close if
      
      
      ## Draw many (n_iter) values (= shuffled distribution)
      cr_mix <- sample(cr_mix, size = n_iter, replace = TRUE)
      cr_local <- sample(cr_local, size = n_iter, replace = TRUE)
      
      ### Fill the table (Iter x Parc)
      # morta_distri[ , parcs_L93$NAME[kk]] <- cr_annual$Estimate
      morta_distri[ , parcs_L93$NAME[kk]] <- cr_local + cr_mix
      
    } # close if
    rm(sel)
  } # kk
  dim(morta_distri)
  head(morta_distri)
  
  ### Approximation des mortalités pour les qqs cas de données manquantes
  # Cas 1 : Cormoran huppé, parc d'Yeu-Noirmoutier
  # Cas 2 : Sterne de Dougall, parc de Dunkerque
  # Cas 3 : Goéland cendré, parc de Dunkerque
  
  
  if(esp == "Cormoran huppé" | esp == "Sterne de Dougall" | esp == "Goéland cendré"){
    
    ## Sélectionner le parc éolien concerné (données mortalités manquantes)
    if(esp == "Cormoran huppé") k = which(parcs_L93$NAME == "YEU - NOIRMOUTIER")
    if(esp == "Sterne de Dougall") k = which(parcs_L93$NAME == "DUNKERQUE")
    if(esp == "Goéland cendré") k = which(parcs_L93$NAME == "DUNKERQUE")
    
    ## Ratio : Nombre de mortalité / (nb d'éoliennes*nb d'individus exposés)
    ratio_morta <- matrix(NA, nrow = n_iter, ncol = n_parc-1)
    ctkk = 0
    for(kk in (1:n_parc)[-k]){
      ctkk = ctkk + 1
      ratio_morta[,ctkk] <- (morta_distri[ , kk]) / (sum(group_size[which(AW_group[kk] > 0)]) * parcs_L93$nb_eol[kk])
    } # kk
    rm(ctkk) ; rm(kk)
    head(ratio_morta)
    
    ## Remove Inf and NA's
    ratio_morta <- (ratio_morta[(ratio_morta != Inf) & !is.na(ratio_morta)])

    ## Apply the ratio to estimate missing mortalities 
    morta_distri[,k] <- (ratio_morta * (sum(group_size[which(AW_group[k] > 0)]) * parcs_L93$nb_eol[k])) %>% sample(., size = n_iter, replace = TRUE)
    rm(ratio_morta)
    
  } # close if/esp
  
}  # close if/esp
dim(morta_distri)

## Replace remaining NA by ZERO (these are correct !)
morta_distri[is.na(morta_distri)] <- 0
dim(morta_distri)
head(morta_distri)

## Draw many (n_iter) values for each parc (= shuffled distribution)
morta_iter_parc <- apply(morta_distri, 2, sample, size = n_iter, replace = TRUE)
head(morta_iter_parc)
dim(morta_iter_parc)

## Apportioning mortalities per cluster or group - dim : Iterations X Groups
morta_iter_gp <- morta_iter_parc %*% t(RW_group)
head(morta_iter_gp)
dim(morta_iter_gp)




##################################################################################################-



### Calculer le ratio de mortalité / individu exposé / éolienne, s'il s'agit d'une espèce proxy pour une autre espèce
if(esp == "Goéland argenté" | esp == "Pingouin torda" | esp == "Puffin des Baléares"){
  
  ## Ratio : Nombre de mortalité / (nb d'éoliennes*nb d'individus exposés)
  ratio_morta <- matrix(NA, nrow = n_iter, ncol = n_parc)
  ctkk = 0
  for(kk in (1:n_parc)){
    ctkk = ctkk + 1
    ratio_morta[,ctkk] <- (morta_distri[ , kk]) / (sum(group_size[which(AW_group[kk] > 0)]) * parcs_L93$nb_eol[kk])
  } # kk
  rm(ctkk) ; rm(kk)
  head(ratio_morta)
  dim(ratio_morta)
  
## Parc without info on collision risk ("Inf", because French population are not exposed) : use the average collision risk 
  sel <- which(ratio_morta[1,] != Inf)
  ratio_morta[, -sel] <- rowMeans(ratio_morta[,sel])
  head(ratio_morta)
  
  ## Save it
  save(ratio_morta, file = paste0("../000_Inputs/ratio_morta_", sprintf("%02d", sel_esp), "_", esp, ".rda") )
  
} # close if/esp


