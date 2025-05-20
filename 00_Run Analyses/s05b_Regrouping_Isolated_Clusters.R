## Regroupement des cluster isolés

old_group <- colonies_L93$group

if(h_km > 0){
  # Identifier les clusters/groupes isolés
  iso_clus <- which(table(colonies_L93$group) == 1)
  
  # Distances entre paires de colonies
  colo_dist <- (st_distance(colonies_L93)) %>% set_units(., km) 
  diag(colo_dist) <- Inf
  
  ## Loop over 
  if(length(iso_clus) > 0){
    for(j in 1:length(iso_clus)){
      iso_colo_j <- which(old_group == iso_clus[j])  # identify the colony index from that isolated group
      colo_closest <- which.min(colo_dist[iso_colo_j,])         # identify its closest friend-colony
      
      # Apply its friend group number only if distance is less than XXXX
      if(colo_dist[iso_colo_j, colo_closest] < set_units(dist_regr_isol, km)){
        colonies_L93$group[iso_colo_j] <- colonies_L93$group[colo_closest]
      }
    } # j
  } # if 1
  
  ## Renumber groups
  colonies_L93$group <- colonies_L93$group %>% factor %>% as.numeric %>% as.factor 
}

colonies$group <- colonies_L93$group

## Number of groups
# n_group <- length(unique(colonies_L93$group))
n_group <- nlevels(colonies_L93$group)

### ATTENTION, ne pas reordonner les colonies ; sinon pb avec object shpa_dist
rm(old_group)
rm(colo_dist)

if(length(iso_clus) > 0){
  rm(j)
  rm(iso_colo_j)
  rm(colo_closest)
}

rm(iso_clus)