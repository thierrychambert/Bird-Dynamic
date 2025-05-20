## Appliquer filtre "année minimum"
effec01 <- effec01[effec01$an >= min_year,]
effec01 <- effec01[effec01$an <= last_year,]
dim(effec01)

## Select data only for that species (==> data spp) 
spp <- effec01[which(clean_names(effec01$espece) == clean_names(BD_list$espece_BD)[sel_esp]), ]
# esp <- unique(spp$espece)
esp <- BD_list$espece_BD[sel_esp] # ou esp2


# Define its max foraging range (in km)
max_foraging_range_km <- foraging_range_tbl[clean_names(foraging_range_tbl$Espece) == clean_names(esp), "Max_km"]
max_foraging_range_km
if(length(max_foraging_range_km) == 0) max_foraging_range_km <- 0

