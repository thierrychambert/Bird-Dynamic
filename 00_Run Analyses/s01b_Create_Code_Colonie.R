## Build dataset : colonies00 of all species ##### 
colonies00 <- 
  dplyr::select(.data = effec00, 
                Lat, Lon,
                secteur,
                Colonie, sous_unite
  ) %>% 
  unique 
#%>%  arrange(., secteur, Colonie, sous_unite, )
dim(colonies00)

## Remove duplicates (same colonies00 with slightly different Lat/Lon info)
colonies00 <- colonies00[-which(duplicated(colonies00[ , c("Colonie", "sous_unite")])), ] %>%
  arrange(., secteur, desc(Lat + Lon), Colonie, sous_unite)
dim(colonies00)

## Sort colonies00 : north --> south
# colonies00 <- arrange(colonies00, desc(Lat))

## Create column "code_colonie"
colonies00$code_colonie <- paste0("colo_", sprintf("%03d", 1:nrow(colonies00)))

## Add code_colonie to "effec00
effec00 <- base::merge(x = effec00, y = colonies00, by = c("secteur", "Colonie", "sous_unite"), all = FALSE, )

## Clean Lat/Lon info
names(effec00)[names(effec00) == "Lat.y"] <- "Lat"
names(effec00)[names(effec00) == "Lon.y"] <- "Lon"
#effec00 <-rename(effec00, replace = c("Lat.y" = "Lat"))
#effec00 <- rename(effec00, replace = c("Lon" = "Lon.y"))
effec00$Lat.x <- effec00$Lon.x <- NULL
