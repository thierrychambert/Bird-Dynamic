## Build colonies dataset ##### 
colonies <- 
  dplyr::select(.data = spp, 
         Lat, Lon,
         secteur,
         code_colonie,
         Colonie, sous_unite,
         Espece = espece
  ) %>% 
  unique %>%
  arrange(., code_colonie, Colonie, sous_unite)

## Add column "effectifs"
max_round <- function(x, rd=0) round(max(x), rd)
mean_round <- function(x, rd=0) round(mean(x), rd)
spp$EFF <- spp$EFF_Moy
spp$EFF[is.na(spp$EFF_Moy)] <- 0

## Effectif aggrégé par colonie
Eff <- 
  aggregate(EFF ~  code_colonie + sous_unite + Colonie, 
            data = spp, FUN = mean_round, rd = 1, na.action = NULL) %>% 
  arrange(., code_colonie, Colonie, sous_unite)

colonies$Eff <- Eff$EFF

## Check
colonies %>% arrange(., desc(Eff)) %>% head

## Remove colonies with effectif max = 0 
colonies <- colonies[which(colonies$Eff > 0), ]
colonies %>% arrange(., desc(Eff)) %>% dplyr::select(Colonie, Eff) %>% head
dim(colonies)

## Re-order columns : colonies <- colonies %>% dplyr::select(Eff, everything())

## Have a look
head(colonies)
tail(colonies)
arrange(colonies, desc(Eff)) %>% dplyr::select(code_colonie, Colonie, Eff) %>% head

## Drop NA's (lat/lon)
colonies <- colonies %>% 
  drop_na(Lat, Lon)

# Remove colonies south of 45° (291 > 289)
colonies <- colonies[colonies$Lat > min_Lat, ]
dim(colonies)

## Sort colonies : north --> south
colonies <- arrange(colonies, desc(Lat))
# colonies <- arrange(colonies, desc(scale(Lat*1 + Lon*0)))

## Make it a sf object
colonies_sf <- st_as_sf(colonies, coords = c("Lon", "Lat"), 
                        crs = 4326, agr = "constant")

## Transformation en Lambert 93 : requis pour créer les buffer ensuite
colonies_L93 <- colonies_sf %>% st_transform(2154)
colonies_L93$Lat <- colonies$Lat
colonies_L93$Lon <- colonies$Lon

n_colo <- nrow(colonies)


## Filtre à propager : ne garder que les colonies ayant un effectif max > 0 
# (ce filtre a été appliqué lors de la création de l'objet colonies)
keep <- which(spp$code_colonie %in% colonies$code_colonie)
spp <- spp[keep, ]
rm(keep)
