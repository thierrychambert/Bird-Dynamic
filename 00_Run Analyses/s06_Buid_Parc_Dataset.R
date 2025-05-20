## Add Wind Farm #####
parcs_sf <- st_read("../000_Inputs/Parc_eoliens_FR_shape/FR203096_French OWF_A1.shp", quiet = TRUE)
parcs_sf <- parcs_sf[, c("NAME", "DEVEL", "geometry")]

## Sort wind farms : north --> south
parcs_sf <- parcs_sf %>% cbind(suppressWarnings(st_coordinates(st_centroid(.)))) %>%
  arrange(desc(Y), desc(X)) %>%
  dplyr::select(-X, -Y)

parcs_sf$num_parc <- 1:7
parcs_sf$nb_eol <- c(46, 62, 71, 64, 62, 80, 62)

# Transformation en Lambert 93
parcs_L93 <- parcs_sf %>%
  st_transform(2154)

n_parc <- nrow(parcs_L93)
