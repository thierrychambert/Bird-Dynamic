# Function to make a map to visualize apportioning of windfarm fatalities 

#######################-
### Among COLONIES #####
#######################-
make_map_appo_colonies <- function(RW, j = 1, black = TRUE){
  
  ## Reorder based on code_colonies
  RW <- RW[colonies_L93$code_colonie,] 
  
  ## Add Relative Weight to DF
  colonies_L93$RW <- RW[,j]
  colonies_L93$YesNo <- (RW[,j] %>% as.logical)
  if(black) colonies_L93$gp_col[!colonies_L93$YesNo] <- "#000000"
  
  # If all TRUE or all FALSE in "YesNo" -- no zcol
  if(length(unique(colonies_L93$YesNo)) == 1){
    
    # if(unique(colonies_L93$YesNo)) col_regions <- 2 else col_regions <- 1
    if(unique(colonies_L93$YesNo)) cex <- "RW" else cex <- 3
    
    mapview(colonies_L93, grid = FALSE,
            col = NULL,
            alpha = NULL,
            col.regions = colonies_L93$gp_col,
            alpha.regions = 1, 
            legend = FALSE, 
            cex = cex
    )  + 
      
      mapview(parcs_L93[j,], grid = FALSE,
              col = 1,
              alpha = 1,
              col.regions = "green",
              alpha.regions = 10, 
              legend = FALSE, 
              cex = NULL
      ) 
    
    # If some TRUE and some FALSE in "YesNo" : include zcol
  }else{
    mapview(colonies_L93, grid = FALSE,
            col = NULL,
            alpha = NULL,
            col.regions = colonies_L93$gp_col,
            zcol = "YesNo",
            alpha.regions = 1, 
            legend = FALSE, 
            cex = "RW"
    )  + 
      
      mapview(parcs_L93[j,], grid = FALSE,
              col = 1,
              alpha = 1,
              col.regions = "green",
              alpha.regions = 10, 
              legend = FALSE, 
              cex = NULL
      ) 
  } # end if
  
  
} # end function











#######################-
### Among GROUPS #####
#######################-

make_map_appo_groups <- function(RW, j = 1, black = TRUE){
  
  ## Create groups dataframe
  groups <- data.frame(code_group = sort(unique(colonies$group)))
  groups$Lat <- groups$Lon <- groups$gp_col <- NA
  
  ## Create avg Lat/Lon data of groups, and add color of group
  for(gg in 1:n_group){
    groups$Lat[gg] <- colonies[colonies$group == gg, "Lat"] %>% mean
    groups$Lon[gg] <- colonies[colonies$group == gg, "Lon"] %>% mean
    groups$gp_col[gg] <- unique(colonies[colonies$group == gg, "gp_col"])
  }
  
  ## Transformation en Lambert 93
  groups_L93 <- st_as_sf(groups, coords = c("Lon", "Lat"), crs = 4326, agr = "constant") %>% 
    st_transform(2154)
  groups_L93$Lat <- groups$Lat
  groups_L93$Lon <- groups$Lon
  groups_L93$gp_col <- groups$gp_col
  
  ## Add relative weight to the DF
  groups_L93$RW <- RW[,j]
  groups_L93$YesNo <- (RW[,j] %>% as.logical)
  if(black) groups_L93$gp_col[!groups_L93$YesNo] <- "#000000"
  
  
  # If all TRUE or all FALSE in "YesNo" -- no zcol
  if(length(unique(groups_L93$YesNo)) == 1){
    
    # if(unique(groups_L93$YesNo)) col_regions <- 2 else col_regions <- 1
    if(unique(groups_L93$YesNo)) cex <- "RW" else cex <- 3
    
    mapview(groups_L93, grid = FALSE,
            col = 1,
            alpha = 1,
            #col.regions = col_regions,
            col.regions = groups_L93$gp_col,
            alpha.regions = 1, 
            legend = FALSE, 
            cex = cex
    )  + 
      
      mapview(parcs_L93[j,], grid = FALSE,
              col = 1,
              alpha = 1,
              col.regions = "green",
              alpha.regions = 10, 
              legend = FALSE, 
              cex = NULL
      ) 
    
    # If some TRUE and some FALSE in "YesNo" : include zcol
  }else{
    mapview(groups_L93, grid = FALSE,
            col = 1,
            alpha = 1,
            col.regions = groups_L93$gp_col,
            zcol = "YesNo",
            alpha.regions = 1, 
            legend = FALSE, 
            cex = "RW"
    )  + 
      
      mapview(parcs_L93[j,], grid = FALSE,
              col = 1,
              alpha = 1,
              col.regions = "green",
              alpha.regions = 10, 
              legend = FALSE, 
              cex = NULL
      ) 
  } # end if
  
} # end function
