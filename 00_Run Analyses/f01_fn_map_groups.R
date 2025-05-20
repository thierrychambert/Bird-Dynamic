################################################################################-
## Make group colors
################################################################################-
make_gp_col <- function(){
  # Define Distinctive Palette 
  qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
  col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  col_vector <- col_vector[!duplicated(col_vector)]
  length(col_vector)
  
  # Define group colors
  gp_col <- col_vector[1:n_group]
  return(gp_col)
}




################################################################################-
## Map Groups
################################################################################-

map_groups <- function(parcs = TRUE){
  
  ### Map
  if(parcs){
    mapview(colonies_L93, grid = FALSE,
            col = NULL,
            alpha = NULL,
            col.regions = colonies_L93$gp_col,
            alpha.regions = 1, 
            zcol = "group", 
            legend = FALSE, 
            cex = 3
    ) +
      mapview(parcs_L93, grid = FALSE,
              col = 1,
              alpha = 1,
              col.regions = "green",
              alpha.regions = 10, 
              legend = FALSE, 
              cex = NULL
      ) 
  }else{
    mapview(colonies_L93, grid = FALSE,
            col = NULL,
            alpha = NULL,
            col.regions = colonies_L93$gp_col, # 1:n_group,
            alpha.regions = 1, 
            zcol = "group", 
            legend = FALSE, 
            cex = 3
    )
  }
  
} # END FUNCTION map_groups


