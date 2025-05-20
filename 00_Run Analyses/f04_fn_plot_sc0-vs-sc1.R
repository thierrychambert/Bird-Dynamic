## Function to plot estimated trajectories #####
plot_traj_sc0_sc1 <- function(n_sc0, n_sc1, group, CI = 0.95, start_year = 1, show_legend = FALSE){
  
  ## Bornes de l'IC
  p_lci <- (1-CI)/2
  p_uci <- 1 - ((1-CI)/2)
  
  if(is.na(group) | is.null(group)){
    
    title = paste(esp, ": National")
    
    N_sc0 <- (n_sc0[, start_year:ncol(n_sc0),] %>% colSums) 
    N_sc1 <- (n_sc1[, start_year:ncol(n_sc1),] %>% colSums) 
    
    ## estimates
    # sc0
    est_n_sc0 = N_sc0 %>% apply(., 1, quantile, probs = 0.5, na.rm = TRUE)
    lci_n_sc0 = N_sc0 %>% apply(., 1, quantile, probs = p_lci, na.rm = TRUE)
    uci_n_sc0 = N_sc0 %>% apply(., 1, quantile, probs = p_uci, na.rm = TRUE)
    # sc1
    est_n_sc1 = N_sc1 %>% apply(., 1, quantile, probs = 0.5, na.rm = TRUE)
    lci_n_sc1 = N_sc1 %>% apply(., 1, quantile, probs = p_lci, na.rm = TRUE)
    uci_n_sc1 = N_sc1 %>% apply(., 1, quantile, probs = p_uci, na.rm = TRUE)
    
  }else{
    
    title = paste(esp, ": Population", group)
    
    dim(n_sc0[group, start_year:ncol(n_sc0),])
    ## estimates
    # sc0
    est_n_sc0 = n_sc0[group, start_year:ncol(n_sc0),] %>% apply(., 1, quantile, probs = 0.5, na.rm = TRUE)
    lci_n_sc0 = n_sc0[group, start_year:ncol(n_sc0),] %>% apply(., 1, quantile, probs = p_lci, na.rm = TRUE)
    uci_n_sc0 = n_sc0[group, start_year:ncol(n_sc0),] %>% apply(., 1, quantile, probs = p_uci, na.rm = TRUE)
    
    # sc1
    est_n_sc1 = n_sc1[group, start_year:ncol(n_sc1),] %>% apply(., 1, quantile, probs = 0.5, na.rm = TRUE)
    lci_n_sc1 = n_sc1[group, start_year:ncol(n_sc1),] %>% apply(., 1, quantile, probs = p_lci, na.rm = TRUE)
    uci_n_sc1 = n_sc1[group, start_year:ncol(n_sc1),] %>% apply(., 1, quantile, probs = p_uci, na.rm = TRUE)
    
  }
  
  ## dimensions
  years <- start_year + (1:ncol(n_sc0)) - 1
  
  ## Define ylims
  ymin <- 0 
  ymax <- max(uci_n_sc0)
  
  # Estimates
  par(mar = c(6,6,4,1), cex.axis = 1.5, cex.lab = 2, cex.main = 2.5)
  plot(x = years, y = est_n_sc0, 'n', ylim = c(ymin, ymax),
       ylab = "Effectifs", xlab = "Année", main = title)
  
  ## Estimated Trends (average)
  points(x = years, y = est_n_sc0, 'l', pch = 1, lwd = 3, lty = 1, col = "black")
  points(x = years, y = est_n_sc1, 'l', pch = 1, lwd = 3, lty = 1, col = "red")
  
  
  
  ## CIs
  polygon(x = c(years, rev(years)), y = c(lci_n_sc0, rev(uci_n_sc0)), 
          col = adjustcolor("black", alpha.f = 0.2), border = NA)
  polygon(x = c(years, rev(years)), y = c(lci_n_sc1, rev(uci_n_sc1)), 
          col = adjustcolor("red", alpha.f = 0.2), border = NA)
  
  
  
  if(is.na(group) | is.null(group)){
    
    RI <- (N_sc0[ny_proj,] - N_sc1[ny_proj,])/N_sc0[ny_proj,]
    est_RI <- sprintf("%.1f", quantile(RI, probs = 0.5, na.rm = TRUE) *100)
    lci_RI <- sprintf("%.1f", quantile(RI, probs = p_lci, na.rm = TRUE) *100)
    uci_RI <- sprintf("%.1f", quantile(RI, probs = p_uci, na.rm = TRUE) *100)
    
  }else{
    
    ## Relative impact
    rel_impact <- (n_sc0[,ny_proj,] - n_sc1[,ny_proj,])/n_sc0[,ny_proj,]
    est_RI <- sprintf("%.1f", quantile(rel_impact[group,] , probs = 0.5, na.rm = TRUE) *100)
    lci_RI <- sprintf("%.1f", quantile(rel_impact[group,] , probs = p_lci, na.rm = TRUE) *100)
    uci_RI <- sprintf("%.1f", quantile(rel_impact[group,] , probs = p_uci, na.rm = TRUE)*100)
    
  }
  
  text_RI <- paste0("Impact relatif : ", est_RI, "%  [", lci_RI, "% ; ", uci_RI, "%]")
  
  ## Print value of (final) relative impact
  graphics::text(x = 0, y = max(uci_n_sc0), labels = text_RI, adj = c(0,0), cex = 1.5)
  
  
  ## Add legend
  if(show_legend) legend(x = 0, y = max(uci_n_sc0), legend = c("sc0", "sc1"), 
                         col = c("black", "red"), bty = "n", lwd = 4, cex = 1.8)
  
  
} # end function

#graphics.off()



