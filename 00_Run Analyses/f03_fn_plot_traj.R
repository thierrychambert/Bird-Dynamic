## Function to plot estimated trajectories #####
plot_traj <- function(est_n, lci_n, uci_n, group_col = NULL, years = NULL, ylim = NULL, title){

  ## Colors
  if(is.null(group_col)) group_col <- 1:nrow(est_n)
  
  ## dimensions
  ii <- 1:nrow(est_n)
  if(is.null(years)) years <- min_year:(last_year + ny_proj)    # 1:ncol(est_n)
  
  ## Define ylims
  if(is.null(ylim)){
    ymin <- 0 
    ymax <- max(uci_n[ii,])
    ylim = c(ymin, ymax)  
  }
  
  # Estimates
  par(mar = c(6,6,4,1), cex.axis = 1.5, cex.lab = 2, cex.main = 2.5)
  plot(x = years, y = est_n[1,], 'n', ylim = ylim,
       ylab = "Effectifs",
       xlab = "Année",
       main = title)
  
  
  ## Colors
  CI_col <- rgb(red = col2rgb(group_col)["red",], 
                green = col2rgb(group_col)["green",],
                blue = col2rgb(group_col)["blue",],
                maxColorValue = 255, alpha = 100)
  
  ## Estimated Trends (average)
  for(i in ii){
    points(x = years, y = est_n[i,], 'l', pch = 1, lwd = 3, lty = 1, col = group_col[i])
  }
  
  ## CIs
  for(i in ii){
    polygon(x = c(years, rev(years)), 
            y = c(lci_n[i,], rev(uci_n[i,])), 
            col = CI_col[i], border = NA)
  }
  
} # end function




