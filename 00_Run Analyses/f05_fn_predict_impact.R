predict_impact <- function(){
  for(k in 1:n_iter){
    
    if((k %% (n_iter/100)) == 0) print(paste("simulation :", (k/n_iter)*100, "%"))
    
    ### Mortalities (collisions)
    # mu_m <- morta_iter_gp[k,] # NUMBER of mortalities (independent from Current Pop Size) -- with uncertainty on mortality 
    mu_m <- morta_iter_gp[k,]/n_sc0[,1,k] # mortality RATE -- with uncertainty on mortality 
    morta_rate[,k] <- mu_m
    
    for(t in 2:ny_proj){
      
      # sc0 : no collision fatalities
      n_sc0[,t,k] <- rpois(n=n_group, n_sc0[,t-1,k]*lam[,k])
      
      ## save demographic stochasticity
      fac_ds <- 1 + ((n_sc0[,t,k] - (n_sc0[,t-1,k]*lam[,k]))/(n_sc0[,t-1,k]*lam[,k]))
      fac_ds[is.nan(fac_ds)] <- 1 ## for cases where n = 0
      
      # sc1: with collision fatalities : MORTALITY RATE
      n_tmp <- round((n_sc1[,t-1,k]*lam[,k]) * fac_ds)
      
      #real_m <- rpois(n=n_group, mu_m) # Apply mortality as a NUMBER independent from Current Pop Size
      real_m <- rpois(n=n_group, (mu_m*n_tmp)) # Apply mortality as a RATE
      
      for(i in 1:n_group) real_m[i] <- min(real_m[i], n_tmp[i])
      n_sc1[,t,k] <- n_tmp - real_m
      
    } # t
  } # k (simulation iterations)
  
  return(list(n_sc0 = n_sc0, n_sc1 = n_sc1, morta_rate = morta_rate))
  
} # end function

