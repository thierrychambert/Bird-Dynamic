cat(file = "model_IMPACT.txt", "
  model{
    
  ## National Scale
  # Prior on lambda_G (population growth rate at global scale)
  lambda_G ~ dunif(0, 10)
  #F ~ dunif(0.75, 0.95)
  F ~ dunif(0, 1)
  #F <- 0
  
  # Prior on N0
  N_sc0[1] ~ dunif(0, max.N0)
  Nb_sc0[1] <- N_sc0[1]
  
  N_sc1[1] <- N_sc0[1]
  Nb_sc1[1] <- N_sc0[1]

  # Process model over time (population growth)
  for (t in 1:(T-1)){
    # Scenario 0
    Nb_sc0[t+1] <- N_sc0[t] * lambda_G
    dNb_sc0[t] <- Nb_sc0[t+1] - N_sc0[t]
    
    Nb_sc0_M1[t] <- Nb_sc0[t+1]*(1-F)
    Nb_sc0_F1[t] <- Nb_sc0[t+1] - Nb_sc0_M1[t]
    
    # Scenario 1
    Nb_sc1[t+1] <- N_sc1[t] * lambda_G
    dNb_sc1[t] <- Nb_sc1[t+1] - N_sc1[t]
    
    Nb_sc1_M1[t] <- Nb_sc1[t+1]*(1-F)
    Nb_sc1_F1[t] <- Nb_sc1[t+1] - Nb_sc1_M1[t]
  } # t
  
  ## Colony-groups Scale
  # Priors
  for(i in 1:I){
    w0[i] ~ dunif(0,100)
    gamma0[i] <- w0[i]/sum(w0[1:I])
    b0[i] ~ dunif(0,100)
    b1[i] ~ dunif(0,100)
  } # i
  
  for(t in 1:(T-1)){
    for(i in 1:I){
      w[i,t] <- b0[i] + b1[i]*(t-1)
      gamma[i,t] <- w[i,t]/sum(w[1:I,t])
    } # i
  } # t
  
  # Process model : allocation of N among colony-groups 
  nb_sc0[1:I,1] <- gamma0[1:I]*Nb_sc0[1]
  nb_sc1[1:I,1] <- gamma0[1:I]*Nb_sc1[1]

  # Fidelity within colony/group
  for(t in 1:(T-1)){
    # Scenario 0
    nb_sc0_F2[1:I,t] <- (n_sc0[1:I,t]*F)  # ATTENTION : ici aussi c'est 'n', pas 'nb'
    PI_nb_sc0[1:I,t] <- nb_sc0[1:I,t]/max(Nb_sc0[t], 0)
    nb_sc0_F1[1:I,t] <- (PI_nb_sc0[1:I,t]*Nb_sc0_F1[t]) 

    # Scenario 1
    nb_sc1_F2[1:I,t] <- (n_sc1[1:I,t]*F)  ## here it is 'n' not 'nb' that is being transported from t to t+1 (so it's : n[i,t]*F)
    PI_nb_sc1[1:I,t] <- nb_sc1[1:I,t]/max(Nb_sc1[t], 0)
    nb_sc1_F1[1:I,t] <- (PI_nb_sc1[1:I,t]*Nb_sc1_F1[t])
    
    ## Fidels (final: depending on whether pop is growing or declining)
    for(i in 1:I){
      nb_sc0_F[i,t] <- min(nb_sc0_F1[i,t], nb_sc0_F2[i,t])
      nb_sc1_F[i,t] <- min(nb_sc1_F1[i,t], nb_sc1_F2[i,t])
    } # i
  } # t
  
  for(t in 1:(T-1)){
    # Movers at colony/group scale
    nb_sc0_M[1:I,t] <- n_sc0[1:I,t] - nb_sc0_F[1:I,t]   # ATTENTION : ici aussi c'est 'n', pas 'nb' 
    Nb_sc0_M2[t] <- sum(nb_sc0_M[1:I,t]) + dNb_sc0[t]
    
    nb_sc1_M[1:I,t] <- n_sc1[1:I,t] - nb_sc1_F[1:I,t]   # ATTENTION : ici aussi c'est 'n', pas 'nb'
    Nb_sc1_M2[t] <- sum(nb_sc1_M[1:I,t]) + dNb_sc1[t]
    
    # Movers (final: depending on whether pop is growing or declining)
    Nb_sc0_M[t] <- max(Nb_sc0_M1[t], Nb_sc0_M2[t])
    Nb_sc1_M[t] <- max(Nb_sc1_M1[t], Nb_sc1_M2[t])
    
    # Total per colony/group, before mortality
    nb_sc0[1:I,t+1] <- nb_sc0_F[1:I,t] + (gamma[1:I,t]*Nb_sc0_M[t])
    nb_sc1[1:I,t+1] <- nb_sc1_F[1:I,t] + (gamma[1:I,t]*Nb_sc1_M[t])
  } # t
  
  ## Apply mortality (to all age classes : n_TOT)
  for(i in 1:I){
    for(t in 1:T){
      
      nb_TOT_sc0[i,t] <- g*nb_sc0[i,t]
      nb_TOT_sc1[i,t] <- g*nb_sc1[i,t]
  
      n_TOT_sc0[i,t] <- max((nb_TOT_sc0[i,t]), 0)                     # sc0 : no mortality
      n_TOT_sc1[i,t] <- max((nb_TOT_sc1[i,t] - m[i,t]), 0)            # sc1 : apply 'm'
      
      n_sc0[i,t] <- n_TOT_sc0[i,t]/g
      n_sc1[i,t] <- n_TOT_sc1[i,t]/g
  
    } # t
  } # i
  
  # Affect total (sc1, after mortalities) to national scale
  for (t in 2:T){
    N_sc0[t] <- sum(n_sc0[1:I,t])
    N_sc1[t] <- sum(n_sc1[1:I,t])
  } # t
  
  ## Observation process
  for(i in 1:I){
    for(t in 1:T){
      #y[i,t] ~ dnorm(n_sc0[i,t]*PI[i,t], 1000)
      y[i,t] ~ dpois(n_sc0[i,t]*PI[i,t])
    } # t
  } # i
  
  ## Derived parameters : relative impact and growth_i (colony scale)
  for(i in 1:I){
    growth_i_sc0[i] <- pow((n_sc0[i,T]/n_sc0[i,1]), (1/(T-1)))
    growth_i_sc1[i] <- pow((n_sc1[i,T]/n_sc1[i,1]), (1/(T-1)))
    
    rel_impact[i] <- (n_sc0[i, T] - n_sc1[i, T])/n_sc0[i, T]
  } # i
    
    # NATIONAL
    growth_N_sc0 <- pow((N_sc0[T]/N_sc0[1]), (1/(T-1)))
    growth_N_sc1 <- pow((N_sc1[T]/N_sc1[1]), (1/(T-1)))
  
    rel_impact_national <- (N_sc0[T] - N_sc1[T])/N_sc0[T]
  
  } # model
") # cat