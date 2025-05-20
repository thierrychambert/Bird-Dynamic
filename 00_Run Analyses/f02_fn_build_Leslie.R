library(popbio)

##==============================================================================
##               Function to extract Leslie matrix elements                   ==
##==============================================================================
elements_Leslie <- function(s, f, type = "pre"){
  
  nac <- length(s)
  vr_list <- as.list(c(s,f))
  names(vr_list) <- c(paste0("s", (1:nac)-1), paste0("f", (1:nac)-1))
  vital_rates <- unlist(vr_list)
  
  # If Pre-Breeding
  if(type == "pre"){
    A <- diag(0, nrow = nac-1)
    
    A[1,] <- paste0(  paste0("f",1:(nac-1)), "*", "s0"  )
    
    if((nac-1) < 3){
      A[-1,] <- paste0("s", 1:(nac-1))
    }else{
      diag(A[-1,]) <- paste0("s", 1:(nac-2))
      A[nac-1,nac-1] <- paste0("s", nac-1)
    }
    
    # If Post-Breeding
  }else{
    A <- diag(0, nrow = nac)
    
    A[1,] <-  paste0(paste0("s", (1:nac)-1),"*",paste0("f", c((2:nac)-1,nac-1)))
    if(nac < 3){
      A[-1,] <- paste0("s", (1:nac)-1)
    }else{
      diag(A[-1,]) <- paste0("s", (1:(nac-1))-1)
      A[nac,nac] <- paste0("s", nac-1)
    }
    
  } # enf if
  
  symbolic <- noquote(A)
  elements <- parse(text=t(A))
  
  A <- matrix( sapply(elements, eval, vr_list), nrow=nrow(A), byrow=TRUE)
  
  return(list(A=A, vital_rates=vital_rates, symbolic=symbolic, elements=elements,
              vr_list = vr_list, vr_names = names(vr_list)))
}
## End of function
################################################################################
# s <- c(0.5, 0.7, 0.8, 0.95)
# f <- c(0, 0, 0.05, 0.55)
# elements_Leslie(s=s, f=f)



##==============================================================================
##               Function to build the Leslie matrix                          ==
##==============================================================================
build_Leslie <- function(s, f, type = "pre"){ elements_Leslie(s=s, f=f, type=type)$A } # End of function
# s <- c(0.5, 0.7, 0.8, 0.95)
# f <- c(0, 0, 0.05, 0.55)
# build_Leslie(s=s, f=f)



##==============================================================================
##   Function to build a vector of population size, for each age class        ==
##==============================================================================
#' @param nb_pair a single number. Total Population Size or Number of Pairs.
#' @param pop_size_type character value indicating if the provided value pop_size correpsonds to Total Population Size ("Ntotal")
#' or the Number of Pairs ("Npair"). A stable age distribution is used to infer the size of each age class.
#' @param s a vector of survival probabilities for each age class
#' @param f a vector of fecundity values for each age class
pop_vector <- function(nb_pair, s, f, pr){
  
  N00 <- nb_pair*2
  
  # Get the LESLIE matrix, SAD and mature age classes
  A <- build_Leslie(s, f, type = "post")
  SAD <- stable.stage(A)
  mature <- which(f != 0) # identify mature age classes
  
  Ntot <- (N00/sum(SAD*pr))
  N0 <- round(Ntot*SAD)
  
  return(N0)
}
