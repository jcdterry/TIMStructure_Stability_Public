
BuildWeb <- function (ID= 1, 
                      size = 100, 
                      connectance = 0.1,
                      mux = -3, # Mean of the negative interaction strengths
                      muy = 1.5, #Mean of the positive interaction strengths
                      sigmax = 1, 
                      sigmay = 0.75, 
                      rhoxy = -2/3) {
  
  set.seed(ID)
  Connected<- FALSE
  
  while(!Connected){
    try({
      Links <- NicheModel(size, connectance)
      
      J <- matrix(0, size, size)
      NumPairs <- dim(Links)[1]
      
      mus <- c(mux, muy)
      covariance.matrix <- matrix(c(sigmax^2, rhoxy * sigmax * 
                                      sigmay, rhoxy * sigmax * sigmay, sigmay^2), 2, 2)
      Pairs <- mvrnorm(NumPairs, mus, covariance.matrix)
      J[Links] <- Pairs[, 1]
      J[Links[, 2:1]] <- Pairs[, 2]
      diag(J) <- 0
      
      Connected<-is_connected(graph_from_adjacency_matrix(J, weighted = TRUE)) # Backup check
    }, silent = TRUE) ## this silencing shuts up complaints about cyclic networks
    
  }
  
  return(J)
}

