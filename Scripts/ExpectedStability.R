ExpectedStability <- function(J){
  
  # adapted from eyeball::eye.approximate.ReL1 (Allesina et al)
  S <- dim(J)[1]
  N <- S * (S - 1)
  d <- mean(diag(J))
  diag(J) <- 0
  mu <- sum(J)/N
  var <- sum(J^2)/N - mu^2
  rho <- (sum(J * t(J))/N - mu^2)/var
  
  
  Dot<- ((S - 1) * mu) +  d
  Disk <- sqrt((S - 1) * var) * (1 + rho) - mu + d
  
  Expected <- max(Dot,Disk)
  
  eigens <- eigen(J, only.values = TRUE, symmetric = FALSE)$values
  obs <- max(Re(eigens))
  
  return(list('obs'=obs ,
              'ExpectedStability' = Expected,
              'S' = S, 
              'V' = var, 
              'mu' = mu, 
              'rho' = rho,
              'Tot_Conn' = Tot_Conn(J),
              'DotToRightOfDisk' = Dot>Disk))
}

