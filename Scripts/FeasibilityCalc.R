
Omega <- function(alpha) {
  # adapted from https://github.com/clsong/JTB-Song_et_al-2018
  
  if (class(try(solve(t(alpha) %*% alpha), silent = T))[1] != "matrix") {
    return(0)
  }else{
    Sigma <- solve(t(alpha) %*% alpha)
    
    ### With empirical webs, numerical errors in the solution can creep in, which means 
    ## Sigma can fail an internal check of pmvnorm. 
    ##  This fixes it, by making sure it is a nice mirror, by taking an average
    
    if (!isTRUE(all.equal(Sigma, t(Sigma)))){
      Sigma<- (Sigma +t(Sigma))/2
    }
    
    
    S <- nrow(alpha)
    d <- pmvnorm(lower = rep(0, S),
                 upper = rep(Inf, S),
                 mean = rep(0, S),
                 sigma = Sigma)
    return( d[1]^(1 / S))
  }
}

CalcFeasEnv<-function(J){
  
  J_d5 <- J_d2 <- J
  diag(J_d2)<- -2
  diag(J_d5)<- -5
  return(list(Omega =   Omega(J),
              Omega_d2 = Omega(J_d2),
              Omega_d5 = Omega(J_d5))) 
}


CalcFeasEnv_Emprical<-function(J){
  
  J_d50000<-J_d5000<- J_d500 <- J_d100 <- J
  diag(J_d100)<- -100
  diag(J_d500)<- -500
  diag(J_d5000)<- -5000
  diag(J_d50000)<- -50000
  
  return(list(Omega =   Omega(J),
              Omega_d100 = Omega(J_d100),
              Omega_d500 = Omega(J_d500),
              Omega_d5000 = Omega(J_d5000),
              Omega_d50000 = Omega(J_d50000))) 
}




ExpectedOmega<- function(A){
  # Followng equation for Xi in Grilli et al 2017 Nat Comms
  # NB does not test if negative definite... So largely operating outside of the domain where it might be expected to work
  
  Ints <- A
  diag(Ints)<-NA
  E1 = mean(Ints, na.rm=TRUE)
  d = mean(diag(A))
  S= ncol(A)
  
  xi = 1+ (1/pi) * ((E1*(2*d- S*E1))  / (d - S*E1*E1)) # nb xi not Xi, (so no ^s)
  
  omega_est = xi/2
  return(omega_est)
  
}


