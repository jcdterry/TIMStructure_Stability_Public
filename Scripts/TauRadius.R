TauRadius <- function(J){
  S<-  dim(J)[1]
  UT<-upper.tri( J)
  LT<-lower.tri( J)
  tau <-FindTau(J)
  E_2<- mean((J* t(J))[LT])  # mean of products of off diagonal pairs
  V  <- var( c(J[UT], J[LT])) # Variance 
  E  <- mean(c(J[UT], J[LT]))  # Mean
  return( sqrt(S*V)*(1+tau)-E)
}


FindTau<- function(J){
  
  diag(J)<-NA
  C<- mean(J!=0, na.rm=TRUE)
  #Expected Fractions if random:
  Exp<-c((C^2)/2,# Competiton or mutualism
         (C^2)/2,   # Pred-prey
         (1-C)^2, # non-interacting
         2*C*(1-C)) # (0,+/-)
  Fracs<-DetFracs(J)
  FracRandom<- sum(Fracs[3:4]) / sum(Exp[3:4]) # in this case there are 82% of the expected number of zero and one way,
  
  FracPred_Prey <-max(0,Fracs[2] - Exp[2]) # Necessary surplus of predator-preys, must be non-negative
  FracRecip <- max(0,Fracs[1] - Exp[1]) # Necessaryl surpuls of Reciprical , must be non-negative
  # print(FracRandom + FracPred_Prey + FracRecip)
  
  tau = FracRecip*(2/pi)   - FracPred_Prey *(2/pi) 
  
  return(tau)
}


DetFracs <- function(J){
  
  Jij <- sign(J[upper.tri(J)])
  Jji <- sign(t(J)[upper.tri(J)])
  
  Recip <- mean(Jij == 1 & Jji==1 ) +  mean(Jij == -1 & Jji==-1 )
  Pred <- mean(Jij==1 & Jji==-1) + mean(Jij==-1 & Jji==1)
  Zero <-  mean(Jij == 0 &  Jji ==0)
  OneWay <- 1-Recip - Pred - Zero
  return(c(Recip, Pred, Zero, OneWay ))
}