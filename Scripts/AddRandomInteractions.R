AddRandomInteractions <- function(J, TIM_Dens, TIMStrength ){
  if(TIM_Dens==0){return(J)}
  S<- dim(J)[1]
  TIMStrength <- (TIMStrength * sqrt(pi))/sqrt(2) # Needed as an adjustor for half-normal distribution
  
  
  # NumWantedTIM interactions  / NumPossibleTIM interactions
  ProbTIMIntPresent<- (TIM_Dens * S * 2)  / (S*(S-1)*(S-2))
  TIMEffectCube <- array( rnorm(S^3,mean = 0,sd = TIMStrength),dim = c(S,S,S))
  TIMEffectCube<- TIMEffectCube *  rbinom(S^3,1, prob=ProbTIMIntPresent) 
  
  TIMEffectMatrix<- apply(TIMEffectCube, MARGIN = c(1,2), sum)
  diag(TIMEffectMatrix) <- 0 
  
  J<- J + TIMEffectMatrix # Add extra interactions 
  return(J)
}