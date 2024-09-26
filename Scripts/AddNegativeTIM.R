AddNegativeTIM <- function(J, TIM_Dens, TIMStrength ){
  if(TIM_Dens==0){return(J)}
  
  TIMStrength <- (TIMStrength * sqrt(pi))/sqrt(2) # Needed as an adjustor for half-normal distribution
  
  ## 'Negative' TIMs reduce the strength of the interaction with more of the modifier
  #   In the short term at least this is good for the prey and bad for the predator
  
  S <- dim(J)[1]
  TIMs <- PotentialTIMs(J)
  
  ## Cut down TIM list to correct number
  # NumWantedTIM interactions  / NumPossibleTIM interactions
  ProbTIMIntPresent<- (TIM_Dens * S)  / nrow(TIMs)
  TIMs <- TIMs[rbinom( nrow(TIMs),1, prob=ProbTIMIntPresent)==1,  ]
  
  TIMs$Pred_on_Prey <-sign(J[cbind(TIMs$prey, TIMs$pred)])
  TIMs$Prey_on_Pred <-sign(J[cbind(TIMs$pred, TIMs$prey)])
  ## Have to identify with confidence which is the predator and which is the prey
  ## Where the interaction is a +/+ or -/-,  leave as is. 
  TIMs %>% 
    mutate(NeedSwap = ifelse(Prey_on_Pred== -1 & Pred_on_Prey==1, -1, 1))-> TIMs
  
  # Effect Sizes all Positive, then For those the wrong' way round, swap sign
  
  TIMs$EffectSize <- abs(rnorm(nrow(TIMs), mean = 0, sd = TIMStrength))* TIMs$NeedSwap 
  TIMEffectMatrix <- matrix(0, ncol=S, nrow= S)
  ## Have to deal with difficulty of multiple TIMs, potentially of different signs, acting via the same effective interaction
  # Assume additive effect of TIMs
  if(nrow(TIMs)>0){
    for(i in 1: nrow(TIMs)){# Cycle through TIMs
      
      TIMEffectMatrix[TIMs$prey[i], TIMs$mod[i]] <- TIMEffectMatrix[TIMs$prey[i], TIMs$mod[i]] + TIMs$EffectSize[i]
      TIMEffectMatrix[TIMs$pred[i], TIMs$mod[i]] <- TIMEffectMatrix[TIMs$pred[i], TIMs$mod[i]] - TIMs$EffectSize[i]
      
    }
  }
  J<- J + TIMEffectMatrix # Add TIM Effect
  return(J)
}