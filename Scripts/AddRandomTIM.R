AddRandomTIM <- function(J, TIM_Dens, TIMStrength ){
  if(TIM_Dens==0){return(J)}
  
  TIMStrength <- (TIMStrength * sqrt(pi))/sqrt(2) # Needed as an adjustor for half-normal distribution
  
  S <- dim(J)[1]
  TIMs <- PotentialTIMs(J)
  
  ## Cut down TIM list to correct number
  # NumWantedTIM interactions  / NumPossibleTIM interactions
  ProbTIMIntPresent<- (TIM_Dens * S)  / nrow(TIMs)
  TIMs <- TIMs[rbinom( nrow(TIMs),1, prob=ProbTIMIntPresent)==1,  ]
  
  TIMs$EffectSize <- rnorm(nrow(TIMs), mean = 0, sd = TIMStrength)
  TIMEffectMatrix <- matrix(0, ncol=S, nrow= S)
  
  ## Have to deal with difficulty of multiple TIMs, potentially of different signs, acting via the same effective interaction
  # Assume additive effect of TIMs
  if(nrow(TIMs)>0){
    for(i in 1: nrow(TIMs)){# Cycle through TIMs
      TIMEffectMatrix[TIMs$prey[i], TIMs$mod[i]] <- TIMEffectMatrix[TIMs$prey[i], TIMs$mod[i]] - TIMs$EffectSize[i]
      TIMEffectMatrix[TIMs$pred[i], TIMs$mod[i]] <- TIMEffectMatrix[TIMs$pred[i], TIMs$mod[i]] + TIMs$EffectSize[i]
    }
  }
  J<- J + TIMEffectMatrix # Add TIM Effect
  return(J)
}