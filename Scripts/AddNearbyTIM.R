AddNearbyTIM<- function(J, TIM_Dens, TIMStrength ){
  if(TIM_Dens==0){return(J)}
  
  TIMStrength <- (TIMStrength * sqrt(pi))/sqrt(2) # Needed as an adjustor for half-normal distribution
  
   Size <- dim(J)[1]
  
  
  which(J!=0&lower.tri(J), arr.ind=T) %>%  as.data.frame %>%
    mutate(Present=paste(row, col, sep='_' ))%>%
    filter(row!=col) %>%
    mutate(Prey_Pred=paste(row, col, sep='_' ))%>%
    mutate(Pred_Prey=paste(col,row, sep='_' ))-> PreyPred
  
  Links<-   c(PreyPred$Pred_Prey, PreyPred$Prey_Pred)
  
  PotentialTIMs(J) %>%#  Modify present interactions
    mutate(Mod_Prey_link = paste(mod, prey, sep='_')) %>%
    mutate(Mod_Pred_link = paste(mod, pred, sep='_')) %>%
    filter(Mod_Prey_link %in%Links | Mod_Pred_link %in%Links )   -> TIMs  # modifier linked to prey or predator
  
  
  # Can get a case where number of possible TIMs is smaller than the desired case
  # In this case all possible TIMs are used
  
  if(nrow(TIMs)<(TIM_Dens*Size)){
    ProbTIM <- 1
  }else{
    ProbTIM <- (TIM_Dens*Size)/nrow(TIMs)
  }
  
  TIMs<- TIMs[rbinom(nrow(TIMs),1, prob=ProbTIM)==1,] # Select fraction of possible TIMs that exist
  TIMs$EffectSize <- rnorm(nrow(TIMs), mean = 0, sd = TIMStrength)
  TIMEffectMatrix <- matrix(0, ncol=Size, nrow= Size)
  
  # TIMs %>%
  #   select( prey, pred, mod, EffectSize) %>%
  #   gather( 'category' ,'target',  prey ,pred ) %>%   ###### Unifinished, wouild need to changes sign based on category
  #   group_by(mod, target ) %>%
  #   summarise(TotEffect =  sum(EffectSize)) %>%
  #   ungroup() -> TIMs
  
  

  ## Have to deal with difficulty of multiple TIMs, potentially of different signs, acting via the same effective interaction
  # Assume additive effect of TIMs
  if(nrow(TIMs)>0){
    for(i in 1: nrow(TIMs)){# Cycle through TIMs
      TIMEffectMatrix[TIMs$prey[i], TIMs$mod[i]] <- TIMEffectMatrix[TIMs$prey[i], TIMs$mod[i]] - TIMs$EffectSize[i]
      TIMEffectMatrix[TIMs$pred[i], TIMs$mod[i]] <- TIMEffectMatrix[TIMs$pred[i], TIMs$mod[i]] + TIMs$EffectSize[i]
    }
    
  } # Add TIM Effect to matrix
  return(J + TIMEffectMatrix )
}