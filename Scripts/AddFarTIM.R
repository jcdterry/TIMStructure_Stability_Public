AddFarTIM<- function(J, TIM_Dens, TIMStrength ){
  ## This function only adds TIMs where the modifier is *NOT* trophically linked to the interactors
  if(TIM_Dens==0){return(J)}
  
  TIMStrength <- (TIMStrength * sqrt(pi))/sqrt(2) # Needed as an adjustor for half-normal distribution
  Size <- dim(J)[1]

  ###
  which(J!=0&lower.tri(J), arr.ind=T) %>%  as.data.frame %>%
    mutate(Present=paste(row, col, sep='_' ))%>%
    filter(row!=col) %>%
    mutate(Prey_Pred=paste(row, col, sep='_' ))%>%
    mutate(Pred_Prey=paste(col,row, sep='_' ))-> PreyPred
  
  Links<-   c(PreyPred$Pred_Prey, PreyPred$Prey_Pred)
  
  PotentialTIMs(J) %>%#  Modify present interactions
    mutate(Mod_Prey_link = paste(mod, prey, sep='_')) %>%
    mutate(Mod_Pred_link = paste(mod, pred, sep='_')) %>%
    filter( !(Mod_Prey_link %in%Links | Mod_Pred_link %in%Links) )   -> TIMs  # modifier *NOT* linked to prey or predator
  ###

  TIMs<- TIMs[rbinom(nrow(TIMs),1, prob=(TIM_Dens*Size)/nrow(TIMs))==1,] # Select fraction of possible TIMs that exist
  TIMs$EffectSize <- rnorm(nrow(TIMs), mean = 0, sd = TIMStrength)
  TIMEffectMatrix <- matrix(0, ncol=Size, nrow= Size)

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