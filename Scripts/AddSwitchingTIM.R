AddSwitchingTIM<- function(J, TIM_Dens, TIMStrength ){
  ## This function adds TIMs in pairs torepresent predator switching
  # Each pair involves the same three species, one predator, two prey,  
  
  if(TIM_Dens==0){return(J)}
  
  Size <- dim(J)[1]
  TIMStrength <- (TIMStrength * sqrt(pi))/sqrt(2) # Needed as an adjustor for half-normal distribution
  
  TIMs <-  PotentialTIMs(J) #Modify present interactions
  
  ## Keep only predator prey interactions
  TIMs$AonB <-sign(J[cbind(TIMs$prey, TIMs$pred)])
  TIMs$BonA <-sign(J[cbind(TIMs$pred, TIMs$prey)])
  filter(TIMs,AonB!=BonA) -> TIMs
  
  # Any species not able to form part of a reciprocal pair of TIMs?
  # First remove all interactions not in a pair
  ## If predator has only one prey then switching not possible
  
  # Can still have signs the wrong way round, need to go through and flip those 
  # around as which is prey and which is pred actually matters
  
  ## Assume A on B 'should' be predator effects on prey, ie negative
  TIMs %>% filter(AonB ==-1) -> RightWayRound
  TIMs %>% filter(AonB == 1) -> WrongWayRound
  
  ### Re arrange the wrong way round ones
  ShouldBePrey<-WrongWayRound$pred
  ShouldBePred<-WrongWayRound$prey
  WrongWayRound$pred <- ShouldBePred
  WrongWayRound$prey <- ShouldBePrey
  # Put them back together again:
  bind_rows(RightWayRound,WrongWayRound ) -> TIMs 
  
  ## Because of potential for -/- interactions need to be a little more careful
  PredMatrix <- J<0 & t(J)>0
  diag(PredMatrix)<-NA
  NumPreysOfX <- colSums(PredMatrix, na.rm = TRUE)#  j must have only one prey (col j <=1)
  
  TIMs %>%
    filter(ifelse( NumPreysOfX[pred]>1,TRUE, FALSE))-> TIMs  # Only look at possibles %>%
  
  # To add TIMs, 
  # draw a possibly modified interaction i-j
  # Find the potential third species
  # Randomly select one 
  # Add TIMs. C_ijk + C_kji 
  # Remove both potential TIMs from draw list
  
  # Keep going until have drawn enbough TIMs (round down number)
  
  TIMs<- TIMs[sample(1:nrow(TIMs), nrow(TIMs),replace = FALSE ), ] # randomly reorder
  
  TIMEffectMatrix <- matrix(0, ncol=Size, nrow= Size)
  
  
  TIMs$Used<-'NotYet'
  TIMsIncluded <- 0
  TIMsTarget <- round(TIM_Dens*Size,0)
  counter<-0
  
  
  while( TIMsIncluded < TIMsTarget){ # pick an interaction
    
    counter <-counter+1 
        xx<- sample(1:nrow(TIMs), 1) # Pick a TIM to try
    if(TIMs$Used[xx] =='NotYet'){     # If not draw another one
      
      i<- TIMs$prey[xx]
      j<- TIMs$pred[xx]
      
      # First find potential modifiers (prey also eaten by the predator)
      PreyOfJ<- which(PredMatrix[,j])#  prey that share a predator
      Potential3rd<- PreyOfJ[PreyOfJ!=i] # Exclude prey already being considered
      
      if(length(Potential3rd)>0){

        ShareOptions<- data.frame(prey_in_TIM2=Potential3rd,
                                  mod_in_TIM2=i, 
                                  prey_in_TIM1=i, 
                                  mod_in_TIM1=Potential3rd)
        
        ## Select one of the options
        SP <-ShareOptions[sample(1:nrow(ShareOptions), 1),]
        
        ## Identify TIM 2 
        yy<-which(TIMs$prey==SP$prey_in_TIM2 & TIMs$pred==j & TIMs$mod== SP$mod_in_TIM2)

        if(length(yy)==1){     
          
          if(TIMs$Used[yy] =='NotYet'){# If not draw another one
            
            TIMsIncluded <- TIMsIncluded+2 # Add Two to count
            
            # Remove TIMs from future consideration
            TIMs$Used[xx]<- 'Used'
            TIMs$Used[yy]<- 'UsedAsRecip'
            
            # Add TIMs to TIM effect Matrix 
            ## First
            mod_in_TIM1<- SP$mod_in_TIM1
            prey_in_TIM1<-SP$prey_in_TIM1
            EffectSize1<- abs(rnorm(1, mean = 0, sd = TIMStrength))
            TIMEffectMatrix[j, mod_in_TIM1]            <- TIMEffectMatrix[j,            mod_in_TIM1] - EffectSize1
            TIMEffectMatrix[prey_in_TIM1, mod_in_TIM1] <- TIMEffectMatrix[prey_in_TIM1, mod_in_TIM1] + EffectSize1
            ## Second 
            mod_in_TIM2<- SP$mod_in_TIM2
            prey_in_TIM2<-SP$prey_in_TIM2
            EffectSize2<- abs(rnorm(1, mean = 0, sd = TIMStrength))
            TIMEffectMatrix[j, mod_in_TIM2]            <- TIMEffectMatrix[j,            mod_in_TIM2] - EffectSize2
            TIMEffectMatrix[prey_in_TIM2, mod_in_TIM2] <- TIMEffectMatrix[prey_in_TIM2, mod_in_TIM2] + EffectSize2

            }
        }
      }
    }
    if(counter > 10000){
      print(paste('Failed to find enough TIMs to put in. Switching Model, TIM CONN = ', TIM_Dens))
      break}
  }
  
  return(J + TIMEffectMatrix )# Add TIM Effect to matrix
}