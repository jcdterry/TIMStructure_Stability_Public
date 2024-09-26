AddTightReciprocalTIM<- function(J, TIM_Dens, TIMStrength ){
  ## This function adds TIMs in pairs.
  # Each pair of TIMs involves the same three species, 
  
  if(TIM_Dens==0){return(J)}
    Size <- dim(J)[1]
    TIMStrength <- (TIMStrength * sqrt(pi))/sqrt(2) #Adjustor for half-normal distribution
    
  TIMs <-PotentialTIMs(J) 

  TIMEffectMatrix <- matrix(0, ncol=Size, nrow= Size)
  
  TIMs$Used<-'NotYet'
  TIMsIncluded <- 0
  TIMsTarget <- round(TIM_Dens*Size,0)
  
  while( TIMsIncluded < TIMsTarget ){ # pick an interaction

    xx<- sample(1:nrow(TIMs), 1) # Pick a TIM to try
    
    if(TIMs$Used[xx] =='NotYet'){
      # 
      i<- TIMs$prey[xx]
      j<- TIMs$pred[xx]
      k<- TIMs$mod[xx]
      
      threeSp <- c(i,j,k)
      
      TIMs %>%  # Identify TIMs that involve the same three species
      filter(!all(c(prey==i,pred==j,mod==k  ))) %>% # Don't pick the same one again
      filter(prey %in% threeSp & pred %in% threeSp & mod %in% threesp)%>%# Must use ijk
        filter(Used == 'NotYet')-> PotPairs 
      
    if(nrow(PotPairs>0)){
      ## Select one of the options
       zz<-sample(1:nrow(PotPairs), 1)
      i2<- PotPairs$prey[zz]
      j2<- PotPairs$pred[zz]
      k2<- PotPairs$mod[zz]
      

      # Add TIMs to TIM effect Matrix 
      ## First
      EffectSize1<- rnorm(1, mean = 0, sd = TIMStrength)
      TIMEffectMatrix[i, k] <- TIMEffectMatrix[i, k] - EffectSize1
      TIMEffectMatrix[j, k] <- TIMEffectMatrix[j, k] + EffectSize1
      ## Second 

      EffectSize2<- rnorm(1, mean = 0, sd = TIMStrength)
      TIMEffectMatrix[i2, k2] <- TIMEffectMatrix[i2, k2] - EffectSize2
      TIMEffectMatrix[j2, k2] <- TIMEffectMatrix[j2, k2] + EffectSize2
      
      # Remove TIMs from future consideration
      TIMs$Used[xx]<- 'Used'
      TIMs$Used[which(TIMs$prey==i2 & TIMs$pred==j2 & TIMs$mod== k2)]<- 'UsedAsRecip'
      
      # Add Two to count:
      TIMsIncluded <- TIMsIncluded+2 
      
         }
      }else{
      # Draw another one
    }   
  }
  return(J + TIMEffectMatrix )# Add TIM Effect to matrix
}