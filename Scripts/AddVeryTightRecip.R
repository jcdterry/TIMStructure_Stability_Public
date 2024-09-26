AddTightReciprocalTIM<- function(J, TIM_Dens, TIMStrength ){
  ## This function adds TIMs in pairs.
  # Each pair involves the same three species, with one species playing hte same role in each interaction
  # one predator, two prey,   or one prey, two predators
  
  if(TIM_Dens==0){return(J)}
  Size <- dim(J)[1]
  TIMStrength <- (TIMStrength * sqrt(pi))/sqrt(2) # Needed as an adjustor for half-normal distribution
  
  
  #NB interaction not necessarily a predator - prey link!  Pred, prey labelling may be a bit missleading
  
  PotentialTIMs(J) -> TIMs #Modify present interactions
  
  # Any species not able to form part of a reciprocal pair of TIMs?
  
  # First remove all interactions not in a pair
  ## Only if a species is the sole predator of its sole prey AND is the sole prey of its sole predator
  # For it not to be possible to add a reciprical tim to an interaction i-j (prey-preedator)
  # i must have only one predator (row i <=1) &
  # j must have only one prey (col j <=1)
  
  NumPredatorsOfX <- rowSums(J<0, na.rm=TRUE)
  NumPreysOfX <- colSums(J<0, na.rm=TRUE)
  
  TIMs %>%
    mutate(SharedPredatorNotPossible = ifelse( NumPredatorsOfX[prey]==1,TRUE, FALSE)) %>%
    mutate(SharedPreyNotPossible = ifelse( NumPreysOfX[pred]==1,TRUE, FALSE)) %>%
    filter( !(SharedPreyNotPossible  & SharedPredatorNotPossible  )) -> TIMs  # Filter out NotPossibles
  
  # To add TIMs, 
  # draw a possibly modified interaction i-j
  # Find the potential third species (element in row i !=j or in col j != i)
  # Randomly select one species
  # Add TIMs. either  C_ijk + C_ikj    or C_ijk + C_kji for tight recipricality
  # Remove both potential TIMs from draw list
  
  # Keep going until have drawn enbough TIMs (round down number)
  
  TIMs<- TIMs[sample(1:nrow(TIMs), nrow(TIMs),replace = FALSE ), ] # randomly reorder
  
  TIMEffectMatrix <- matrix(0, ncol=Size, nrow= Size)
  
  TIMs$Used<-'NotYet'
  TIMsIncluded <- 0
  TIMsTarget <- round(TIM_Dens*Size,0)
  
  while( TIMsIncluded < TIMsTarget ){ # pick an interaction
    
    xx<- sample(1:nrow(TIMs), 1) # Pick a TIM to try
    
    if(TIMs$Used[xx] =='NotYet'){
      # First find potnetial modifiers
      i<- TIMs$prey[xx]
      j<- TIMs$pred[xx]
      
      PreyShareOptions<- NULL
      PredShareOptions <- NULL
      
      Potential3rdPreyShare<- unique(TIMs$prey[TIMs$pred == j ])#   share prey
      Potential3rdPreyShare<- Potential3rdPreyShare[Potential3rdPreyShare!=i]
      if(length(Potential3rdPreyShare)>0){
        PreyShareOptions<- data.frame(prey1=i, pred1=j, mod1=Potential3rdPreyShare, prey2=Potential3rdPreyShare, pred2=j, mod2=i  )
      }
      # first mod = ijk, second = ikj
      Potential3rdPredShare<- unique(TIMs$pred[TIMs$prey == i ])#   share pred
      Potential3rdPredShare<- Potential3rdPredShare[Potential3rdPredShare!=j]
      
      if(length(Potential3rdPredShare)>0){
        PredShareOptions <-  data.frame(prey1=i, pred1=j, mod1=Potential3rdPredShare, prey2=i, pred2=Potential3rdPredShare, mod2=j)
      }
      
      if(!(is.null(PreyShareOptions)&is.null(PredShareOptions))){
      ShareOptions <- bind_rows(PreyShareOptions, PredShareOptions)
      
      ## Select one of the options
      SP <-ShareOptions[sample(1:nrow(ShareOptions), 1),]
      
      
      TIMsIncluded <- TIMsIncluded+2 # Add Two to count
      # Remove TIMs from future consideration
      TIMs$Used[xx]<- 'Used'
      
      yy<-which(TIMs$prey==SP$prey2 & TIMs$pred==SP$pred2 & TIMs$mod== SP$mod2)
      TIMs$Used[yy]<- 'UsedAsRecip'
      
      # Add TIMs to TIM effect Matrix 
      ## First
      mod<- SP$mod1
      int1<-SP$prey1
      int2<-SP$pred1
      EffectSize<- rnorm(1, mean = 0, sd = TIMStrength)
      TIMEffectMatrix[int1, mod] <- TIMEffectMatrix[int1, mod] - EffectSize
      TIMEffectMatrix[int2, mod] <- TIMEffectMatrix[int2, mod] + EffectSize
      ## Second 
      mod<- SP$mod2
      int1<-SP$prey2
      int2<-SP$pred2
      EffectSize<- rnorm(1, mean = 0, sd = TIMStrength)
      TIMEffectMatrix[int1, mod] <- TIMEffectMatrix[int1, mod] - EffectSize
      TIMEffectMatrix[int2, mod] <- TIMEffectMatrix[int2, mod] + EffectSize
    }
      }else{
      # Draw another one
    }
    
    
  }
  return(J + TIMEffectMatrix )# Add TIM Effect to matrix
}