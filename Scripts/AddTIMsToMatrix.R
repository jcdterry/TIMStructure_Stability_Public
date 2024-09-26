AddTIMsToMatrix<- function(J,TIM_Model,TIM_Dens, TIMStrength ){
  
  # List when checked :
  
  if(TIM_Model=='Nearby'){    J<-   AddNearbyTIM(J, TIM_Dens, TIMStrength )  } # :)
  if(TIM_Model=='Far'){   J<-   AddFarTIM(J, TIM_Dens, TIMStrength )  }  # :)
  if(TIM_Model=='Normal'){   J<-   AddRandomTIM(J, TIM_Dens, TIMStrength )  }  #:)
  if(TIM_Model=='Random'){    J<-   AddRandomInteractions(J, TIM_Dens, TIMStrength )  } #:)
  if(TIM_Model=='Positive'){   J<-   AddPositiveTIM(J, TIM_Dens, TIMStrength )  } # :)
  if(TIM_Model=='Negative'){   J<-   AddNegativeTIM(J, TIM_Dens, TIMStrength )  } # :)
  if(TIM_Model=='TightRecip'){   J<-   AddTightReciprocalTIM(J, TIM_Dens, TIMStrength )  }# :)  # need to sort out which function to use!
  if(TIM_Model=='Switching'){   J<-   AddSwitchingTIM(J, TIM_Dens, TIMStrength )  } #:)
  return(J) 
}