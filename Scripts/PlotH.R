## Function to plot the Hermitian part of a a Jacobian Matrix
PlotH <- function(TIM_Model,B,TIM_Dens, TIMStrength ){
  
  A <-  AddTIMsToMatrix(B,TIM_Model,TIM_Dens, TIMStrength) 
  H <- (A + t(A))/2
  title<-''
  
  title <- paste(TIM_Model)#,
               #  'Mean Magnitude:', round(mean(abs(A), na.rm=TRUE),3),
                 #'\n  NT Connectance:', round(mean(A!=0, na.rm=TRUE),2) , 
               #  'ColVar',var(rowMeans(A)), 
                # 'Var:', var(as.vector(A)))   
  print(title)
  return(PlotMatrix(H, title = ''))
}