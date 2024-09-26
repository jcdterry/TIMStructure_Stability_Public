Capper<- function(J, cap=100){
  J[J< -cap]<-  -cap
  J[J >cap]<-  cap
  return(J)
}