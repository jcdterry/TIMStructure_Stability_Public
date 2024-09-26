Tot_Conn<- function(J){
  diag(J)<-NA
  return(mean(J!=0, na.rm=T))
}