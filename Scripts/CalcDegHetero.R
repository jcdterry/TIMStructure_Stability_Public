CalcDegHetero<- function(J){
  ## Transpose necessary to covert from Jacobian input to igraph network notation of effect direction
  diag(J)<-0
  J %>%  t  %>% graph.adjacency(weighted = TRUE,mode = 'directed') -> N
  IN<-var(degree(N, mode='in', normalized = TRUE))
  OUT<-var(degree(N, mode='out', normalized = TRUE))
  BOTH<-var(degree(N, mode = 'all', normalized = TRUE))
  return(list('In'=IN, 'Out'=OUT, 'Both' = BOTH))
}