GetTL <- function(web){
  
  diag(web)<-0 # Remove cannibal links
  tweb <- t(web)
  sp<- length(tweb[,1])
  rs <- rowSums(tweb)
  for(i in 1:sp){
    tweb[i,tweb[i,]==1] = 1/rs[i] }   ## make the rows add to one
  nb.TL <- solve(diag(sp) - tweb)
  return(rowSums(nb.TL))
}