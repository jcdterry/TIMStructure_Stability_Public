RowColStructureEMP<- function(J){
  diag(J)<-NA
  J<- abs(J)
  J<- log(J+1)
  return(c(var(rowMeans(J, na.rm = TRUE)),
           var(colMeans(J, na.rm = TRUE))))
}