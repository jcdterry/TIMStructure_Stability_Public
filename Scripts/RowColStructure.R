RowColStructure<- function(J){
  K<- abs(J)
  diag(K)<-NA
  return(data.frame('RowHet'=var(rowMeans(K, na.rm = TRUE)),
                    'ColHet'=var(colMeans(K, na.rm = TRUE))))
}