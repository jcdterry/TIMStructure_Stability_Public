TIM_Dist_Examine <- function(A, B){
  diag(A)<-NA
  C <- as.vector(A-B)
  NTES<- C[C!=0]
  
  return(data.frame('MeanMagNT'=mean(abs(NTES), na.rm=TRUE),
                    'MeanNT'= mean(NTES, na.rm=TRUE),
                    'Mean_C'= mean(C, na.rm=TRUE),
                    'C_VAR'= var(C, na.rm = TRUE),
                    'NT_VAR' = var(NTES, na.rm=TRUE), 
                    'NT_Conn'=mean(C !=0, na.rm=TRUE),
                    'Covariance'=cov(as.vector(B), as.vector(C),
                                     "pairwise.complete.obs")))
}