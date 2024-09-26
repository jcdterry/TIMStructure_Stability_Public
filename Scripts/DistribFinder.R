DistribFinder <- function(J){
  S<- dim(J)[1]
  diag(J)<-0
  LPOS<-log(J[J>0])
  LNEG<-log(-J[J<0])
  LOverall<-c(LNEG, LPOS)
  
  return(data.frame('OverallLMu'= mean(LOverall), 
                    'OverallLSD'= sd(LOverall),
                    'PosLMu'= mean(LPOS) , 
                    'NegLMu'= mean(LNEG), 
                    'PosLSD'= sd(LPOS), 
                    'NegLSD' = sd(LNEG)))
}
