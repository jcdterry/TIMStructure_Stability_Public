RunArtificialWebAnalysis <- function(TrophicNetworks,
                                     TIMStrength=0.5,
                                     Repeats= 10,
                                     Name='Unnamed',
                                     Models,
                                     RowStruct=FALSE,
                                     FREQ_TO_TEST=c(1)
){
  
  data<- data.frame( 'obs' =NA,'ExpectedStability'=NA,'rho'=NA,
                     'mu' =NA,
                     'Omega'=NA,'Omega_d2'=NA, 'Omega_d5'=NA,
                     'DotToRightOfDis' =NA,
                     'V'=NA,
                     'cov'=NA,
                     'NT_VAR'= NA,
                     'C_VAR'=  NA,
                     'MeanMagNT'= NA,
                     'MeanNT'=  NA,
                     'Mean_C'= NA,
                     'NT_Conn'= NA,
                     'TIM_Model'=NA,
                      'TIM_Dens'=NA,
                     'ActualTrophC'= NA,'TIMStrength'=NA,'Tot_Conn'=NA,
                     'rhoxy'=NA,'AInHet' =NA,
                     'AOutHet'=NA,'ABothHet'=NA, 
                     'RowStructure'=NA, 'ColStructure'=NA)
  
  Bs<- TrophicNetworks
  ActualTrophC <- map_dbl(Bs, Tot_Conn)
  
  for(TIM_Model in Models){
    print(TIM_Model)
    for(TIM_Dens in FREQ_TO_TEST){ # 
      try({
        
        As<- lapply(Bs,AddTIMsToMatrix,TIM_Model, TIM_Dens, TIMStrength)
        
        Cs <- map2(As, Bs, function(A,B){A-B})
        
        if(RowStruct){
          
          Bs<-map(Bs, function(M){M*rlnorm(dim(M)[1], -0.25, sqrt(0.5))})
          Cs<-map(Cs, function(M){M*rlnorm(dim(M)[1], -0.25, sqrt(0.5))})
          As <- map2(Bs, Cs, function(B,C){B+C})
        }
        
        Ap<- map_df(As, ExpectedStability)
        Feasib <- map_df(As, CalcFeasEnv)
        RowColStrcut <-map_df(As, RowColStructure)
        Cstats <- map2_df(As, Bs, TIM_Dist_Examine)
        ADegrHet <- map_df(As, CalcDegHetero)
        
        data<-bind_rows(data, 
                        data.frame('obs' =Ap$obs, 
                                   'ExpectedStability'=Ap$ExpectedStability,
                                   'rho'=Ap$rho,
                                   'mu'=Ap$mu,
                                   'Omega'=Feasib$Omega,
                                   'Omega_d2'=Feasib$Omega_d2,
                                   'Omega_d5'=Feasib$Omega_d5,
                                   'DotToRightOfDisk'=Ap$DotToRightOfDisk, 
                                   'V'=Ap$V,
                                   'cov'=  Cstats$Covariance,
                                   'NT_VAR'=  Cstats$NT_VAR,
                                   'C_VAR'=  Cstats$C_VAR,
                                   'MeanMagNT'=  Cstats$MeanMagNT,
                                   'MeanNT'=  Cstats$MeanNT,
                                   'Mean_C'=  Cstats$Mean_C,
                                   'NT_Conn'= Cstats$NT_Conn,
                                   'TIM_Model'=TIM_Model,
                                   'TIM_Dens'=TIM_Dens,
                                   'ActualTrophC'= ActualTrophC,
                                   'TIMStrength' = TIMStrength,
                                   'Tot_Conn'= map_dbl(As, Tot_Conn), 
                                   'AInHet' =ADegrHet$In,
                                   'AOutHet'=ADegrHet$Out,
                                   'ABothHet'=ADegrHet$Both,
                                   'RowStructure'=RowColStrcut$RowHet,
                                   'ColStructure'=RowColStrcut$ColHet))
      })
    }
  }
  data<- data[-1,]
  
  save(data,file = paste0(Name,'DataFrame' )) 
  return(data)
}