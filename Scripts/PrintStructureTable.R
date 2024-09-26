PrintStructureTable <- function(data){
  
  ROWORDER <-  c('Normal','Random' , 
                 'Nearby',  'Far' , 'EcoEng', 
                 'Positive', 'Negative','Unif' ,'LaPlace', 'Scaled' ,
                 'TightRecip', 'Switching','BoundSw' ,
                 'RanSkew' ,
                 'IncSkew' ,'DecSkew')
  
  LABELS<- c('Normally\nDistributed Strengths',
             'Random NTEs',
             'Nearby TIMs', 'Far TIMs',    'Heterogenous TIM Out-degree',
             'Facilitating Modifications', 'Interfering Modifications', 
             'Uniformally\nDistributed Strengths',
             'Laplace\nDistributed Strengths',  
             'Scaled Strengths',
             'Reciprocal Modifications',
             'Reciprocal Interference',
             'Bounded Reciprocal Interference',
             'Unbalanced NTEs', 
             'Larger Positive NTEs', 'Larger Negative NTEs')
  
  VARLIST<- c ("rho" ,"mu","V", "cov","C_VAR",
               "NT_Conn","Tot_Conn",
               "AInHet","AOutHet",
               "RowStructure" ,"ColStructure" )
  
  data2<-data
  
  data2$TIM_Model<-relevel(as.factor(data2$TIM_Model), 'Normal' )
  
  data2 %>%
    filter(TIM_Dens ==10) %>%
    group_by(TIM_Model) %>%
    summarise(rho=signif(mean(rho, na.rm=TRUE),3),
              mu=round(mean(mu, na.rm=TRUE),2),
              V=signif(mean(V, na.rm=TRUE),3),
              cov=signif(mean(cov, na.rm=TRUE),3),
              C_VAR=signif(mean(C_VAR, na.rm=TRUE),3),
              NT_Conn=signif(mean(NT_Conn, na.rm=TRUE),3),
              Tot_Conn=signif(mean(Tot_Conn, na.rm=TRUE),3),
              AInHet=round(mean(AInHet, na.rm=TRUE),4),
              AOutHet=round(mean(AOutHet, na.rm=TRUE),4),
              RowStructure=signif(mean(RowStructure, na.rm=TRUE),2),
              ColStructure=signif(mean(ColStructure, na.rm=TRUE),2)) -> MeanValues
  
  
  data2 %>%  
    dplyr::select(TIM_Model) %>%
    filter(TIM_Model != 'Normal') %>%
    arrange(TIM_Model) %>% unique -> Table
  
  
  ### Outout Full .csv of all results. 
  
  FullStatsResults <- data.frame('StructuralProperty'=NA, 'TIM_Model'=NA,
                                 'estimate'=NA,
                                 'statistic'=NA, 'p.value'=NA)
  
  
  FullNames <- c( 'Pairwise Correlation', 'Mean Interaction Strength',
  'Total Variance','Trophic : Non-Trophic Covariance',
  'Non-Trophic Variance',  'Non-Trophic Connectance',
 'Total Connectance','In-degree',
  'Out-degree', 'Row Structure', 'Column Structure')
  
  
  for( i in 1: length(VARLIST)){
    
    VAR = VARLIST[i]
    data2 %>% 
      tbl_df %>%
      filter(TIM_Dens ==10) %>%
      lm(paste(VAR,'~TIM_Model'), data=.) %>% 
      tidy %>%
      filter(term != '(Intercept)') %>%
      mutate(StructuralProperty = FullNames[i],
             TIM_Model = str_sub(term, 10),
             TIM_Model = factor(TIM_Model, levels = ROWORDER, labels = LABELS))%>%
    select(-term, -std.error)%>%
      bind_rows(FullStatsResults, .) -> FullStatsResults
  }
  
  
  colnames(FullStatsResults) <- c('Structural Property',
                                  'TIM Model',
                                  'Estimate of Difference',
                                  't-value',
                                  'p-value')
  
  write.csv(FullStatsResults[-1,], file = 'Full Statistical Results.csv' )
  
  
########  
  
  
  
  for( VAR in VARLIST){
    data2 %>% 
      tbl_df %>%
      filter(TIM_Dens ==10) %>%
      lm(paste(VAR,'~TIM_Model'), data=.) %>%
      tidy %>%
      filter(term != '(Intercept)')%>%
      dplyr::select(p.value) %>%
      transmute(VAR = p.value <0.05) %>%
      bind_cols(Table, .) -> Table
  }
  
  ### Ceate Table of the directions of responses
  data2 %>%  
    dplyr::select(TIM_Model) %>%
    filter(TIM_Model != 'Normal') %>%
    arrange(TIM_Model) %>% unique -> Direction_Table
  
  
  for( VAR in VARLIST){
    data2 %>% 
      tbl_df %>%
      filter(TIM_Dens ==10) %>%
      lm(paste(VAR,'~TIM_Model'), data=.) %>%
      tidy %>%
      filter(term != '(Intercept)')%>%
      dplyr::select(estimate, p.value) %>%
      transmute(VAR = ifelse(p.value <0.05, estimate>0, NA)) %>%
      bind_cols(Direction_Table, .) -> Direction_Table
  }
  
  colnames(Direction_Table)<- c('TIM_Model', VARLIST)
  colnames(Table)<- c('TIM_Model', VARLIST)
  
  MeanValues%>%
    gather('Column','Mean', rho:ColStructure ) -> LongMean
  
  Direction_Table%>%
    tbl_df %>%
    gather('Column','Direction', rho:ColStructure ) -> direction
  
  Table%>%
    tbl_df %>%
    gather('Column','Signif', rho:ColStructure ) ->  signif

  left_join(direction, signif)%>%
    dplyr::select(TIM_Model ,Column ,Direction ,Signif) %>%
    right_join(LongMean, by=c('TIM_Model', 'Column'))%>%
    mutate(MeanArrow = ifelse(is.na(Direction), Mean, 
                              ifelse(Direction==TRUE,
                                     paste0(Mean, ' (+)' ),
                                     paste0(Mean, ' (-)'))))%>%
    mutate(BoldMean = cell_spec(MeanArrow, "html", escape = FALSE,
                                bold =ifelse(Signif==TRUE,TRUE, FALSE  )))%>%
    mutate(TIM_Model = factor(TIM_Model, levels = ROWORDER, labels = LABELS))%>%
    arrange(TIM_Model) %>%
    dplyr::select(-c(Signif,Mean, MeanArrow, Direction) )%>%
    spread(Column, BoldMean) %>%
    dplyr::select(TIM_Model,
                  Tot_Conn	,
                  NT_Conn,
                  V,
                  C_VAR,
                  mu,
                  cov,
                  rho,
                  RowStructure,
                  ColStructure,
                  AInHet,
                  AOutHet)%>%
    rename( `TIM Model` =TIM_Model,
            `Total`=Tot_Conn	,
            `Non-Trophic`=NT_Conn,
            `Total`=V,
            `Non-Trophic`=C_VAR,
            `Mean Interaction Strength`= mu,
            `Trophic : Non-Trophic Covariance `=cov,
            `Pairwise Correlation`=rho,
            `Row Structure`=RowStructure,
            `Column Structure`=ColStructure,
            `In-degree`=AInHet,
            `Out-degree`=AOutHet)%>%
    kable(format = "html", escape = FALSE ,align = 'lccccccccccc',
caption = 'Table S2. Mean value of structural properties of the communities with TIMs introduced under
different models at a TIM density of 10 per species. To aid interpretation those models that differed
significantly (linear model, p<0.05, n=100 per model) from the baseline of equally distributed TIMs 
with a Normal strength distribution (first row) for a particular structural feature are shown in bold 
with a (+) or (-) indicating the direction of difference.
Exact values of estimated differences, t-values, and p-values are listed in "Full Statistical Results.csv", available at https://osf.io/8zrsm/') %>%  
    kable_styling()%>%
    add_header_above(c(" ",
                       "Connectance" = 2, 
                       "Variance" = 2, 
                       ' '= 3,
                       "Structuring" = 2,
                       'Total Degree Heterogeneity'=2),
                     escape = FALSE) %>% print
}