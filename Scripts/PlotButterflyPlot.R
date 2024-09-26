
PlotButterflyPlot<- function(J, title=''){
  S<- dim(J)[1]
  diag(J)<-NA
  
  
  data.frame('Upper'= J[upper.tri(J)] ,
             'Lower'=  -t(J)[upper.tri(J)] ) %>%
    filter(Upper!=0 & Lower !=0)%>%
    ggplot(aes(Upper, Lower))+
    scale_y_log10()+
    scale_x_log10()+
    xlab('Prey effect\non Predator')+
    ylab('Predator effect\non Prey')+
    geom_point()->Logged
  
  data.frame('Upper'= J[upper.tri(J)] ,
             'Lower'=  -t(J)[upper.tri(J)] ) %>%
    filter(Upper!=0 & Lower !=0)%>%
    ggplot(aes(Upper, Lower))+
    xlab('Prey effect\non Predator')+
    ylab('Predator effect\non Prey')+
    geom_point()->Linear
  
  
  return(plot_grid(Linear, Logged))
  
}

