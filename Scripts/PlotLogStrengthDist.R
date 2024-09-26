PlotLogStrengthDist <- function(J, title=''){
  S<- dim(J)[1]
  diag(J)<-NA
  J %>% as.data.frame-> Z
  colnames(Z)<-1:S
  Z %>%
    rownames_to_column() %>%
    gather('column', 'Strength', 2:(S+1)) %>%
    filter(Strength>0)%>%
    mutate(LogStrength= log(abs(Strength)))%>%
    ggplot(aes(x=LogStrength))+
    theme(text = element_text(size=10))+
    theme(plot.title = element_text(size=12))+
    geom_histogram()->pos
  
  
  Z %>%
    rownames_to_column() %>%
    gather('column', 'Strength', 2:(S+1)) %>%
    filter(Strength<0)%>%
    mutate(LogStrength= log(abs(Strength)))%>%
    ggplot(aes(x=LogStrength))+
    theme(text = element_text(size=10))+
    geom_histogram()-> neg
  
  return(plot_grid(neg, pos, label_size=10, 
                   labels = c('Negative', 'Positive'), label_x=0.15))
  
}

