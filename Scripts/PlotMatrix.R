
PlotMatrix <- function(J, title=''){
  #title='' # comment out for titles
  S<- dim(J)[1]
  diag(J)<-NA
  J %>% as.data.frame-> Z
  colnames(Z)<-1:S
  Z %>%
    rownames_to_column() %>%
    gather('column', 'Strength', 2:(S+1)) %>%
    ggplot(aes(y=as.numeric(rowname), x=as.numeric(column), fill=Strength), size=0.1)+
    geom_tile()+
    scale_y_reverse()+
    scale_fill_gradient2(low = 'red', mid='white', high='blue', na.value = 'grey',limits = c(-5,5) )+
    coord_fixed()+
    guides(fill=FALSE)+
    labs(subtitle=title)+
    theme(line = element_blank(),
          axis.text = element_blank())+
    theme(plot.caption=element_text(hjust=-0.5))+
    labs(x = NULL, y = NULL)-> P
  # theme(axis.line.y = element_line(colour = 'white'),
  #       axis.line.x = element_line(colour = 'white'),
  #       axis.ticks = element_blank(),
  #       axis.title = element_text(colour='white'), 
  #       axis.text = element_text(colour='white'))+
  # theme(panel.spacing = unit(c(0, 0, 0, 0), "cm"))+
  # theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
  
  return(P)
}