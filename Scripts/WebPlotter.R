WebPlotter<-function(web){
  
  S <-  dim(web)[1]
  
  lay<-matrix(nrow=S,ncol=2) # create a matrix with one column as runif, the other as trophic level
  lay[,1]<-sample((1:S)*4000, S)
  lay[,2]<-GetTL( web<0)
  
  sign<- sign(web)
  Scaled<-((abs(web)^(1/9)*sign))
  Scaled[is.nan(Scaled)]<-0
  Scaled%>% 
    t %>%  # Igraph expects a different input to a predation matrix to generate a food web
    graph_from_adjacency_matrix(weighted = TRUE,
                                mode = 'directed' )->gr
  EdgeWidths=(abs(E(gr)$weight)+1)
  
  EdgeCols=round(abs(E(gr)$weight), 1)*10
  EdgeCols <- 100 - EdgeCols * (100 %/% max(EdgeCols))  # Scale up to 100
  EdgeCols<- paste0('grey', EdgeCols)
  

  plot.igraph(gr,layout=lay, edge.curved=0.2,
              edge.width=EdgeWidths,
              edge.color=adjustcolor(EdgeCols, alpha.f = 0.5),
              edge.arrow.size=0.5)
}
