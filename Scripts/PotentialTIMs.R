PotentialTIMs <- function(J){
  
  S <- dim(J)[1]
  
  # To make sure all taken care of, including non-predator prey interactions, use this syntax:
  
  which(J!=0&upper.tri(J), arr.ind=T) %>%  as.data.frame %>%
    mutate(Present=paste(row, col, sep='_' ))-> PreyPred
  
  expand.grid(1:S, 1:S, 1:S) %>% # all possible combinations
    as.data.frame %>% 
    dplyr::rename(prey=Var1, pred=Var2, mod= Var3 ) %>%
    filter(prey!=pred) %>% # species can't modify cannibalism
    filter(prey!=mod) %>% # species can't modifiy their interactons
    filter(pred!=mod) %>% 
    mutate(Interaction = paste(prey, pred, sep='_')) %>%
    filter(Interaction %in%  PreyPred$Present) -> TIMs
  
  return(TIMs)
}