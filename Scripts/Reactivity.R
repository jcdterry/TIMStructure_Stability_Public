Reactivity <- function(J){
  return(max(Re(eigen((J +t(J))/2, only.values = TRUE)$values)))
}