NicheModel<- function (size = 100, connectance = 0.25) {
   # Derived from eyeball::eye.foodweb.niche() Allesina et al 2015
  
  K <- matrix(0, size, size)
  ni <- sort(runif(size, 0, 1))
  beta <- 1/connectance - 1
  ri <- rbeta(size, 1, beta) * ni
  ri[1] <- 0
  ci <- numeric(size)
  for (i in 1:size) {
    ci[i] <- runif(1, ri[i]/2, min(ni[i], 1 - ri[i]/2))
    upper <- ci[i] + ri[i]/2
    lower <- ci[i] - ri[i]/2
    for (j in 1:size) {
      if (ni[j] > lower & ni[j] < upper) 
        K[j, i] <- 1
      if (K[i, j] == 1) {
        if (runif(1) < 0.5) {
          K[i, j] <- 0
        }
        else {
          K[j, i] <- 0
        }
      }
    }
  }
  diag(K) <- 0
  L <- sum(K)
  expected.L <- choose(size, 2) * connectance
  variance.L <- choose(size, 2) * connectance * (1 - connectance)
  if (L < (expected.L - 2 * sqrt(variance.L)) | L > (expected.L +  2 * sqrt(variance.L))) {
  #  warning("Failed: wrong number of links. Going again...")
    return(NicheModel(size, connectance))
  }
  g <- graph_from_adjacency_matrix(K, mode = "directed")
  if (is_connected(g, mode = "weak") == FALSE) {
    #warning("Failed: Not connected. Going again...")
    return(NicheModel(size, connectance))
  }
  ts <- topo_sort(g)
  if (length(ts) == size) {
    K <- K[ts, ts]
    g <- graph_from_adjacency_matrix(K, mode = "directed")
  }
  return(as_edgelist(g))
}
