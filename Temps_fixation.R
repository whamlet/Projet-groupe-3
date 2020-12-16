one.simu<- function(N, p){
  f1 <- -N^2*((1 - p)*log10(1 - p) + p*log10(p))
  return(f1)
}
