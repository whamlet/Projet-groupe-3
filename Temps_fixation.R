one.simu<- function(N, p){
  population<- (sample(c(1,0), replace = TRUE, size =2*N, prob =c(p, 1-p)))
  a <- sum(population == "1")
  p <- ((N*2) - a)/(2*N)
  f1 <- -N^2*((1 - p)*log10(1 - p) + p*log10(p))
  return(f1)
}
