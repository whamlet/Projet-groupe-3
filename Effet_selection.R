selection <- function(N,p){
  h <- 0
  while(p > 0 && p < 1){
    Individu <- sample(c(1,0), 2*N, replace=TRUE, prob =c(p,1-p))
    p <-sum(Individu)/(2*N)
    h <- h + 1
    j <- print(distIndividu <- table(Individu))
  }
  return(j)
}
