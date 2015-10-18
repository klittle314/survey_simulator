require(shiny)
require(ggplot2)
set.seed(1234)

p_calc <- function(p1,num,reps=10000){
  v1 <- rbinom(n=reps,size=num,p=p1)
  phats <- v1/num
  phat_out <- list(phats,quantile(phats,probs=c(0.01,0.05,0.25,0.5,0.75,0.95,0.99)))
}

#change to hypergeometric sampling to account for sampling from finite population
#suppose that the sample is 40% of the sample

p_calcHG <- function(p1,num,reps=10000,nfrac) {
  k1<- num
  tot <- round(k1/nfrac,digits=0)
  m1 <- round(p1*tot, digits=0)
  n1 <- tot - m1
  v2 <- rhyper(nn=reps,m=m1,n=n1,k=k1)
  phatsHG <- v2/k1
  phatsHG_out <- list(phatsHG,quantile(phatsHG,probs=c(0.01,0.05,0.25,0.5,0.75,0.95,0.99)))
}

