binomial_estimation <- function(y){
  for (j in 1:1000) {
    xi <- sample(x, 150, replace = T)
    ## likelihood function
    llik2 <- function(p,n) {
      -sum(dbinom(xi,prob=p,size=n,log=TRUE))
    }
    ## possible N values (15 to 50)
    nvec <- max(x1):50
    Lvec <- numeric(length(nvec))
    for (i in 1:length(nvec)) {
      ## optim() wants method="Brent"/lower/upper for 1-D optimization
      Lvec[i] <- optim(par=0.5,fn=llik2,n=nvec[i],method="Brent",
                       lower=0.001,upper=0.999)$val
    }
    
    val[j] <- bind_cols(
      nvec,Lvec
    ) %>% 
      magrittr::set_colnames(c('n','lik')) %>% 
      filter(lik == min(lik)) %>% 
      .$n %>% 
      suppressMessages()
    return(mean(val))
    
  }
}
