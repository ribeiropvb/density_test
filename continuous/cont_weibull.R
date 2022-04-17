cont_weibull <- function(x){
  
  fit.model <- fit.weibull
  
  # Simulação de teste de Kolmogorov-Smirnov ####
  
  n.sims <- 5e3
  
  stats <- replicate(n.sims, {      
    r <- rweibull(n = length(x)
                , shape = fit.weibull$estimate[1]
                , scale = fit.weibull$estimate[2]
    )
    estfit.weibull <- fitdist(r, "weibull") # added to account for the estimated parameters
    as.numeric(
      suppressWarnings(
        ks.test(r, "pweibull"
                , shape = fit.weibull$estimate[1]
                , scale = fit.weibull$estimate[2])$statistic
      )
    )      
  })
  

  # Simulação PDF e CDF ####
  xs <- seq(
    from = 0,
    to = max(x) + 2*sd(x),#fit.weibull$estimate[1] + 4*fit.weibull$estimate[2],
    length.out = 500
  )
  
  true.weibull <- rweibull(
    1e6
    , shape = fit.weibull$estimate[1]
    , scale = fit.weibull$estimate[2]
  )
  
  boot.pdf <- sapply(1:1000, function(i) {
    xi <- sample(x, size = length(x), replace=TRUE)
    MLE.est <- suppressWarnings(fitdist(xi, distr="weibull"))  
    dweibull(
      xs
      , shape = MLE.est$estimate[1]
      , scale = MLE.est$estimate[2]
    )
  })
  
  boot.cdf <- sapply(1:1000, function(i) {
    xi <- sample(
      x
      , size = length(x)
      , replace=TRUE
    )
    MLE.est <- suppressWarnings(fitdist(xi, distr="weibull"))  
    pweibull(
      xs
      , shape = MLE.est$estimate[1]
      , scale = MLE.est$estimate[2]
    )
  })
  
  result <- list(
    fit.model = fit.model
    , stats = stats
    , xs = xs
    , boot.pdf = boot.pdf
    , boot.cdf = boot.cdf
  )
  
  return(result)
  
}
