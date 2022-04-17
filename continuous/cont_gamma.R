cont_gamma <- function(x){
  
  fit.model <- fit.gamma
  
  # Simulação de teste de Kolmogorov-Smirnov ####
  
  n.sims <- 5e3
  
  stats <- replicate(n.sims, {      
    r <- rgamma(n = length(x)
                , shape = fit.gamma$estimate[1]
                , rate = fit.gamma$estimate[2]
    )
    estfit.gamma <- fitdist(r, "gamma") # added to account for the estimated parameters
    as.numeric(
      suppressWarnings(
        ks.test(r, "pgamma"
                , shape = fit.gamma$estimate[1]
                , rate = fit.gamma$estimate[2])$statistic
      )
    )      
  })
  
  # Simulação PDF e CDF ####
  xs <- seq(
    from = 0,
    to = fit.gamma$estimate[1] + 4*fit.gamma$estimate[2],
    length.out = 500
  )
  
  true.gamma <- rgamma(
    1e6
    , shape = fit.gamma$estimate[1]
    , rate = fit.gamma$estimate[2]
  )
  
  boot.pdf <- sapply(1:1000, function(i) {
    xi <- sample(x, size = length(x), replace=TRUE)
    MLE.est <- suppressWarnings(fitdist(xi, distr="gamma"))  
    dgamma(
      xs
      , shape = MLE.est$estimate[1]
      , rate = MLE.est$estimate[2]
    )
  })
  
  boot.cdf <- sapply(1:1000, function(i) {
    xi <- sample(
      x
      , size = length(x)
      , replace=TRUE
    )
    MLE.est <- suppressWarnings(fitdist(xi, distr="gamma"))  
    pgamma(
      xs
      , shape = MLE.est$estimate[1]
      , rate = MLE.est$estimate[2]
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
