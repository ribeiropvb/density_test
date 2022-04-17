cont_beta <- function(x){
  
  fit.model <- fit.beta
  
  # Simulação de teste de Kolmogorov-Smirnov ####
  
  n.sims <- 5e3
  
  stats <- replicate(n.sims, {      
    r <- rbeta(n = length(x)
                  , shape1 = fit.beta$estimate[1]
                  , shape2 = fit.beta$estimate[2]
    )
    estfit.beta <- fitdist(r, "beta") # added to account for the estimated parameters
    as.numeric(
      suppressWarnings(
        ks.test(r, "pbeta"
                , shape1 = fit.beta$estimate[1]
                , shape2 = fit.beta$estimate[2])$statistic
      )
    )      
  })
  
  # Simulação PDF e CDF ####
  xs <- seq(
    from = 0,
    to = 1,
    length.out = 500
  )
  
  true.beta <- rbeta(
    1e6
    , shape1 = fit.beta$estimate[1]
    , shape2 = fit.beta$estimate[2]
  )
  
  boot.pdf <- sapply(1:1000, function(i) {
    xi <- sample(x, size = length(x), replace=TRUE)
    MLE.est <- suppressWarnings(fitdist(xi, distr="beta"))  
    dbeta(
      xs
      , shape1 = MLE.est$estimate[1]
      , shape2 = MLE.est$estimate[2]
    )
  })
  
  boot.cdf <- sapply(1:1000, function(i) {
    xi <- sample(
      x
      , size = length(x)
      , replace=TRUE
    )
    MLE.est <- suppressWarnings(fitdist(xi, distr="beta"))
    pbeta(
      xs
      , shape1 = MLE.est$estimate[1]
      , shape2 = MLE.est$estimate[2]
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
