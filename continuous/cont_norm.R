cont_normal <- function(x){
  fit.model <- fit.norm
  
  # Simulação de teste de Kolmogorov-Smirnov ####
  
  n.sims <- 5e3
  
  stats <- replicate(n.sims, {      
    r <- rnorm(n = length(x)
               , mean = fit.norm$estimate[1]
               , sd = fit.norm$estimate[2]
    )
    estfit.norm <- fitdist(r, "norm") # added to account for the estimated parameters
    as.numeric(
      suppressWarnings(
        ks.test(r, "pnorm"
                , mean = estfit.norm$estimate[1]
                , sd = estfit.norm$estimate[2])$statistic
      )
    )      
  })
  
  # Simulação PDF e CDF ####
  xs <- seq(
    from = fit.norm$estimate[1] - 4* fit.norm$estimate[2],
    to = fit.norm$estimate[1] + 4* fit.norm$estimate[2],
    length.out = 500
  )
  
  true.norm <- rnorm(
    1e6, mean = fit.norm$estimate[1]
    , sd = fit.norm$estimate[2]
  )
  
  boot.pdf <- sapply(1:1000, function(i) {
    xi <- sample(x, size = length(x), replace=TRUE)
    MLE.est <- suppressWarnings(fitdist(xi, distr="norm"))  
    dnorm(
      xs, mean = MLE.est$estimate[1],  sd = MLE.est$estimate[2]
    )
  })
  
  boot.cdf <- sapply(1:1000, function(i) {
    xi <- sample(
      x
      , size = length(x)
      , replace=TRUE
    )
    MLE.est <- suppressWarnings(fitdist(xi, distr="norm"))  
    pnorm(
      xs, mean = MLE.est$estimate[1],  sd = MLE.est$estimate[2]
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
