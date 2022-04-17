cont_lnormal <- function(x, y_axis, contexto = c('pdf','cdf')){
  
  fit.model <- fit.lnorm
  
  # Simulação de teste de Kolmogorov-Smirnov ####
  
  n.sims <- 5e3
  
  stats <- replicate(n.sims, {      
    r <- rlnorm(n = length(x)
               , meanlog = fit.norm$estimate[1]
               , sdlog = fit.norm$estimate[2]
    )
    estfit.lnorm <- fitdist(r, "lnorm") # added to account for the estimated parameters
    as.numeric(
      suppressWarnings(
        ks.test(r, "plnorm"
                , meanlog = estfit.lnorm$estimate[1]
                , sdlog = estfit.lnorm$estimate[2])$statistic
      )
    )      
  })
  
  # Simulação PDF e CDF ####
  xs <- seq(
    from = min(x) - 4*fit.lnorm$estimate[2],
    to = quantile(x,0.975) + 4*fit.lnorm$estimate[2],
    length.out = 500
  )
  
  true.lnorm <- rlnorm(
    1e6, meanlog = fit.lnorm$estimate[1]
    , sdlog = fit.lnorm$estimate[2]
  )
  
  boot.pdf <- sapply(1:1000, function(i) {
    xi <- sample(x, size = length(x), replace=TRUE)
    MLE.est <- suppressWarnings(fitdist(xi, distr="lnorm"))  
    dlnorm(
      xs, meanlog = MLE.est$estimate[1],  sdlog = MLE.est$estimate[2]
    )
  })
  
  boot.cdf <- sapply(1:1000, function(i) {
    xi <- sample(
      x
      , size = length(x)
      , replace=TRUE
    )
    MLE.est <- suppressWarnings(fitdist(xi, distr="lnorm"))  
    plnorm(
      xs, meanlog = MLE.est$estimate[1],  sdlog = MLE.est$estimate[2]
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
