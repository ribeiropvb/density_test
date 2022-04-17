cont_exp <- function(x){
  
   fit.model <- fit.exp
  
  # Simulação de teste de Kolmogorov-Smirnov ####
  
  n.sims <- 5e3
  
  stats <- replicate(n.sims, {    
    r <- rexp(n = length(x)
              , rate = fit.exp$estimate[1]
    )
    estfit.norm <- fitdist(r, "exp") # added to account for the estimated parameters
    as.numeric(
      suppressWarnings(
        ks.test(r, "pexp"
                , rate = fit.exp$estimate[1])$statistic
      )
    )      
  })
  
  # Simulação PDF e CDF ####
  xs <- seq(
    from = 0,#fit.exp$estimate[1] - 4* fit.exp$sd[1],
    to = 50,#,fit.exp$estimate[1] + 4* fit.exp$sd[1],
    length.out = 500
  )
  
  true.exp <- rexp(
    1e6, rate = fit.exp$estimate[1]
  )
  
  boot.pdf <- sapply(1:1000, function(i) {
    xi <- sample(x, size = length(x), replace=TRUE)
    MLE.est <- suppressWarnings(fitdist(xi, distr="exp"))  
    dexp(
      xs, rate = MLE.est$estimate[1]
    )
  })
  
  boot.cdf <- sapply(1:1000, function(i) {
    xi <- sample(
      x
      , size = length(x)
      , replace=TRUE
    )
    MLE.est <- suppressWarnings(fitdist(xi, distr="exp"))  
    pexp(
      xs, rate = MLE.est$estimate[1]
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
