cont_weibull <- function(x){
  print(fit.weibull)
  fit.weibull %>% plot()
  
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
  plot(
    ecdf(stats), las = 1, lwd = 1.7
    , main = "KS-test statistic simulation (CDF) - Weibull"
    , col = "steelblue"
  )
  
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
  
  #-----------------------------------------------------------------------------
  # Plot PDF
  #-----------------------------------------------------------------------------
  
  par(bg="white", las=1, cex=1.2)
  plot(xs, boot.pdf[, 1], type="l"
       , col=rgb(.6, .6, .6, .1)
       , main = "Probability density function - Weibull"
       , ylim=range(boot.pdf)
       , xlab="x", ylab="Probability density")
  for(i in 2:ncol(boot.pdf)) lines(xs, boot.pdf[, i], col=rgb(.6, .6, .6, .1))
  
  # Add pointwise confidence bands
  
  quants <- apply(boot.pdf, 1, quantile, c(0.025, 0.5, 0.975))
  min.point <- apply(boot.pdf, 1, min, na.rm=TRUE)
  max.point <- apply(boot.pdf, 1, max, na.rm=TRUE)
  lines(xs, quants[1, ], col="red", lwd=1.5, lty=2)
  lines(xs, quants[3, ], col="red", lwd=1.5, lty=2)
  lines(xs, quants[2, ], col="darkred", lwd=2)
  
  #-----------------------------------------------------------------------------
  # Plot CDF
  #-----------------------------------------------------------------------------
  
  par(bg="white", las=1, cex=1.2)
  plot(xs, boot.cdf[, 1], type="l"
       , col=rgb(.6, .6, .6, .1)
       , main = "Cumulative distribution function - Weibull"
       , ylim=range(boot.cdf)
       , xlab="x", ylab="F(x)")
  for(i in 2:ncol(boot.cdf)) lines(xs, boot.cdf[, i], col=rgb(.6, .6, .6, .1))
  
  # Add pointwise confidence bands
  
  quants <- apply(boot.cdf, 1, quantile, c(0.025, 0.5, 0.975))
  min.point <- apply(boot.cdf, 1, min, na.rm=TRUE)
  max.point <- apply(boot.cdf, 1, max, na.rm=TRUE)
  lines(xs, quants[1, ], col="red", lwd=1.5, lty=2)
  lines(xs, quants[3, ], col="red", lwd=1.5, lty=2)
  lines(xs, quants[2, ], col="darkred", lwd=2)
  #lines(xs, min.point, col="purple")
  #lines(xs, max.point, col="purple")
}
