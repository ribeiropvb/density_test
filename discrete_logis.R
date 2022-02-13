discrete_logis <- function(x){
  print(fit.logis)
  fit.norm %>% plot()
  
  # Simulação de teste de Kolmogorov-Smirnov ####
  
  n.sims <- 5e3
  
  stats <- replicate(n.sims, {      
    r <- rlogis(n = length(x)
                , location = fit.logis$estimate[1]
                , scale = fit.logis$estimate[2]
    )
    estfit.norm <- fitdist(r, "logis") # added to account for the estimated parameters
    as.numeric(
      suppressWarnings(
        ks.test(r, "plogis"
                , location = estfit.norm$estimate[1]
                , scale = estfit.norm$estimate[2])$statistic
      )
    )      
  })
  plot(
    ecdf(stats), las = 1, lwd = 1.7
    , main = "KS-test statistic simulation (CDF) - Logistic"
    , col = "steelblue"
  )
  
  # Simulação PDF e CDF ####
  xs <- seq(
    from = fit.logis$estimate[1] - 4* fit.logis$estimate[2],
    to = fit.logis$estimate[1] + 4* fit.logis$estimate[2],
    length.out = 500
  )
  
  true.norm <- rlogis(
    1e6, location = fit.norm$estimate[1]
    , scale = fit.norm$estimate[2]
  )
  
  boot.pdf <- sapply(1:1000, function(i) {
    xi <- sample(x, size = length(x), replace=TRUE)
    MLE.est <- suppressWarnings(fitdist(xi, distr="logis"))  
    dlogis(
      xs, location = MLE.est$estimate[1],  scale = MLE.est$estimate[2]
    )
  }
  )
  
  boot.cdf <- sapply(1:1000, function(i) {
    xi <- sample(
      x
      , size = length(x)
      , replace=TRUE
    )
    MLE.est <- suppressWarnings(fitdist(xi, distr="logis"))  
    plogis(
      xs, location = MLE.est$estimate[1],  scale = MLE.est$estimate[2]
    )
  }
  )
  
  #-----------------------------------------------------------------------------
  # Plot PDF
  #-----------------------------------------------------------------------------
  
  par(bg="white", las=1, cex=1.2)
  plot(
    xs, boot.pdf[, 1], type="l"
    , col=rgb(.6, .6, .6, .1)
    , ylim = range(boot.pdf)
    , main = "Probability density function - Logistic"
    , xlab="x", ylab="Probability density"
  )
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
  plot(
    xs, boot.cdf[, 1], type="l"
    , col=rgb(.6, .6, .6, .1), ylim=range(boot.cdf)
    , main = "Cumulative distribution function - Logistic"
    , xlab="x", ylab="F(x)"
  )
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
