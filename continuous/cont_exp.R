cont_exp <- function(x){
  print(fit.exp)
  fit.exp %>% plot()
  
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
  plot(
    ecdf(stats), las = 1, lwd = 1.7
    , main = "KS-test statistic simulation (CDF) - Exponencial"
    , col = "steelblue"
  )
  
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
  }
  )
  
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
  }
  )
  
  #-----------------------------------------------------------------------------
  # Plot PDF
  #-----------------------------------------------------------------------------
  
  par(bg="white", las=1, cex=1.2)
  plot(xs, boot.pdf[, 1], type="l"
       , col=rgb(.6, .6, .6, .1)
       , main = "Probability density function - Exponencial"
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
       , main = "Cumulative distribution function - Exponencial"
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
