
library(tidyverse)
library(magrittr)
library(fitdistrplus)

x <- data("GasolineYield")
x <- GasolineYield$yield

if(sum(x == round(x,0)) == length(x)){
  
  n <- trunc(binomial_estimation(x))
  
  fit.norm <- fitdist(x, discrete = T, dist = 'norm')
  fit.logis <- fitdist(x, discrete = T, dist = 'logis')
  fit.binom <- fitdist(
    data = x, dist = "binom"
    , fix.arg = list(size = n)
    , start = list(prob=0.1)
  )
  fit.nbinom <- fitdist(x, discrete = T, dist = 'nbinom')
  fit.geom <- fitdist(x, discrete = T, dist = 'geom')
  fit.pois <- fitdist(x, discrete = T, dist = 'pois')
  
  resumo <- tibble(
    Modelo = c(
      'Normal','Logistic','Binomial'
      ,'Negative Binomial'
      , 'Geometric', 'Poisson'
    ),
    AIC = c(
      fit.norm$aic, fit.logis$aic, fit.binom$aic
      , fit.nbinom$aic, fit.geom$aic, fit.pois$aic
    ),
    BIC = c(
      fit.norm$bic, fit.logis$bic, fit.binom$bic
      , fit.nbinom$bic, fit.geom$bic, fit.pois$bic
    ),
    LogLik = c(
      fit.norm$loglik, fit.logis$loglik, fit.binom$loglik
      , fit.nbinom$loglik, fit.geom$loglik, fit.pois$loglik
    ),
    distr = c('norm','logis','binom','nbinom','geom','pois')
  ) %>% arrange(BIC)
  mod_selec <- resumo %>% .$distr %>% .[1]
  
  if(mod_selec == 'norm'){
    
    source('https://raw.githubusercontent.com/ribeiropvb/density_test/main/discrete/discrete_normal.R')
    discrete_normal(x)
    
  } else if(mod_selec == 'logis'){
    
    source('https://raw.githubusercontent.com/ribeiropvb/density_test/main/discrete/discrete_logis.R')
    discrete_logis(x)
    
  } else if(mod_selec == 'binom'){
    
    source('https://raw.githubusercontent.com/ribeiropvb/density_test/main/discrete/Binominal_Estimation/binomial_estimation.R')
    source('https://raw.githubusercontent.com/ribeiropvb/density_test/main/discrete/discrete_binom.R')
    discrete_binom(x)
    
  } else if(mod_selec == 'nbinom'){
    
    source('https://raw.githubusercontent.com/ribeiropvb/density_test/main/discrete/discrete_nbinom.R')
    discrete_nbinom(x)
    
  } else if(mod_selec == 'geom'){
    
    source('https://raw.githubusercontent.com/ribeiropvb/density_test/main/discrete/discrete_geom.R')
    discrete_geom(x)
    
  } else if(mod_selec == 'pois'){
    
    source('https://raw.githubusercontent.com/ribeiropvb/density_test/main/discrete/discrete_pois.R')
    discrete_pois(x)
    
  }
  
} else {
  
  fit.norm <- fitdist(x, dist = 'norm')
  fit.lnorm <- fitdist(x, dist = 'lnorm')
  fit.exp <- fitdist(x, dist = 'exp')
  fit.gamma <- fitdist(x, dist = 'gamma')
  fit.weibull <- fitdist(x, dist = 'weibull')
  
  resumo <- tibble(
    Modelo = c(
      'Normal', 'Log-Normal' ,'Exponencial'
      , 'Gamma', 'Weibull'
    ),
    AIC = c(
      fit.norm$aic, fit.lnorm$aic, fit.exp$aic
      , fit.gamma$aic, fit.weibull$aic
    ),
    BIC = c(
      fit.norm$bic, fit.lnorm$bic, fit.exp$bic
      , fit.gamma$bic, fit.weibull$bic
    ),
    LogLik = c(
      fit.norm$loglik, fit.lnorm$loglik, fit.exp$loglik
      , fit.gamma$loglik, fit.weibull$loglik
    ),
    distr = c('norm', 'lnorm','exp','gamma','weibull')
  )
  
  if(sum(between(x, 0, 1)) == length(x)){
    fit.beta <- fitdist(x, dist = 'beta')
    resumo %<>% bind_rows(
      tibble(
        Modelo = 'Beta',
        AIC = fit.beta$aic,
        BIC = fit.beta$bic,
        LogLik = fit.beta$loglik,
        distr = 'beta'
      ) %>% arrange(BIC)
    )
  }
  
  resumo %<>%
    arrange(BIC)
  mod_selec <- resumo %>%
    .$distr %>% .[1]
  err <- try(fitdist(x, mod_selec)) %>% class
  beta_test <- sum(between(x, 0, 1)) == length(x)
  
  if(mod_selec == 'norm'){
    
    source('https://raw.githubusercontent.com/ribeiropvb/density_test/main/continuous/cont_norm.R')
    fit <- cont_normal(x)
    
  } else if(mod_selec == 'lnorm'){
    
    source('https://raw.githubusercontent.com/ribeiropvb/density_test/main/continuous/cont_lnormal.R')
    fit <- cont_lnormal(x)
    
  }  else if(err != 'try-error' & mod_selec == 'exp'){
    
    source('https://raw.githubusercontent.com/ribeiropvb/density_test/main/continuous/cont_exp.R')
    fit <- cont_exp(x)
    
  } else if(err != 'try-error' & mod_selec == 'gamma'){
    
    source('https://raw.githubusercontent.com/ribeiropvb/density_test/main/continuous/cont_gamma.R')
    fit <- cont_gamma(x)
    
  }  else if(err != 'try-error' & mod_selec == 'weibull'){
    
    source('https://raw.githubusercontent.com/ribeiropvb/density_test/main/continuous/cont_weibull.R')
    fit <- cont_weibull(x)
    
  }  else if(beta_test & mod_selec == 'beta'){
    
    source('https://raw.githubusercontent.com/ribeiropvb/density_test/main/continuous/cont_beta.R')
    fit <- cont_beta(x)
    
  }
  
}

attach(fit)
Modelo <- resumo[1,1]
theme_clear <- theme_light() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

print(fit.model)
fit.model %>% plot()

plot(
  ecdf(stats), las = 1, lwd = 1.7
  , main = paste("KS-test statistic simulation (CDF) -", Modelo)
  , col = "steelblue"
)

#source('https://raw.githubusercontent.com/ribeiropvb/density_test/main/plot_function/generate_plot.R')

#-----------------------------------------------------------------------------
# Plot PDF
#-----------------------------------------------------------------------------

generate_plot(data = boot.pdf, contexto = 'pdf')

#-----------------------------------------------------------------------------
# Plot CDF
#-----------------------------------------------------------------------------

generate_plot(data = boot.cdf, contexto = 'cdf')
