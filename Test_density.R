
library(tidyverse)
library(magrittr)
library(fitdistrplus)

x <- rpois(50, 75)

if(x == round(x,0)){
  
  fit.norm <- fitdist(x, discrete = T, dist = 'norm')
  fit.logis <- fitdist(x, discrete = T, dist = 'logis')
  fit.nbinom <- fitdist(x, discrete = T, dist = 'nbinom')
  fit.geom <- fitdist(x, discrete = T, dist = 'geom')
  fit.pois <- fitdist(x, discrete = T, dist = 'pois')
  
  resumo <- tibble(
    Modelo = c(
      'Normal','Logistic','Negative Binomial'
      , 'Geometric', 'Poisson'
    ),
    AIC = c(
      fit.norm$aic, fit.logis$aic, fit.nbinom$aic
      , fit.geom$aic, fit.pois$aic
    ),
    BIC = c(
      fit.norm$bic, fit.logis$bic, fit.nbinom$bic
      , fit.geom$bic, fit.pois$bic
    ),
    LogLik = c(
      fit.norm$loglik, fit.logis$loglik, fit.nbinom$loglik
      , fit.geom$loglik, fit.pois$loglik
    ),
    distr = c('norm','logis','nbinom','geom','pois')
  ) %>% arrange(BIC)
  mod_selec <- resumo %>% .$distr %>% .[1]
  
  if(mod_selec == 'norm'){
    
    discrete_normal(x)
    
  } else if(mod_selec == 'logis'){
    
    discrete_logis(x)
    
  } else if(mod_selec == 'nbinom'){
    
    discrete_nbinom(x)
    
  } else if(mod_selec == 'geom'){
    
    discrete_geom(x)
    
  } else if(mod_selec == 'pois'){
    
    discrete_pois(x)
    
  }
  
}
