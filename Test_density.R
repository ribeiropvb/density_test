
library(tidyverse)
library(magrittr)
library(fitdistrplus)

x <- rexp(500, 0.1)

if(x == round(x,0)){
  
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
    
    discrete_normal(x)
    
  } else if(mod_selec == 'logis'){
    
    discrete_logis(x)
    
  } else if(mod_selec == 'binom'){
    
    discrete_binom(x)
    
  } else if(mod_selec == 'nbinom'){
    
    discrete_nbinom(x)
    
  } else if(mod_selec == 'geom'){
    
    discrete_geom(x)
    
  } else if(mod_selec == 'pois'){
    
    discrete_pois(x)
    
  }
  
} else{
  
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
  
  mod_selec <- resumo %>%
    arrange(BIC) %>%
    .$distr %>% .[1]
  err <- try(fitdist(x, mod_selec)) %>% class
  beta_test <- sum(between(x, 0, 1)) == length(x)
  
  if(mod_selec == 'norm'){
    
    cont_normal(x)
    
  } else if(mod_selec == 'lnorm'){
    
    cont_exp(x)
    
  }  else if(err != 'try-error' & mod_selec == 'exp'){
    
    cont_exp(x)
    
  } else if(err != 'try-error' & mod_selec == 'gamma'){
    
    cont_gamma(x)
    
  }  else if(err != 'try-error' & mod_selec == 'weibull'){
    
    cont_weibull(x)
    
  }  else if(beta_test & mod_selec == 'beta'){
    
    cont_beta(x)
    
  }
  
}

