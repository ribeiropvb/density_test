generate_plot <- function(data){
  
  # Add pointwise confidence bands
  
  quants <- apply(data, 1, quantile, c(0.025, 0.5, 0.975))
  min.point <- apply(data, 1, min, na.rm=TRUE)
  max.point <- apply(data, 1, max, na.rm=TRUE)
  
  pdf <- bind_cols(
    xs = xs %>% as_tibble(),
    pdf = data %>% as_tibble(),
    inferior = quants[1, ],
    mean = quants[2, ],
    superior = quants[3, ]
  ) %>% magrittr::set_colnames(c(
    'xs', paste0('pdf',seq(1, ncol(data)))
    , 'inferior','mean','superior'
  ))
  
  pdf %<>% pivot_longer(cols = -xs, names_to = "pdf") %>% 
    mutate(
      Cor = case_when(
        str_detect(pdf,'pdf') ~ 'grey',
        pdf == 'mean' ~ 'darkred',
        T ~ 'red'
      ) ,
      alpha = ifelse(str_sub(pdf, 1,3) == 'pdf',1, 2),
      size = ifelse(str_sub(pdf, 1,3) == 'pdf',0.15, 0.25),
      lint = ifelse(Cor == 'red', 'longdash','solid')
    )
  
  ggplot()+
    geom_line(
      data = pdf %>% filter(Cor == 'grey'),
      aes(
        x = xs, y = value, group = pdf
        , alpha = alpha, linetype = lint
      ), color = 'grey')+
    geom_line(
      data = pdf %>% filter(pdf == 'mean'),
      aes(
        x = xs, y = value
      ), color = 'darkred'
      , size = 1.25
    )+
    geom_line(
      data = pdf %>% filter(Cor == 'red'),
      aes(
        x = xs, y = value, group = pdf
      ), color = 'red'
      , size = 1.25
      , linetype = 'longdash'
    )+
    ylim(range(data))+
    xlab("x")+ylab("Probability density")+
    theme_clear+
    theme(legend.position = 'none')
  
}
