library(tidyverse)
library(broom)
library(broom.mixed)
library(furrr)
library(nlme)

plan(multisession, workers = 3)

set.seed(1)


# Add ylag1 as a predictor

doOneSim <- function(seedVal) {
  set.seed(seedVal)
  
  y <- arima.sim(model=list(order=c(0,1,0)), n=200, mean=0, sd=5)  # random walk 1
  x <- arima.sim(model=list(order=c(0,1,0)), n=200, mean=0, sd=5)  # random walk 2
  
  df <- data.frame(y=as.numeric(y), x=as.numeric(x), time=1:length(y))
  
  rm(x, y)
  
  df <- df %>% mutate(ylag1 = lag(y)) %>% drop_na()
  
  #ggplot(df, aes(x=time, y=y)) + geom_line()

  ## OLS
  lm_mod <- lm(y ~ ylag1 + x, data=df)  
  
  ## GLS assuming AR(1) errors
  gls_mod <- gls(y ~ ylag1 + x, data=df,
                 correlation=corARMA(p=1, q=0), method="ML")  
  
  ## p-values for x
  ols_stat <- tidy(lm_mod) %>% filter(term=='x') %>% select(p.value)
  gls_stat <- tidy(gls_mod) %>% filter(term=='x') %>% select(p.value)
  
  
  ## type 1 error flag
  out <- data.frame(ols_error = as.integer(ols_stat <= 0.10),
              gls_err = as.integer(gls_stat <= 0.10))
}

allSim <- future_map_dfr(1:2000, doOneSim,
                         .options=furrr_options(seed=NULL,
                                                packages=c('broom', 'nlme', 'broom.mixed')
                                                )
                         )

future:::ClusterRegistry("stop")

## Even in the presence of a lagged response (ylag1) on the RHS
## OLS and GLS have type 1 error rates of 25.5% with an alpha of 10%

sapply(allSim, mean)  
