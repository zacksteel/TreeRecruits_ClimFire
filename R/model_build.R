## Purpose: Build a final soil moisture plus fire model of recruitment
## Author: Zack Steel

library(tidyverse)
library(brms)

## Read in data
d <- read_csv("model_data.csv")

## function for scaling
scale2 <- function(x) {(x - mean(x, na.rm = T)) / sd(x, na.rm = T)}
  

## Standardize predictor variables
ds <- d %>% 
  mutate_at(.vars = vars(smf, ffi), scale2)

## set priors
prior1 <- prior(exponential(1), class = sd)

## ~40 min 
m <- brm(n ~ 0 + (1 + smf + ffi | site) +
               ar(time = year, gr = site), 
             data = ds, family = zero_inflated_poisson, 
             prior = prior1,
             control = list(adapt_delta = 0.99, max_treedepth = 15),
             chains = 4, cores = 4, iter = 2000) 

brms::posterior_summary(m) %>% 
  as.data.frame() %>% 
  mutate_all(round,4)

write_rds(m, 'model_fit.rds', compress = "bz")
