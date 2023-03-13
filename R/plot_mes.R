## Purpose: Generate marginal effects plots for soil moisture and fire-free interval
## Author: Zack Steel

library(tidyverse)
library(brms)
library(tidybayes)
library(cowplot)
library(patchwork)

## read in model
m <- read_rds('model_fit.rds')

## Read in data
d <- read_csv("model_data.csv")

## function for scaling
scale2 <- function(x) {(x - mean(x, na.rm = T)) / sd(x, na.rm = T)}


## Standardize predictor variables
ds <- d %>% 
  mutate_at(.vars = vars(smf, ffi), scale2)

## Try plotting some prediction
p.sm <- ds %>% 
  group_by(site) %>% 
  modelr::data_grid(
    year = median(year),
    ffi = 0,
    smf = seq_range(smf, n = 30)) %>% 
  add_epred_draws(m, incl_autocor = F) %>% 
  median_qi(.width = .9) %>% 
  ggplot(aes(y = .epred, x = smf, ymin = .lower, ymax = .upper,
             group = site, fill = site, color = site)) +
  geom_lineribbon(fill = 'grey50', alpha = 1/2, color = NA) +
  geom_line(size = 2) +
  scale_color_viridis_d(name = 'Site', option = 'magma', end = .85,
                        labels = c('SN-Gran', 'SSPM-Gran', 'SSPM-Meta')) +
  coord_cartesian(ylim = c(0, 8)) +
  labs(y = 'Predicted Annual Recruitment', x = 'Soil Moisture Anomaly') +
  theme_bw() +
  theme(legend.justification = c(0, 1),
        legend.position = c(0.1, .95))

## for back-transforming axes
ffi.real <- c(0, 25, 50, 75, 100)
ffi.stan <- map_dbl(ffi.real, ~(.x - mean(d$ffi)) / sd(d$ffi))

p.ffi <- ds %>% 
  group_by(site) %>% 
  modelr::data_grid(
    year = median(year),
    ffi = seq_range(ffi, n = 30),
    smf = 0) %>% 
  add_epred_draws(m, incl_autocor = F) %>% 
  median_qi(.width = .9) %>% 
  ggplot(aes(y = .epred, x = ffi, ymin = .lower, ymax = .upper,
             group = site, fill = site, color = site)) +
  geom_lineribbon(fill = 'grey50', alpha = 1/2, color = NA) +
  geom_line(size = 2) +
  scale_color_viridis_d(option = 'magma', end = .85,
                        labels = c('SN-Gran', 'SSPM-Gran', 'SSPM-Meta')) +
  coord_cartesian(ylim = c(0, 8)) +
  scale_x_continuous(breaks = ffi.stan, labels = ffi.real) +
  labs(y = 'Predicted Annual Recruitment', x = 'Fire-free Interval (Years)') +
  theme_bw() +
  theme(legend.position = 'none')

p <- p.sm + p.ffi + ylab(NULL) 

save_plot('Figures/marg_effs.png', p,
          base_height = 4, base_width = 8)
