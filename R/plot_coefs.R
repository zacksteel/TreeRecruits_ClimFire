## Purpose: plot coefficients for final model
## Author: Zack steel

library(tidyverse)
library(brms)
library(tidybayes)
library(cowplot)
library(modelr)
library(ggeffects)
library(cowplot)

m <- read_rds('model_fit.rds')

## looking at the best model
p <- m %>% 
  spread_draws(r_site[site, term]) %>% 
  summarise_draws() %>% 
  mutate(label = case_when(term == 'ffi' ~ 'Fire Free Interval',
                           term == 'smf' ~ 'Soil Moisture Anomaly',
                           term == 'Intercept' ~ 'Intercept'),
         label = as.factor(label),
         label = fct_relevel(label, 'Intercept'),
         label = fct_rev(label),
         site = fct_rev(site)) %>% 
  ggplot(aes(y = label, x = median, xmin = q5, xmax = q95, color = site)) +
  geom_pointinterval(size = 6, position = 'dodge') +
  geom_vline(xintercept = 0, linetype = 2) +
  scale_color_viridis_d(name = 'Site', option = 'magma', end = .85,
                        labels = c('SN-Gran', 'SSPM-Gran', 'SSPM-Meta')) +
  theme_bw() +
  labs(y = 'Model Parameter', x = 'Effect Estimate')

save_plot('Figures/Coeffs.png', p)
