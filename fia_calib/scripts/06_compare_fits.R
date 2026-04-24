# DESCRIPTION ==================================================================
#>
#> Purpose:
#> 
#> Outputs:
#> 
#> Notes:
#>
# ==============================================================================
library(here)
library(ggplot2)
library(dplyr)
library(patchwork)

lvl1 <- readRDS(here('model_outputs', 'lvl1_fit.RDS'))$draws
lvl2 <- readRDS(here('model_outputs', 'lvl2_plot_species_fit.RDS'))$draws
lvl3 <- readRDS(here('model_outputs', 'lvl3_evap_density_species_fit.RDS'))$draws

# Joint distributions ----------------------------------------------------------
## level 1 ----
lvl1_mu <- ggplot(lvl1, aes(x = mu))+
  geom_histogram(col = 'black', fill = 'lightgrey')+
  scale_y_reverse()+
  xlim(0.5, 0.55)
lvl1_sigma <- ggplot(lvl1, aes(y = sigma))+
  geom_histogram(col = 'black', fill = 'lightgrey')+
  scale_x_reverse()
lvl1_joint <- ggplot(lvl1, aes(x = mu, y = sigma))+
  geom_bin_2d(bins = 40)+
  xlim(0.5, 0.55)+
  scale_fill_viridis_c()

layout <- 'ABB
           ABB
           CDD'
lvl1_sigma+lvl1_joint+plot_spacer()+lvl1_mu+plot_layout(design = layout, 
                                                        axis_titles = 'collect')&
  theme_bw()

## level 2 ----
lvl2_gg <- select(lvl2, beta_larch, sigma, plot_mean, sigma_plot)
pars <- c('beta_larch', 'sigma', 'plot_mean', 'sigma_plot')

# beta_larch vs everything
p2.1 <- ggplot(lvl2_gg)+
  geom_density_2d(aes(x = sigma, y = exp(beta_larch)))+
  scale_fill_viridis_c()+
  labs(title = expr(paste(sigma['tree'])),
       x = '',
       y = expr(paste(beta['larch'])))
p2.2 <- ggplot(lvl2_gg)+
  geom_bin_2d(aes(x = exp(plot_mean), y = exp(beta_larch)))+
  scale_fill_viridis_c()+
  labs(title = expr(paste(mu['plot'])),
       x = '',
       y = expr(paste(beta['larch'])))
p2.3 <- ggplot(lvl2_gg)+
  geom_bin_2d(aes(x = exp(sigma_plot), y = exp(beta_larch)))+
  scale_fill_viridis_c()+
  labs(title = expr(paste(sigma['plot'])),
       x = '',
       y = expr(paste(beta['larch'])))

# sigma vs the rest
p2.4 <- ggplot(lvl2_gg)+
  geom_bin_2d(aes(x = exp(plot_mean), y = sigma))+
  scale_fill_viridis_c()+
  labs(y = expr(paste(sigma['tree'])),
       x = '')
p2.5 <- ggplot(lvl2_gg)+
  geom_bin_2d(aes(x = exp(sigma_plot), y = sigma))+
  labs(y = expr(paste(sigma['tree'])),
       x = '')+
  scale_fill_viridis_c()

# sigma plot vs sigma tree
p2.6 <- ggplot(lvl2_gg)+
  geom_bin_2d(aes(x = exp(sigma_plot), y = exp(plot_mean)))+
  scale_fill_viridis_c()+
  labs(y = expr(paste(mu['plot'])),
       x = '')
# Marginals
# p2.7 <- ggplot(lvl2_gg)+
#   geom_histogram(aes(y = beta_larch), fill = 'lightgrey', col = 'black')+
#   scale_x_reverse()+
#   labs(x = '', y = expr(paste(beta['larch'])))
# 
# p2.8 <- ggplot(lvl2_gg)+
#   geom_histogram(aes(x = sigma), fill = 'lightgrey', col = 'black')+
#   scale_y_reverse()+
#   labs(x = '', y = '')
# 
# p2.9 <- ggplot(lvl2_gg)+
#   geom_histogram(aes(x = plot_mean), fill = 'lightgrey', col = 'black')+
#   scale_y_reverse()+
#   labs(x = '', y ='')
# p2.10 <- ggplot(lvl2_gg)+
#   geom_histogram(aes(x = sigma_plot), fill = 'lightgrey', col = 'black')+
#   scale_y_reverse()+
#   labs(y = '', x = '')
# 
# layout <- 'IAABBCC
#            IAABBCC
#            IAABBCC
#            MJJEEFF
#            DDDEEFF
#            DDDKKHH
#            DDDGGHH
#            DDDGGNN
#            DDDGGLL'
layout <- 'AABBCC
           DDEEFF
           DDGGHH'
p2.1+p2.2+p2.3+plot_spacer()+p2.4+p2.5+plot_spacer()+p2.6+#+p2.7+p2.8+p2.9+p2.10+
  plot_layout(design = layout,
              guides = 'collect', axis_titles = 'collect')&
  theme_bw()&
  theme(axis.title.y = element_text(size = 18),
        plot.title = element_text(size = 18),
        legend.position = 'none')

# Level 3 ----------------------------------------------------------------------
lvl3_gg <- select(lvl3, sigma, sigma_plot, beta_evap, beta_density, beta_0, beta_larch)
# first row: b0 vs everything
p3.1 <- ggplot(lvl3_gg)+
  geom_bin_2d(aes(x = exp(beta_evap), y = exp(beta_0)))+
  labs(title = expr(paste(beta['evap'])),
       x = '',
       y = expr(paste(beta[0])))
p3.2 <- ggplot(lvl3_gg)+
  geom_bin_2d(aes(x = exp(beta_density), y = exp(beta_0)))+
  labs(title = expr(paste(beta['density'])),
       x = '',
       y = expr(paste(beta[0])))
p3.3 <- ggplot(lvl3_gg)+
  geom_bin_2d(aes(x = exp(beta_larch), y = exp(beta_0)))+
  labs(title = expr(paste(beta['larch'])),
       x = '',
       y = expr(paste(beta[0])))
p3.4 <- ggplot(lvl3_gg)+
  geom_bin_2d(aes(x = exp(sigma_plot), y = exp(beta_0)))+
  labs(title = expr(paste(sigma['plot'])),
       x = '',
       y = expr(paste(beta[0])))
p3.5 <- ggplot(lvl3_gg)+
  geom_bin_2d(aes(x = sigma, y = exp(beta_0)))+
  labs(title = expr(paste(sigma['tree'])),
       x = '',
       y = expr(paste(beta[0])))

# second row: beta_evap vs everything
p3.6 <- ggplot(lvl3_gg)+
  geom_bin_2d(aes(x = exp(beta_density), y = exp(beta_evap)))+
  labs(x = '',
       y = expr(paste(beta['evap'])))
p3.7 <- ggplot(lvl3_gg)+
  geom_bin_2d(aes(x = exp(beta_larch), y = exp(beta_evap)))+
  labs(x = '',
       y = expr(paste(beta['evap'])))
p3.8 <- ggplot(lvl3_gg)+
  geom_bin_2d(aes(x = exp(sigma_plot), y = exp(beta_evap)))+
  labs(x = '',
       y = expr(paste(beta['evap'])))
p3.9 <- ggplot(lvl3_gg)+
  geom_bin_2d(aes(x = sigma, y = exp(beta_evap)))+
  labs(x = '',
       y = expr(paste(beta['evap'])))

# third row: exp(beta_density) vs everything
p3.10 <- ggplot(lvl3_gg)+
  geom_bin_2d(aes(x = exp(beta_larch), y = exp(beta_density)))+
  labs(x = '',
       y = expr(paste(beta['density'])))
p3.11 <- ggplot(lvl3_gg)+
  geom_bin_2d(aes(x = exp(sigma_plot), y = exp(beta_density)))+
  labs(x = '',
       y = expr(paste(beta['density'])))
p3.12 <- ggplot(lvl3_gg)+
  geom_bin_2d(aes(x = sigma, y = exp(beta_density)))+
  labs(x = '',
       y = expr(paste(beta['density'])))

# fourth row: beta larch vs everything
p3.13 <- ggplot(lvl3_gg)+
  geom_bin_2d(aes(x = exp(sigma_plot), y = exp(beta_larch)))+
  labs(x = '',
       y = expr(paste(beta['larch'])))
p3.14 <- ggplot(lvl3_gg)+
  geom_bin_2d(aes(x = sigma, y = exp(beta_larch)))+
  labs(x = '',
       y = expr(paste(beta['larch'])))

# fifth row: sigma plot vs sigma tree
p3.15 <- ggplot(lvl3_gg)+
  geom_bin_2d(aes(x = sigma, y = exp(sigma_plot)))+
  labs(x = '',
       y = expr(paste(sigma['plot'])))

layout_lvl3 <- 'AABBCCDDEE
                PPFFGGHHII
                QQQQJJKKLL
                RRRRRRMMNN
                SSSSSSSSOO'

p3.1+p3.2+p3.3+p3.4+p3.5+p3.6+p3.7+p3.8+p3.9+p3.10+p3.11+p3.12+p3.13+p3.14+p3.15+
  plot_layout(design = layout_lvl3, axes = 'collect', guides = 'collect')&
  scale_fill_viridis_c()&
  theme_bw()&
  theme(legend.position = 'none',
        axis.title.y = element_text(size = 14))

# Level 3 Marginals ------------------------------------------------------------
lvl3_gg |>
  tidyr::pivot_longer(cols = everything(), names_to = 'coeff') |>
  mutate(value_adj = ifelse(coeff == 'sigma', value, exp(value))) |>
  group_by(coeff) |>
  mutate(post_mean = mean(value_adj),
         post_lower = quantile(value_adj, 0.025),
         post_upper = quantile(value_adj, 0.975)) |>
  filter(grepl('sigma', coeff)) |>
  ggplot()+
  geom_histogram(aes(x = value_adj), fill = 'lightgrey',
                 col = 'black')+
  geom_vline(aes(xintercept = post_mean), col = 'blue', lwd = 1)+
  geom_vline(aes(xintercept = post_lower), col = 'red', lwd = 1, lty = 2)+
  geom_vline(aes(xintercept = post_upper), col = 'red', lwd = 1, lty = 2)+
  facet_wrap(facets = vars(coeff), scales = 'free_x')+
  theme_bw()


