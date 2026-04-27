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
  scale_y_reverse()
lvl1_sigma <- ggplot(lvl1, aes(y = 1/rate))+
  geom_histogram(col = 'black', fill = 'lightgrey')+
  scale_x_reverse()+
  labs(y = 'scale')
lvl1_joint <- ggplot(lvl1, aes(x = mu, y = 1/rate))+
  geom_bin_2d(bins = 40)+
  scale_fill_viridis_c()+
  labs(y = 'scale')

layout <- 'ABB
           ABB
           CDD'
lvl1_sigma+lvl1_joint+plot_spacer()+lvl1_mu+plot_layout(design = layout, 
                                                        axis_titles = 'collect')&
  theme_bw()

ggsave(here('figures', 'lvl1_joint.png'), width = 8, height = 6, units = 'in', dpi = 600)



## level 2 ----
lvl2_gg <- select(lvl2, beta_larch, rate, plot_mean, sigma_plot) |>
  mutate(scale = 1/rate)
# pars <- c('beta_larch', 'sigma', 'plot_mean', 'sigma_plot')

# beta_larch vs everything
p2.1 <- ggplot(lvl2_gg)+
  geom_bin_2d(aes(x = scale, y = exp(beta_larch)))+
  scale_fill_viridis_c()+
  labs(title = expr(paste(scale['tree'])),
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
  geom_bin_2d(aes(x = exp(plot_mean), y = scale))+
  scale_fill_viridis_c()+
  labs(y = expr(paste(scale['tree'])),
       x = '')
p2.5 <- ggplot(lvl2_gg)+
  geom_bin_2d(aes(x = exp(sigma_plot), y = scale))+
  labs(y = expr(paste(scale['tree'])),
       x = '')+
  scale_fill_viridis_c()

# sigma plot vs scale
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

ggsave(here('figures', 'lvl2_joint.png'), width = 8, height = 6, units = 'in', dpi = 600)


# Level 3 ----------------------------------------------------------------------
lvl3_gg <- select(lvl3, rate, sigma_plot, beta_evap, beta_density, beta_0, beta_larch) |>
  mutate(scale = 1/rate)
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
  geom_bin_2d(aes(x = scale, y = exp(beta_0)))+
  labs(title = expr(paste(scale['tree'])),
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
  geom_bin_2d(aes(x = scale, y = exp(beta_evap)))+
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
  geom_bin_2d(aes(x = scale, y = exp(beta_density)))+
  labs(x = '',
       y = expr(paste(beta['density'])))

# fourth row: beta larch vs everything
p3.13 <- ggplot(lvl3_gg)+
  geom_bin_2d(aes(x = exp(sigma_plot), y = exp(beta_larch)))+
  labs(x = '',
       y = expr(paste(beta['larch'])))
p3.14 <- ggplot(lvl3_gg)+
  geom_bin_2d(aes(x = scale , y = exp(beta_larch)))+
  labs(x = '',
       y = expr(paste(beta['larch'])))

# fifth row: sigma plot vs scale
p3.15 <- ggplot(lvl3_gg)+
  geom_bin_2d(aes(x = scale , y = exp(sigma_plot)))+
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

ggsave(here('figures', 'lvl3_joint.png'), width = 9, height = 6, units = 'in', dpi = 600)

# Level 3 Marginals ------------------------------------------------------------
lvl3_marginals <- lvl3_gg |>
  mutate(scale = 1/rate) |>
  tidyr::pivot_longer(cols = everything(), names_to = 'coeff') |>
  mutate(value_adj = ifelse(coeff == 'scale', value, exp(value))) |>
  group_by(coeff) |>
  mutate(post_mean = mean(value_adj),
         post_lower = quantile(value_adj, 0.025),
         post_upper = quantile(value_adj, 0.975)) 

filter(lvl3_marginals, grepl('beta', coeff)) |>
  ggplot()+
  geom_histogram(aes(x = value_adj), fill = 'lightgrey',
                 col = 'black')+
  geom_vline(aes(xintercept = post_mean), col = 'blue', lwd = 1)+
  geom_vline(aes(xintercept = post_lower), col = 'red', lwd = 1, lty = 2)+
  geom_vline(aes(xintercept = post_upper), col = 'red', lwd = 1, lty = 2)+
  facet_wrap(facets = vars(coeff), scales = 'free_x')+
  theme_bw()

ggsave(here('figures', 'lvl3_coeffs.png'), width = 8, height = 8, units = 'in', dpi = 600)

filter(lvl3_marginals, coeff == 'scale'|coeff == 'sigma_plot') |>
  ggplot()+
  geom_histogram(aes(x = value_adj), fill = 'lightgrey',
                 col = 'black')+
  geom_vline(aes(xintercept = post_mean), col = 'blue', lwd = 1)+
  geom_vline(aes(xintercept = post_lower), col = 'red', lwd = 1, lty = 2)+
  geom_vline(aes(xintercept = post_upper), col = 'red', lwd = 1, lty = 2)+
  facet_wrap(facets = vars(coeff), scales = 'free_x')+
  theme_bw()
ggsave(here('figures', 'lvl3_dispersion.png'), width = 8, height = 5, units = 'in', dpi = 600)

# Posterior Predictions --------------------------------------------------------
compare_growth <- readRDS(here('data', 'sim_outputs', 'uc_compare_growth.rds')) |>
  dplyr::filter(interval == '1') # for now, only plots that have been remeasured once

deficit <- readRDS(here('data', 'env_data', 'climatic_water_deficit.rds')) |>
  dplyr::filter(PID %in% compare_growth$PID) |>
  # rescale to "z-score" such that coefficient represents effect of 1 SD change from mean
  mutate(rescaled_def = (water_deficit-mean(water_deficit, na.rm = TRUE))/sd(water_deficit, na.rm = TRUE))

stands <- readRDS(here('data', 'fvs_ready', 'FVS_StandInit_MTID.rds')) |>
  dplyr::filter(PID %in% compare_growth$PID, REM_CD == 0)

stand_density <- readRDS(here('data', 'fvs_ready', 'FVS_TreeInit_MTID.rds')) |>
  inner_join(stands[c('STAND_CN')], 
             by = 'STAND_CN')|>
  filter(DIAMETER > 0.1, REM_CD == 0) |>
  # BA in ft^2
  mutate(BA = (pi*(DIAMETER/2)^2)/144) |>
  summarize(STAND_BA = sum(BA), .by = PID) |>
  mutate(centered_STAND_BA = STAND_BA-mean(STAND_BA))


stand_data <- right_join(deficit, stand_density, by = 'PID') |>
  filter(!is.na(rescaled_def), !is.nan(rescaled_def)) |>
  mutate(stan_plot_id = as.numeric(factor(PID))) |>
  arrange(stan_plot_id)


compare_growth_full <- compare_growth |>
  filter(PID %in% stand_data$PID) |> # not all stands have deficit data...
  left_join(stand_data)

compare_growth_full$larch <- ifelse(compare_growth_full$SPECIES == 73, 1, 0)

p1 <- ggplot(compare_growth, aes(x = 10*bag_FIA/growth_pd, y = 10*bag_FVS/growth_pd,
                                 col = initial_dbh))+
  geom_point(alpha = 0.5, size = 1)+
  geom_abline(slope = 1, intercept = 0, col =  'black', lty = 'dashed', lwd = 1)+
  labs(x = expr(paste('10-year Measured Basal Area Increment, ', 'ft'^2)),
       y = expr(paste('Projected 10-year BA increment, ', 'ft'^2)),
       title = 'Unscaled Predictions',
       color = 'Initial Diameter')+
  scale_color_viridis_c()+
  theme_bw()+
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1))

compare_growth_full$pred_sf <- exp(mean(lvl3$beta_0)+
                 mean(lvl3$beta_larch)*compare_growth_full$larch+
                 mean(lvl3$beta_evap)*compare_growth_full$rescaled_def+
                 mean(lvl3$beta_density)*compare_growth_full$centered_STAND_BA)

p2 <- ggplot(compare_growth_full, aes(x = 10*bag_FIA/growth_pd,
                                      y = pred_sf*10*bag_FVS/growth_pd,
                                      col = initial_dbh))+
  geom_point(alpha = 0.5, size = 1)+
  geom_abline(slope = 1, intercept = 0, col =  'black', lty = 'dashed', lwd = 1)+
  labs(x = expr(paste('10-year Measured Basal Area Increment, ', 'ft'^2)),
       y = expr(paste('Projected 10-year BA increment, ', 'ft'^2)),
       title = 'Scaled Predictions using density and deficit',
       color = 'Initial Diameter')+
  scale_color_viridis_c()+
  theme_bw()+
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1))

p1+p2+plot_layout(guides = 'collect', axis_titles = 'collect')

ggsave(here('figures', 'pred_comparison.png'), 
       width = 8, height = 5, units = 'in', dpi = 600)

p3 <- ggplot(compare_growth_full)+
  geom_histogram(aes(x = pred_sf, fill = factor(larch), group = factor(larch)),
                 alpha = 0.75)
p3+labs(fill = 'Larch?')

ggsave(here('figures', 'sf_distribution.png'), 
       width = 8, height = 8, units = 'in', dpi = 600)
