library(dplyr)
library(ggplot2)
library(viridis)
library(patchwork)
library(here)

# turn variable names like sigma_plot and beta_evap into expressions
pretty_varname <- function(varname){
  split_var <- unlist(strsplit(varname, split = '_'))
  if(length(split_var) == 2){
    v <- sym(split_var[1])
    sub <- sym(split_var[2])
    rlang::call2("[", v, sub) 
  }else if(length(split_var == 1)){
    v <- sym(split_var)
    expr(!!v)
  }
}

plot_joint <- function(df, y, x, ...){
  # only non log-transformed predictor is scale
  if(deparse(substitute(x)) == 'scale'){
    p <- ggplot(df, aes(x = {{x}}, y = exp({{y}})))
  }else if(deparse(substitute(y)) == 'scale'){
    p <- ggplot(df, aes(x = exp({{x}}), y = {{y}}))
  }else{
    # everybody else gets exponentiated to get actual effect
    p <- ggplot(df, aes(x = exp({{x}}), y = exp({{y}})))
  }
  p+
    geom_bin_2d(...)+
    scale_fill_viridis(limits = c(0, 80))+
    labs(x = pretty_varname(deparse(substitute(x))),
         y = pretty_varname(deparse(substitute(y))))
}

# Read in data -----------------------------------------------------------------
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
  left_join(stand_data[c('PID', 'stan_plot_id')])

compare_growth_full$larch <- ifelse(compare_growth_full$SPECIES == 73, 1, 0)

# Read in model fit ------------------------------------------------------------
params <- readRDS(here('model_outputs', 'lvl3_evap_density_species_fit.RDS'))$draws
beta_ijs <- readRDS(here('model_outputs', 'lvl3_evap_density_species_fit.RDS'))$beta_ijs
y_rep <- readRDS(here('model_outputs', 'lvl3_evap_density_species_fit.RDS'))$post_pred
# Parameter identifiability: joint posterior distributions ---------------------
layout <- c('AABBCCDDEE
             AABBCCDDEE
             PPFFGGHHII
             PPFFGGHHII
             PPQQJJKKLL
             PPQQJJKKLL
             PPQQRRMMNN
             PPQQRRMMNN
             PPQQRRSSOO
             PPQQRRSSOO')
# first row
plot_joint(params, beta_0, beta_larch)+labs(title = pretty_varname('beta_larch'))+
  plot_joint(params, beta_0, beta_evap)+labs(title = pretty_varname('beta_evap'))+
  plot_joint(params, beta_0, beta_density)+labs(title = pretty_varname('beta_density'))+
  plot_joint(params, beta_0, scale)+labs(title = pretty_varname('scale'))+
  plot_joint(params, beta_0, sigma_plot)+labs(title = pretty_varname('sigma_plot'))+
# second row
  plot_joint(params, beta_larch, beta_evap)+
  plot_joint(params, beta_larch, beta_density)+
  plot_joint(params, beta_larch, scale)+
  plot_joint(params, beta_larch, sigma_plot)+
# third row
  plot_joint(params, beta_evap, beta_density)+
  plot_joint(params, beta_evap, scale)+
  plot_joint(params, beta_evap, sigma_plot)+
# fourth row
  plot_joint(params, beta_density, scale)+
  plot_joint(params, beta_density, sigma_plot)+
# final row
  plot_joint(params, scale, sigma_plot)+
  plot_layout(design = layout, guides = 'collect', axis_titles = 'collect')&
  theme_linedraw()&scale_x_continuous(n.breaks = 4)&
  theme(axis.title.y = element_text(size = 14))&
  scale_y_continuous(n.breaks = 4)

ggsave(here('figures', 'lvl3_joint.png'),
       dpi = 600, width = 12, height = 8, units = 'in')

# Posterior Predictive Checks --------------------------------------------------
# test statistics
ppc <- data.frame(mean = apply(y_rep, 1, mean),
                  min = apply(y_rep, 1, min),
                  max = apply(y_rep, 1, max),
                  median = apply(y_rep, 1, \(x) quantile(x, 0.5)))


ppc_means <- ppc |>
  tidyr::pivot_longer(cols = everything()) |>
  summarize(.by = name, mean = mean(value))

obs_vals <- summarize(compare_growth_full,
                                 mean = mean(bag_FIA/bag_FVS),
                                 min = min(bag_FIA/bag_FVS),
                                 median = quantile(bag_FIA/bag_FVS, 0.5),
                                 max = max(bag_FIA/bag_FVS)) 

ggplot(tidyr::pivot_longer(ppc, cols = everything()))+
  geom_histogram(aes(x = value), col = 'black', fill = 'grey')+
  geom_vline(data = tidyr::pivot_longer(obs_vals, cols = everything()), 
             aes(xintercept = value), col = 'cornflowerblue', lwd = 1)+
  facet_wrap(facets = vars(name), scales = 'free')+
  theme_bw()+
  theme(legend.position = 'bottom')+
  labs(x = expr(G[r]), y = 'Value')
ggsave(here('figures', 'post_pred_check.png'), width = 6, height = 4, units = 'in',
       dpi = 600)

# select 6 y_reps at random for visualization
samp <- sample(1:6000, 6)
y_rep_samp <- y_rep |>
  slice(samp)|>
  mutate(iteration = paste('Iteration', samp), .before = everything()) |>
  tidyr::pivot_longer(cols = -iteration) |>
  select(-name)


ggplot(y_rep_samp)+
  geom_density(aes(x = value, fill = 'Posterior Predictions', 
                   col = 'Posterior Predictions'), 
               alpha = 0.25,
               lwd = 0.25)+
  geom_density(data = compare_growth_full, 
               aes(x = bag_FIA/bag_FVS, fill = 'Data', col = 'Data'), 
               alpha = 0.25,
               lwd = 0.25)+
  facet_wrap(facets = vars(forcats::fct_rev(iteration)), nrow = 2)+
  labs(color = 'Source',
       fill = 'Source',
       x = expr(G[r]),
       y = 'Density')+
  theme_bw()+
  theme(legend.position = 'bottom')

ggsave(here('figures', 'post_preds.png'), width = 6, height = 4, units = 'in',
            dpi = 600)

# Predictions ------------------------------------------------------------------
mean_bij <- unname(apply(beta_ijs, 2, mean))

compare_growth_full <- mutate(compare_growth_full,
                              pred = mean_bij*bag_FVS,
                              species = forcats::fct_rev(factor(SPECIES, 
                                               labels = c('western larch', 
                                                          'Douglas-fir'))))
compare_growth_full |>
  mutate(bag_pred = pred) |>
  select(bag_FIA, bag_FVS, species, bag_pred) |>
  tidyr::pivot_longer(cols = c(bag_FVS, bag_pred), names_to = 'Method') |>
  mutate(Method = ifelse(Method == 'bag_FVS', 'Unscaled', 'Scaled'),
         Method = forcats::fct_rev(factor(Method))) |>
  ggplot(aes(x = bag_FIA, y = value))+
  geom_point(aes(col = species), alpha = 0.25,
             size = 0.75)+
  geom_abline(aes(slope = 1, intercept = 0), col = 'black', lwd = 1)+
  facet_grid(rows = vars(species), cols  = vars(Method))+
 geom_smooth(method ='lm', aes(col = species), 
              se = FALSE, col = '#444', lty = 'dashed', lwd = 0.5)+
  theme_bw()+
  theme(legend.position = 'none')+
  labs(x = 'FIA remeasured basal area increment (sq ft)',
       y = 'FVS projected basal area increment (sq ft)')

ggsave(here('figures', 'lvl3_scaled_preds.png'), dpi = 600,
       width = 6, height = 5, units = 'in')

# Marginal Posteriors ----------------------------------------------------------
coefs <- params |> select(scale, sigma_plot, beta_evap, beta_density, beta_0, 
                          beta_larch) |>
  tidyr::pivot_longer(cols = everything()) |>
  filter(!(name %in% c('sigma_plot', 'scale'))) 

coefs |>
  summarize(mean = mean(exp(value)), .by = name,
            lower = quantile(exp(value), 0.025),
            upper = quantile(exp(value), 0.975)) |>
  ggplot()+
  geom_rect(aes(ymin = 0, ymax = 750, xmin = lower, xmax = upper), fill = 'cornflowerblue',
            alpha = 0.5)+
  geom_histogram(data = coefs, aes(x = exp(value)), col = 'black', fill = 'grey')+
  geom_vline(aes(xintercept = mean), col = 'blue', lwd = 1)+
  geom_rect(aes(ymin = 0, ymax = 750, xmin = lower, xmax = upper), fill = 'cornflowerblue',
            alpha = 0.2)+
  facet_wrap(facets = vars(name), scales = 'free_x')+
  scale_y_continuous(limits = c(0, 750))+
  theme_bw()

ggsave(here('figures', 'lvl3_coefs.png'), dpi = 600, width = 6, height = 4, 
       units = 'in')

dispersions <- params |> select(scale, sigma_plot) |>
  tidyr::pivot_longer(cols = everything())

dispersions |>
  summarize(mean = mean(value),
            lower = quantile(value, 0.025),
            upper = quantile(value, 0.975),
            .by = name) |>
  mutate(mean_adj = ifelse(name == 'scale', mean, exp(mean)),
         lower_adj = ifelse(name == 'scale', lower, exp(lower)),
         upper_adj = ifelse(name == 'scale', upper, exp(upper))) |>
  ggplot()+
  geom_rect(aes(ymin = 0, ymax = 750, xmin = lower_adj, xmax = upper_adj), 
            fill = 'cornflowerblue',
            alpha = 0.5)+
  geom_histogram(data = dispersions, 
                 aes(x = ifelse(name == 'scale', value, exp(value))), 
                 col = 'black', fill = 'grey')+
  geom_rect(aes(ymin = 0, ymax = 750, xmin = lower_adj, xmax = upper_adj), 
            fill = 'cornflowerblue',
            alpha = 0.2)+
  geom_vline(aes(xintercept = mean_adj), col = 'blue', lwd = 1)+
  facet_wrap(facets = vars(name), scales = 'free_x')+
  theme_bw()+
  labs(x = 'Estimate', y = 'Count')

ggsave(here('figures', 'lvl3_vars.png'), dpi = 600, width = 6, height = 2, 
       units = 'in')
