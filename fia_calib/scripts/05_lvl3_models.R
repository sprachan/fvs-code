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
library(rstan)
library(dplyr)

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

# Prep data for stan -----------------------------------------------------------
mod_data <- list(N = nrow(compare_growth_full),
                 N_plots = length(unique(compare_growth_full$stan_plot_id)),
                 G_r = compare_growth_full$bag_FIA/compare_growth_full$bag_FVS,
                 larch = compare_growth_full$larch,
                 plot_id = compare_growth_full$stan_plot_id,
                 evap = stand_data$rescaled_def,
                 density = stand_data$centered_STAND_BA)

# Compile and fit model --------------------------------------------------------
# Initialize Stan Model: translate to C++, compile C++ to DSO, then load.
mod <- stan_model(file = here('scripts', '05_lvl3_evap_density_species.stan'))

# sample using HMC to approximate posterior
#> ran with no warnings!
fit <- sampling(mod, data = mod_data, chains = 4, iter = 3000)

# Examine fit ----------------------------------------------------------
check_hmc_diagnostics(fit) # no issues

# good mixing
traceplot(fit, pars = c('scale', 'sigma_plot',
                        'beta_larch',
                        'beta_evap', 'beta_density', 'beta_0'))

samp <- sample(unique(compare_growth_full$stan_plot_id),
               12, replace = FALSE)
traceplot(fit, pars = paste0('alpha_plot[', samp, ']'))

alpha_plot_rhats <- summary(fit, pars = 'alpha_plot')$summary[,'Rhat']
hist(alpha_plot_rhats) # all < 1.01

fit_params <- as.data.frame(fit, pars = c('scale', 
                                          'sigma_plot',
                                          'beta_evap',
                                          'beta_density',
                                          'beta_0',
                                          'beta_larch',
                                          'alpha_plot'))
beta_ijs <- as.data.frame(fit, pars = 'mu')

post_pred <- as.data.frame(fit, pars = 'y_rep')
fit_diagnostics <- as.data.frame(summary(fit, pars = c('scale', 
                                         'sigma_plot',
                                         'beta_evap',
                                         'beta_density',
                                         'beta_0',
                                         'beta_larch',
                                         'alpha_plot'))$summary)


min(fit_diagnostics[,'n_eff']) # > 1200
max(fit_diagnostics[,'Rhat']) # < 1.01
saveRDS(list(draws = fit_params, diagnostics = fit_diagnostics, beta_ijs = beta_ijs,
             post_pred = post_pred), 
        'model_outputs/lvl3_evap_density_species_fit.RDS')

