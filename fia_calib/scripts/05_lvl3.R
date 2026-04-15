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
  mutate(plot_area_m2 = ifelse(DIAMETER > 5, pi*7.3152^2, pi*2.07624),
         plot_area_ha = plot_area_m2/10000,
         expansion_factor = 1/(plot_area_ha*4)) |>
  summarize(count = n(), .by = c('PID', 'expansion_factor')) |>
  mutate(TPH = count*expansion_factor) |>
  summarize(TPH = sum(TPH), .by = 'PID') |>
  # rescale to 1000 trees per hectare
  mutate(rescaled_TPH = (TPH-median(TPH))/1000)

stand_data <- right_join(deficit, stand_density, by = 'PID') |>
  filter(!is.na(rescaled_def), !is.nan(rescaled_def)) |>
  mutate(stan_plot_id = as.numeric(factor(PID))) |>
  arrange(stan_plot_id)


compare_growth_full <- compare_growth |>
  filter(PID %in% stand_data$PID) |> # not all stands have deficit data...
  left_join(stand_data[c('PID', 'stan_plot_id')])
# Prep data for stan -----------------------------------------------------------
mod_data <- list(N = nrow(compare_growth_full),
                 N_plots = length(unique(compare_growth_full$stan_plot_id)),
                 G_r = compare_growth_full$dg_FIA/compare_growth_full$dg_FVS,
                 initial_dbh = compare_growth_full$initial_dbh,
                 larch = ifelse(compare_growth_full$SPECIES == 73, 1, 0),
                 plot_id = compare_growth_full$stan_plot_id,
                 evap = stand_data$rescaled_def,
                 density = stand_data$rescaled_TPH)

# Compile and fit model --------------------------------------------------------
# Initialize Stan Model: translate to C++, compile C++ to DSO, then load.
mod <- stan_model(file = here('scripts', '05_lvl3.stan'))

# sample using HMC to approximate posterior
#> ran with no warnings!
fit <- sampling(mod, data = mod_data, chains = 4, iter = 3000)

# Examine fit ----------------------------------------------------------
check_hmc_diagnostics(fit) # no issues

# good mixing on everything!!!
traceplot(fit, pars = c('beta_size', 'beta_larch', 'sigma', 'sigma_plot',
                        'beta_evap', 'beta_density', 'beta_0'))

# hopefully adding coefficients and plot error as I did helped convergence
#> relative to previous models
samp <- sample(unique(compare_growth_full$stan_plot_id),
               12, replace = FALSE)
traceplot(fit, pars = paste0('alpha_plot[', samp, ']'))

alpha_plot_rhats <- summary(fit, pars = 'alpha_plot', use_cache = FALSE)$summary[,'Rhat']
alpha_plot_rhats <- sort(alpha_plot_rhats, decreasing = TRUE)
hist(alpha_plot_rhats)

fit_params <- as.data.frame(fit, pars = c('beta_size', 
                                          'beta_larch',
                                          'sigma', 
                                          'alpha_plot',
                                          'sigma_plot',
                                          'beta_evap',
                                          'beta_density',
                                          'beta_0'))
fit_diagnostics <- summary(fit, pars = c('beta_size', 
                                         'beta_larch',
                                         'sigma', 
                                         'alpha_plot',
                                         'sigma_plot',
                                         'beta_evap',
                                         'beta_density',
                                         'beta_0'))$summary

min(fit_diagnostics[,'n_eff'])
max(fit_diagnostics[,'Rhat'])
saveRDS(list(draws = fit_params, diagnostics = fit_diagnostics), 
        'model_outputs/lvl3_fit.RDS')

