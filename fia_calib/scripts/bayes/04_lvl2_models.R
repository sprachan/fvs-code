# DESCRIPTION ==================================================================
#>
#> Purpose: Model remeasured diameter growth as a scale factor*fvs prediction,
#> plus some noise. The scale factor varies by plot and by tree; it is drawn
#> from a distribution that whose mean is a linear combination of (size) and species, 
#> with random plot effect. 
#> 
#> Outputs: List of 2 dataframes; first is MCMC draws of parameters of interest,
#> second is summary information (includes n_eff and Rhat)
#> 
#> Notes: 
#>
# ==============================================================================
library(here)
library(rstan)
library(ggplot2)

# Prepare data -----------------------------------------------------------------
compare_growth <- readRDS(here('data', 'sim_outputs', 'uc_compare_growth.rds'))
compare_growth$stan_plot_id <- as.numeric(factor(compare_growth$PID))
compare_growth$larch <- ifelse(compare_growth$SPECIES == 73, 1, 0)

mod_data <- list(N = nrow(compare_growth),
                 N_plots = length(unique(compare_growth$PID)),
                 G_r = compare_growth$bag_FIA/compare_growth$bag_FVS,
                 # center initial DBH
                 initial_dbh = compare_growth$initial_dbh-mean(compare_growth$initial_dbh),
                 plot_id = compare_growth$stan_plot_id,
                 larch = compare_growth$larch)

# Compile and fit models -------------------------------------------------------
lvl2a <- stan_model(file = here('scripts', '04a_lvl2_plot_species.stan'))
lvl2b <- stan_model(file = here('scripts', '04b_lvl2_plot_species_size.stan'))

lvl2a_fit <- sampling(lvl2a, data = mod_data, chains = 4, iter = 3000)

lvl2b_fit <- sampling(lvl2b, data = mod_data, chain = 4, iter = 3000)

# Get draws and diagnostics ----------------------------------------------------
check_hmc_diagnostics(lvl2a_fit) # looks great
check_hmc_diagnostics(lvl2b_fit) # no issues

# and a random sample of plots, just for traceplot
samp <- sample(unique(compare_growth$stan_plot_id),
               12, replace = FALSE)
traceplot(lvl2a_fit, pars = c('beta_larch', 'plot_mean', 'sigma_plot', 'rate'))
traceplot(lvl2b_fit, pars = c('beta_larch', 'beta_size', 'plot_mean', 
                              'sigma_plot', 'rate'))

traceplot(lvl2a_fit, pars = paste0('alpha_plot[', samp, ']'))
traceplot(lvl2b_fit, pars = paste0('alpha_plot[', samp, ']'))


alpha_plot_rhats <- summary(lvl2a_fit, pars = 'alpha_plot')$summary[,'Rhat']
hist(alpha_plot_rhats) # all < 1.01

alpha_plot_rhats <- summary(lvl2b_fit, pars = 'alpha_plot')$summary[,'Rhat']
hist(alpha_plot_rhats) # all < 1.01

# Save outputs -----------------------------------------------------------------
fit2a_params <- as.data.frame(lvl2a_fit, 
                              pars = c('beta_larch', 'rate', 'plot_mean',
                                       'sigma_plot', 'alpha_plot'))
fit2a_diagnostics <- as.data.frame(summary(lvl2a_fit, probs = c(0.025, 0.1, 0.5, 0.9, 0.975))$summary)

fit2b_params <- as.data.frame(lvl2b_fit, 
                              pars = c('beta_larch', 'beta_size', 'rate', 'plot_mean',
                                       'sigma_plot', 'alpha_plot'))
fit2b_diagnostics <- as.data.frame(summary(lvl2b_fit, probs = c(0.025, 0.1, 0.5, 0.9, 0.975))$summary)

saveRDS(list(draws = fit2a_params, diagnostics = fit2a_diagnostics),
        'model_outputs/lvl2_plot_species_fit.RDS')

saveRDS(list(draws = fit2b_params, diagnostics = fit2b_diagnostics),
        'model_outputs/lvl2_plot_species_size_fit.RDS')
