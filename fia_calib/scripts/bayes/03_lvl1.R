# DESCRIPTION ==================================================================
#>
#> Purpose: Model remeasured diameter growth as a scale factor*fvs prediction,
#> plus some noise. Specifically, assume FIA diameter growth is gamma-distributed
#> with mean scale factor*fvs prediction and some standard deviation. The goal
#> is to sample the posterior distribution of the scale factor.
#> 
#> Outputs: 
#> Dataframe containing draws from the posterior for beta and sigma 
#> for later analysis.
#> 
#> Notes:
#>
# ==============================================================================

library(here)
library(rstan)

# Prepare data -----------------------------------------------------------------
compare_growth <- readRDS(here('data', 'sim_outputs', 'uc_compare_growth.rds'))

# Run model --------------------------------------------------------------------
# Initialize Stan Model: translate to C++, compile C++ to DSO, then load.
mod <- stan_model(file = here('scripts', '03_lvl1.stan'))

# stan expects data object to be named list with N (sample size), dg_fvs (vector),
#> and dg_fia (vector)
mod_data <- list(N = nrow(compare_growth),
                 G_r = compare_growth$bag_FIA/compare_growth$bag_FVS)

# sample using HMC to approximate posterior
fit <- sampling(mod, data = mod_data, chains = 4, iter = 3000)

# Examine outputs --------------------------------------------------------------
traceplot(fit, pars = c('mu', 'rate')) # looks good!
Rhat(as.matrix(fit, pars = 'mu')) # < 1.01
Rhat(as.matrix(fit, pars = 'rate')) # < 1.01
bayesplot::mcmc_pairs(fit, pars = c('mu', 'rate', 'shape'))
# Save draws -------------------------------------------------------------------
# only care about beta and sigma
fit_params <- as.data.frame(fit, pars = c('mu', 'rate', 'shape'))

fit_diagnostics <- as.data.frame(summary(fit, pars = c('mu', 'rate', 'shape'))$summary)

saveRDS(list(draws = fit_params, diagnostics = fit_diagnostics), 
        'model_outputs/lvl1_fit.RDS')
