# DESCRIPTION ==================================================================
#>
#> Purpose: Model remeasured diameter growth as a scale factor*fvs prediction,
#> plus some noise. Specifically, assume FIA diameter growth is gamma-distributed
#> with mean scale factor*fvs prediction and some standard deviation. The goal
#> is to sample the posterior distribution of the scale factor.
#> 
#> Outputs:
#> 
#> Notes:
#>
# ==============================================================================

library(here)
library(rstan)

# Prepare data -----------------------------------------------------------------
compare_growth <- readRDS(here('data', 'sim_outputs', 'uc_compare_growth.rds')) |>
  dplyr::filter(initial_dbh >= 3)

# Run model --------------------------------------------------------------------
# Initialize Stan Model: translate to C++, compile C++ to DSO, then load.
mod <- stan_model(file = here('scripts', '03_first_level_model.stan'))

# stan expects data object to be named list with N (sample size), dg_fvs (vector),
#> and dg_fia (vector)
stan_data <- list(N = nrow(compare_growth),
                  dg_fvs = compare_growth$dg_FVS,
                  dg_fia = compare_growth$dg_FIA)

# sample using HMC to approximate posterior
fit <- sampling(mod, data = stan_data, chains = 4, iter = 2000)

# Get outputs ------------------------------------------------------------------
# only care about beta and sigma
fit_params <- as.data.frame(extract(fit, pars = c('beta', 'sigma')))
fit_summary <- summary(fit, pars = c('beta', 'sigma'))$summary

saveRDS(list(draws = fit_params, summary = fit_summary), 
        'model_outputs/first_level_fit.RDS')
