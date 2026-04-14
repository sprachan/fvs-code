# DESCRIPTION ==================================================================
#>
#> Purpose: Model remeasured diameter growth as a scale factor*fvs prediction,
#> plus some noise. The scale factor varies by plot and by tree; it is drawn
#> from a distribution that whose mean is a linear combination of size and species, 
#> with random plot effect. This is the same as the model in 4a, but we
#> have added the size term.
#> 
#> Outputs:
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
# Run model --------------------------------------------------------------------
# Initialize Stan Model: translate to C++, compile C++ to DSO, then load.
mod <- stan_model(file = here('scripts', '04b_lvl2_plot_species_size.stan'))

# stan expects data object to be named list
mod_data <- list(N = nrow(compare_growth),
                  N_plots = length(unique(compare_growth$PID)),
                  G_r = compare_growth$dg_FIA/compare_growth$dg_FVS,
                  initial_dbh = compare_growth$initial_dbh,
                  plot_id = compare_growth$stan_plot_id,
                  larch = compare_growth$larch)

# sample using HMC to approximate posterior
fit <- sampling(mod, data = stan_data, chains = 4, iter = 3000)

check_hmc_diagnostics(fit) # no divergent transitions or max tree depth

# Examine outputs --------------------------------------------------------------
# all of these mixed
traceplot(fit, pars = c('beta_size', 'sigma', 'beta_larch'))

# and a random sample of plots, just for traceplot
samp <- sample(unique(compare_growth$stan_plot_id),
               12, replace = FALSE)

traceplot(fit, pars = paste0('alpha_plot[', samp, ']'))
alpha_plot_rhats <- summary(fit, pars = 'alpha_plot', use_cache = FALSE)$summary[,'Rhat']

idx <- which(alpha_plot_rhats>1.0025)
max(alpha_plot_rhats) # 1.017, not horrible

for(i in idx){
  ap <- data.frame(est = unname(as.matrix(fit, pars = paste0('alpha_plot[', i, ']'))),
              # chain
              chain = rep(seq(1, 4, by = 1), each = 1500))
  p <- ggplot(ap)+
    geom_histogram(aes(x = est, fill = factor(chain)), col = 'black',
                   bins = 20)
  print(p)
}

# alpha plot 19 is bimodal, which is not great.
#> alpha plot 884 just has a long tail

View(compare_growth[compare_growth$stan_plot_id == 19,]) # plot 19 only has 2 trees!

traceplot(fit, pars = paste0('alpha_plot[', idx, ']'))
ess_bulk(as.matrix(fit, pars = 'alpha_plot')) # 1198 > 400, OK

# Save draws -------------------------------------------------------------------
fit_params <- rstan::extract(fit, pars = c('beta_size', 'sigma', 'beta_species',
                                    'alpha_plot'))
fit_diagnostics <- data.frame(param = c('beta_size', 'sigma', 'beta_species', 
                                        'alpha_plot')) |>
  dplyr::mutate(Rhat = Rhat(as.matrix(fit, pars = param)),
                bulk_ESS = ess_bulk(as.matrix(fit, pars = param)),
                tail_ESS = ess_tail(as.matrix(fit, pars = param)))

saveRDS(list(draws = fit_params, diagnostics = fit_diagnostics), 
        'model_outputs/lvl2_plot_species_size_fit.RDS')
