# DESCRIPTION ==================================================================
#>
#> Purpose: Model remeasured diameter growth as a scale factor*fvs prediction,
#> plus some noise. The scale factor varies by plot and species, with a random plot
#> effect.
#> 
#> Outputs:
#> 
#> Notes:
#>
# ==============================================================================

library(here)
library(rstan)

# Prepare data -----------------------------------------------------------------
compare_growth <- readRDS(here('data', 'sim_outputs', 'uc_compare_growth.rds'))
compare_growth$stan_plot_id <- as.numeric(factor(compare_growth$PID))
compare_growth$larch <- ifelse(compare_growth$SPECIES == 73, 1, 0)

# Simulate data from model -----------------------------------------------------
sim_sigma <- abs(rnorm(1, 0, 1)) # not quite half-normal but
sim_beta_larch <- rnorm(1, 0, 0.5)
sim_sigma_plot <- 0.5
sim_alpha_plot <- rnorm(length(unique(compare_growth$stan_plot_id)), 
                        0, sim_sigma_plot)

sim_G_r <- rep(NA, nrow(compare_growth))

for(i in seq_len(nrow(compare_growth))){
  mu <- exp(sim_alpha_plot[compare_growth$stan_plot_id[i]]+sim_beta_larch*compare_growth$larch[i])
  sim_G_r[i] <- rgamma(1, shape = mu^2/sim_sigma^2, rate = mu/sim_sigma^2)
}

hist(sim_G_r)

sim_mod_data <- list(N = nrow(compare_growth),
                     N_plots = length(unique(compare_growth$PID)),
                     G_r = sim_G_r,
                     plot_id = compare_growth$stan_plot_id,
                     larch = compare_growth$larch)

mod <- stan_model(file = here('scripts', '04a_lvl2_plot_species.stan'))
fit <- sampling(mod, data = sim_mod_data, chains = 2, iter = 2000)

alphas <- as.matrix(fit, pars = 'alpha_plot')
alpha_means <- apply(alphas, 2, mean)
plot(sim_alpha_plot, alpha_means)
abline(0, 1, col = 'red') # looks good, but a few plots that are very off

mean(as.matrix(fit, pars = 'beta_larch')) # very close
mean(as.matrix(fit, pars = 'sigma')) # also very close

traceplot(fit, pars = c('beta_larch', 'sigma')) # good mixing
rm(alphas, fit, sim_mod_data)

# Run model --------------------------------------------------------------------
# stan expects data object to be named list
mod_data <- list(N = nrow(compare_growth),
                 N_plots = length(unique(compare_growth$PID)),
                 G_r = compare_growth$dg_FIA/compare_growth$dg_FVS,
                 plot_id = compare_growth$stan_plot_id,
                 larch = compare_growth$larch)

# sample using HMC to approximate posterior 
fit <- sampling(mod, data = mod_data, chains = 4, iter = 3000)

# ran without warnings
check_hmc_diagnostics(fit) # no divergent transitions or max tree depth

# Examine outputs --------------------------------------------------------------

traceplot(fit, pars = c('sigma', 'beta_larch')) # good mixing

beta_species <- as.matrix(fit, pars = 'beta_larch')
Rhat(beta_species) # 0.9998868

# what about size and sigma?=
Rhat(as.matrix(fit, pars = 'sigma')) # 0.99988, looks good
ess_bulk(as.matrix(fit, pars = 'beta_larch')) # 2320 > 400, OK
ess_bulk(as.matrix(fit, pars = 'sigma')) # 2578 > 400, looks good

# and a random sample of plots, just for traceplot
samp <- sample(unique(compare_growth$stan_plot_id),
               12, replace = FALSE)

traceplot(fit, pars = paste0('alpha_plot[', samp, ']')) # looks good
Rhat(as.matrix(fit, pars = 'alpha_plot')) # 1.989 -- high
idx <- which.max(summary(fit, pars = 'alpha_plot', use_cache = FALSE)$summary[,'Rhat'])
hist(as.matrix(fit, pars = 'alpha_plot')[,idx])
ess_bulk(as.matrix(fit, pars = 'alpha_plot')) # 1198 > 400, OK

# Save draws -------------------------------------------------------------------
fit_params <- extract(fit, pars = c('beta_larch', 'sigma', 'alpha_plot'))
fit_diagnostics <- data.frame(param = c('beta_larch', 'sigma', 'alpha_plot')) |>
  dplyr::mutate(Rhat = Rhat(as.matrix(fit, pars = param)),
                bulk_ESS = ess_bulk(as.matrix(fit, pars = param)),
                tail_ESS = ess_tail(as.matrix(fit, pars = param)))

saveRDS(list(draws = fit_params, diagnostics = fit_diagnostics), 
        'model_outputs/lvl2_plot_species_fit.RDS')

