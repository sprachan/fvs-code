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
tree_list <- readRDS(here('data', 'sim_outputs', 'uc_trees_MTID.rds')) 

tl_fvs <- tree_list |>
  dplyr::group_by(PID) |>
  dplyr::mutate(REM_CD = dplyr::case_when(year == min(year) ~ 0,
                                          year == max(year) ~ 1,
                                          .default = NA)) |>
  dplyr::ungroup()

standInit <- readRDS(here('data', 'fvs_ready', 'FVS_StandInit_MTID.rds')) |>
  dplyr::filter(VARIANT == 'IE')
tl_fia <- readRDS(here('data', 'fvs_ready', 'FVS_TreeInit_MTID.rds')) |>
  dplyr::left_join(standInit[c('STAND_CN', 'INV_YEAR', 'REM_CD', 'N_REM')],
            by = 'STAND_CN') |>
  dplyr::filter(TUID %in% tl_fvs$TUID)



# for name matching
tl_fvs$DIAMETER <- tl_fvs$dbh
tl_fvs$YEAR <- tl_fvs$year
tl_fia$YEAR <- tl_fia$INV_YEAR

compare_wide <- dplyr::full_join(tl_fvs[c('DIAMETER', 'YEAR', 'TUID', 'REM_CD', 'PID')], 
                                 tl_fia[c('DIAMETER', 'YEAR', 'TUID', 'REM_CD', 'PID')], 
                                 by = c('TUID', 'REM_CD', 'PID'),
                                 suffix = c('_FVS', '_FIA'))
compare_growth <- compare_wide |>
  dplyr::group_by(TUID, PID) |>
  dplyr::summarize(growth_pd_FVS = YEAR_FVS[2]-YEAR_FVS[1],
                   growth_pd_FIA = YEAR_FIA[2]-YEAR_FIA[1],
                   dg_FVS = DIAMETER_FVS[2]-DIAMETER_FVS[1],
                   dg_FIA = round(DIAMETER_FIA[2]-DIAMETER_FIA[1], 2)) |>
  dplyr::filter(dg_FIA >= 0) |>
  dplyr::ungroup()

# Run model --------------------------------------------------------------------
# Initialize Stan Model: translate to C++, compile C++ to DSO, then load.
mod <- stan_model(file = here('scripts', 'first_level_model.stan'))

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
