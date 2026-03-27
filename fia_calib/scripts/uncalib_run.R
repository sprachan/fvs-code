# DESCRIPTION ==================================================================
#>
#> Purpose: Run FVS on all eligible FIA plots, with calibration turned off.
#> This gives us a "baseline" for assessing how well calibration works and what
#> general patterns in calibration are. Also gives us the model input for BHM.
#> 
#> Outputs: Two .rds objects. uc_trees_MTID.rds contains treelists for every single
#> stand projected. uc_summary_MTID.rds contains FVS summaries for each stand.
#> 
#> Notes: Write function for adding REM_CD and N_REM for package?
#>
# ==============================================================================`
library(here)
library(rFVSIEtools)
library(dplyr)

fvs_ready <- here('data', 'fvs_ready')
fvs_runs <- here('data', 'fvs_runFiles')
fvs_dir <- file.path('C:', 'FVS', 'FVSSoftware')
fvs_bin <- file.path(fvs_dir, 'FVSbin')

# Read in stand and tree data
#* Note: Write function for REM_CD and N_REM for package?

standInit <- readRDS(here('data', 'fvs_ready', 'FVS_StandInit_MTID.rds')) |>
  dplyr::filter(VARIANT == 'IE')

treeInit <- readRDS(here('data', 'fvs_ready', 'FVS_TreeInit_MTID.rds')) |>
  inner_join(standInit[c('STAND_CN', 'INV_YEAR', 'REM_CD', 'N_REM')], 
             by = 'STAND_CN') |>
  filter(HISTORY == 1, # only live trees
         SPECIES %in% c(202, 73)) # only Doug fir and western larch

t0_stands <- standInit |>
  dplyr::filter(N_REM >= 1) |>
  dplyr::group_by(PID) |>
  dplyr::mutate(REM_YEAR = ifelse(N_REM == 1, max(INV_YEAR), INV_YEAR[2])) |>
  dplyr::ungroup() |>
  dplyr::filter(REM_CD == 0) |>
  as.data.frame()

# only want trees associated with selected stands
t0_trees <- treeInit |>
  inner_join(t0_stands['STAND_CN'], by = 'STAND_CN')

# Projection parameters
num_years <- 11
triple <- FALSE
calibrate <- FALSE
regen <- FALSE
STDIDENT <- 'FIA_UNCALIB'
random_seed <- 2025

# Directory for .key, .tre, and .out files
dt <- paste0('fia_uc_', format(Sys.Date(), '%b%d%Y'), '_', 
             format(Sys.time(), '%H%M'), collapse = '')
out_dir <- file.path(fvs_runs, dt)
dir.create(out_dir)


future::plan('multisession', workers = 5)


sim <- run_FVS_parallel(t0_stands, t0_trees, n_batches = 4, simple_output = TRUE, 
                        out_dir = out_dir, fvs_bin = fvs_bin, 
                        year_col = 'REM_YEAR',
                        proj_len = num_years, triple = triple,
                        calibrate = calibrate, add_regen = regen,
                        STDIDENT = STDIDENT, random_seed = random_seed)

saveRDS(sim$tree_list, file = here('data', 'sim_outputs', 'uc_trees_MTID.rds'))
saveRDS(sim$summary, file = here('data', 'sim_outputs', 'uc_summary_MTID.rds'))
