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
  filter(DAMAGE1 == 0|is.na(DAMAGE1),
         SPECIES %in% c(202, 73)) # only Doug fir and western larch

t0_stands <- standInit |>
  dplyr::filter(N_REM >= 1) |>
  dplyr::group_by(PID) |>
  # year of first remeasurement
  dplyr::mutate(REM_YEAR = ifelse(N_REM == 1, max(INV_YEAR), INV_YEAR[2])) |>
  dplyr::ungroup() |>
  # dplyr::filter(REM_CD == 0) |>
  as.data.frame()

# only want trees associated with selected stands
t0_trees <- treeInit |>
  inner_join(t0_stands['STAND_CN'], by = 'STAND_CN') |>
  filter(HISTORY == 1,
         DIAMETER >= 3) # alive at first remeasurement only, don't want to project dead trees

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

# project forward from the first measurement
t0_stands1 <- dplyr::filter(t0_stands, 
                            # need at least 1 remeasurement for comparison
                            N_REM >= 1,
                            REM_CD == 0)

# Run simulation for installation --> first remeasurement ----------------------

sim1 <- run_FVS_parallel(t0_stands1, t0_trees, n_batches = 4, simple_output = TRUE, 
                        out_dir = out_dir, fvs_bin = fvs_bin, 
                        year_col = 'REM_YEAR',
                        proj_len = num_years, triple = triple,
                        calibrate = calibrate, add_regen = regen,
                        STDIDENT = STDIDENT, random_seed = random_seed)

tl_fvs1 <- sim1$tree_list |>
  group_by(PID) |>
  mutate(REM_CD = case_when(year == min(year) ~ 0, year == max(year) ~ 1, 
                            .default = NA)) |>
  rename(DIAMETER = dbh,
         YEAR = year)

tl_fia1 <- treeInit |>
  dplyr::filter(TUID %in% tl_fvs1$TUID, REM_CD < 2) |>
  rename(YEAR = INV_YEAR)



# for plots remeasured twice, want to project forward from first REmeasurement
t0_stands2 <- dplyr::filter(t0_stands, N_REM == 2, REM_CD >= 1) |>
  group_by(PID) |>
  mutate(REM_YEAR = max(INV_YEAR)) |>
  # REM_CD == 1 means we are taking stands at their first (not second) REmeasurement
  dplyr::filter(REM_CD == 1)

# Run simulation for first remeasurement --> second remeasurement --------------
  
sim2 <- vector(mode = 'list', length = 17)
  
for(s in seq_along(t0_stands2$PID)){
  sim2[[s]] <- run_FVS(tree_list = t0_trees, stand_info = t0_stands2[s,], out_dir = out_dir,
                       fvs_bin = fvs_bin, CYCLEAT = t0_stands2$REM_YEAR[s],
                       proj_len = num_years, triple = triple,
                       calibrate = calibrate, add_regen = regen,
                       STDIDENT = STDIDENT, random_seed = random_seed)
}  

tl_fvs2 <- lapply(sim2, `[[`, 'tree_list') |>
  bind_rows() |>
  group_by(PID) |>
  mutate(REM_CD = case_when(year == min(year) ~ 1, year == max(year) ~ 2,
                            .default = NA)) |>
  rename(DIAMETER = dbh,
         YEAR = year)


tl_fia2 <- treeInit |>
  dplyr::filter(TUID %in% tl_fvs2$TUID, REM_CD > 0) |>
  rename(YEAR = INV_YEAR)


# Save outputs -----------------------------------------------------------------
compare_growth1 <- full_join(tl_fvs1[c('DIAMETER', 'YEAR', 'TUID', 'REM_CD', 'PID')], 
                             tl_fia1[c('DIAMETER', 'YEAR', 'TUID', 'REM_CD', 'PID', 'HISTORY', 'DAMAGE1', 'SPECIES')], 
                             by = c('TUID', 'REM_CD', 'PID', 'YEAR'),
                             suffix = c('_FVS', '_FIA')) |>
  # only want trees that have both FIA and FVS (re)measurements/projections, respectively
  dplyr::filter(!is.na(DIAMETER_FVS), !is.na(DIAMETER_FIA)) |>
  mutate(BA_FIA = (pi/144)*(DIAMETER_FIA/2)^2, # BA in ft^2
         BA_FVS = (pi/144)*(DIAMETER_FVS/2)^2) |>
  group_by(TUID, PID, SPECIES) |>
  dplyr::summarize(growth_pd = YEAR[2]-YEAR[1],
                   status_1 = HISTORY[1],
                   status_2 = HISTORY[2],
                   dg_FVS = DIAMETER_FVS[2]-DIAMETER_FVS[1],
                   dg_FIA = round(DIAMETER_FIA[2]-DIAMETER_FIA[1], 2),
                   bag_FVS = BA_FVS[2]-BA_FVS[1],
                   bag_FIA = BA_FIA[2]-BA_FIA[1],
                   initial_dbh = DIAMETER_FIA[1]) |>
  filter(dg_FIA >= 0) |>
  dplyr::ungroup()

compare_growth2 <- full_join(tl_fvs2[c('DIAMETER', 'YEAR', 'TUID', 'REM_CD', 'PID')], 
                             tl_fia2[c('DIAMETER', 'YEAR', 'TUID', 'PID', 'HISTORY', 'DAMAGE1', 'SPECIES')], 
                             by = c('TUID', 'PID', 'YEAR'),
                             suffix = c('_FVS', '_FIA')) |>
  # only want trees that have both FIA and FVS (re)measurements/projections, respectively
  dplyr::filter(!is.na(DIAMETER_FVS), !is.na(DIAMETER_FIA)) |>
  mutate(BA_FIA = (pi/144)*(DIAMETER_FIA/2)^2, # BA in ft^2
         BA_FVS = (pi/144)*(DIAMETER_FVS/2)^2) |>
  group_by(TUID, PID, SPECIES) |>
  dplyr::summarize(growth_pd = YEAR[2]-YEAR[1],
                   status_1 = HISTORY[1],
                   status_2 = HISTORY[2],
                   dg_FVS = DIAMETER_FVS[2]-DIAMETER_FVS[1],
                   dg_FIA = round(DIAMETER_FIA[2]-DIAMETER_FIA[1], 2),
                   bag_FVS = BA_FVS[2]-BA_FVS[1],
                   bag_FIA = BA_FIA[2]-BA_FIA[1],
                   initial_dbh = DIAMETER_FIA[1]) |>
  filter(dg_FIA >= 0) |>
  dplyr::ungroup()



tree_list <- bind_rows(sim1$tree_list, 
                       bind_rows(lapply(sim2, `[[`, 'tree_list')),
                       .id = 'interval')
saveRDS(tree_list, file = here('data', 'sim_outputs', 'uc_tree_list.rds'))

summaries <- bind_rows(sim1$summary,
                       bind_rows(lapply(sim2, `[[`, 'summary')),
                       .id = 'interval')
saveRDS(tree_list, file = here('data', 'sim_outputs', 'uc_summary.rds'))

compare_growth <- bind_rows(compare_growth1, compare_growth2, .id = 'interval')

saveRDS(compare_growth, file = here('data', 'sim_outputs', 'uc_compare_growth.rds'))
