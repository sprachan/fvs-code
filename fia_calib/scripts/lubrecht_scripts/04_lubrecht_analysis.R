# DESCRIPTION ------------------------------------------------------------------
#>
#> Use equivalence testing and NHST approaches to compare efficacy of various 
#> FVS calibration methods.
#> 
#-------------------------------------------------------------------------------

# Packages and data ------------------------------------------------------------
library(here)
library(dplyr)
library(ggplot2)
theme_set(theme_bw())
library(patchwork)
sim_out <- here('data', 'sim_outputs', 'lubrecht')

# Read in data -----------------------------------------------------------------
# Tree lists
uncalib_tr <- readRDS(file.path(sim_out, 'lub_uncalib_trees.RData')) |>
  dplyr::arrange(STAND_ID, TREE_ID, year) |>
  dplyr::filter(year == 2023)
selfcalib_tr <- readRDS(file.path(sim_out, 'lub_selfcalib_trees.RData')) |>
  dplyr::arrange(STAND_ID, TREE_ID, year) |>
  dplyr::filter(year == 2023)
fiacalib_tr <- readRDS(file.path(sim_out, 'lub_fiacalib_trees.RData')) |>
  dplyr::arrange(STAND_ID, TREE_ID, year) |>
  dplyr::filter(year == 2023)

# Multipliers
fia_mults <- readRDS(file.path(sim_out, 'fia_multipliers.RData'))
self_mults <- readRDS(file.path(sim_out, 'self_multipliers.RData'))

# 2023 actual tree list
lub_treelist2023 <- readxl::read_xlsx(here('data', 'fvs_ready', 
                                       'FVS_Lubrecht_2023.xlsx'),
                                  sheet = 'FVS_TreeInit') 
# Comparison dataframes --------------------------------------------------------
# DBHs and heights
comp_df <- full_join(select(uncalib_tr, -cratio, -tpa, -mcuft, -bdft),
                     select(selfcalib_tr, -cratio, -tpa, -mcuft, -bdft),
                     by = dplyr::join_by(id, plot, age, species, TREE_ID, 
                                         STAND_ID, FIA_SPECIES, year),
                     suffix = c('_uc', '_sc')) |>
  full_join(select(fiacalib_tr, -cratio, -tpa, -mcuft, -bdft),
            by = dplyr::join_by(id, plot, age, species, TREE_ID,
                                STAND_ID, FIA_SPECIES, year)) |>
  rename(dbh_fia = dbh,
         ht_fia = ht) |>
  full_join(select(lub_treelist2023, STAND_ID, TREE_ID, DIAMETER, HT)) |>
  rename(dbh_meas = DIAMETER,
         ht_meas = HT) |>
  mutate(err_uc = dbh_uc-dbh_meas,
         err_sc = dbh_sc-dbh_meas,
         err_fia = dbh_fia-dbh_meas,
         dbh_class = factor(ifelse(dbh_meas >= 3, 'LG', 'SM'))) |>
  filter(!is.na(dbh_meas))

lg <- split(comp_df, comp_df$dbh_class)$LG
sm <- split(comp_df, comp_df$dbh_class)$SM
  
# Multipliers
comp_mults <- dplyr::full_join(dplyr::select(fia_mults, TreeSize, SpeciesPLANTS, 
                                             n, avgScaleFactor, avgReadCorMult),
                               dplyr::select(self_mults, TreeSize, SpeciesPLANTS, 
                                             n, avgScaleFactor, avgReadCorMult),
                               by = dplyr::join_by(TreeSize, SpeciesPLANTS),
                               suffix = c('_fia', '_self')) 

# Visualize comparisons --------------------------------------------------------
comp_uc_plt <- ggplot(data = comp_df)+
  geom_point(aes(x = dbh_meas, y = dbh_uc), alpha = 0.5)+
  geom_abline(slope = 1, intercept = 0, col = 'red')+
  labs(x = 'Measured DBH (in)', y = 'Projected DBH (in)', title = 'Uncalibrated')

comp_sc_plt <- ggplot(data = comp_df)+
  geom_point(aes(x = dbh_meas, y = dbh_sc), alpha = 0.5)+
  geom_abline(slope = 1, intercept = 0, col = 'red')+
  labs(x = 'Measured DBH (in)', y = 'Projected DBH (in)', title = 'Self-calibrated')

comp_fia_plt <- ggplot(data = comp_df)+
  geom_point(aes(x = dbh_meas, y = dbh_fia), alpha = 0.5)+
  geom_abline(slope = 1, intercept = 0, col = 'red')+
  labs(x = 'Measured DBH (in)', y = 'Projected DBH (in)', title = 'FIA calibrated')

comp_plt <- comp_uc_plt+comp_sc_plt+comp_fia_plt+plot_layout(axes = 'collect')

# Visualize DBH error distributions --------------------------------------------
uc_err_plt <- ggplot(data = comp_df)+
  geom_histogram(aes(x = err_uc), bins = 20, col = 'black', fill = 'lightgrey')+
  geom_vline(xintercept = 0, col = 'red')+
  labs(x = 'DBH Prediction Error (inches)', title = 'Uncalibrated', y = 'Count')

sc_err_plt <- ggplot(data = comp_df)+
  geom_histogram(aes(x = err_sc), bins = 20, col = 'black', fill = 'lightgrey')+
  geom_vline(xintercept = 0, col = 'red')+
  labs(x = 'DBH Prediction Error (inches)', title = 'Self-calibrated', y = 'Count')

fia_err_plt <- ggplot(data = comp_df)+
  geom_histogram(aes(x = err_fia), bins = 20, col = 'black', fill = 'lightgrey')+
  geom_vline(xintercept = 0, col = 'red')+
  labs(x = 'DBH Prediction Error (inches)', title = 'FIA calibrated', y = 'Count')

err_hist_plt <- uc_err_plt+sc_err_plt+fia_err_plt+plot_layout(axes = 'collect')


# Run equivalence tests --------------------------------------------------------
# Uncalibrated
lg_uc_tost <- equivalence::tost(lg$dbh_uc,
                                lg$dbh_meas,
                                paired = T,
                                epsilon = 0.2) # p = 1, CI = 0.69, 0.72

sm_uc_tost <- equivalence::tost(sm$dbh_uc,
                                sm$dbh_meas,
                                paired = TRUE,
                                epsilon = 0.1) # p = 1, CI = 0.92, 1.06

# Self calibrated
lg_sc_tost <- equivalence::tost(lg$dbh_sc,
                                lg$dbh_meas,
                                paired = T,
                                epsilon = 0.2) # p = 4.27e=74, CI = 0.07, 0.09

sm_sc_tost <- equivalence::tost(sm$dbh_sc,
                                sm$dbh_meas,
                                paired = TRUE,
                                epsilon = 0.1) # p = 1, CI = 0.86, 1.00

# FIA calibrated
lg_fia_tost <- equivalence::tost(lg$dbh_fia,
                                 lg$dbh_meas,
                                 paired = T,
                                 epsilon = 0.2) # p = 6.9e-91, CI = 0.07, 0.09

sm_fia_tost <- equivalence::tost(sm$dbh_fia,
                                 sm$dbh_meas,
                                 paired = TRUE,
                                 epsilon = 0.1) # p = 1.00, CI = 0.20, 0.36

# Run t-tests ------------------------------------------------------------------
# are means diff from 0?
lg_uc_t <- t.test(lg$err_uc) # p < 2.2e-16, CI = 0.69, 0.7
sm_uc_t <- t.test(sm$err_uc) # p < 2.2e-16, CI = 0.91, 1.1

lg_sc_t <- t.test(lg$err_sc) # p < 2.2e-16, CI = 0.07, 0.10
sm_sc_t <- t.test(sm$err_sc) # p < 2.2e-16, CI = 0.85, 1.01

lg_fia_t <- t.test(lg$err_fia) # p < 2.2e-16, CI = 0.07, 0.09
sm_fia_t <- t.test(sm$err_fia) # p = 4.4e-8, CI = 0.18, 0.37

# are means different from each other?
lg_uc_sc_t <- t.test(lg$err_uc, lg$err_sc) # p < 2.2e-16, CI = 0.60, 0.64
lg_uc_fia_t <- t.test(lg$err_uc, lg$err_fia) # p < 2.2e-16, CI = 0.60, 0.64
lg_sc_fia_t <- t.test(lg$err_sc, lg$err_fia) # p = 0.95, CI = -0.02, 0.02

sm_uc_sc_t <- t.test(sm$err_uc, sm$err_sc) # p = 0.23, CI = -0.05, 0.18
sm_uc_fia_t <- t.test(sm$err_uc, sm$err_fia) # p < 2.2e-16, CI = 0.59, 0.85
sm_sc_fia_t <- t.test(sm$err_sc, sm$err_fia) # p < 2.2e-16, CI = 0.52, 0.78

