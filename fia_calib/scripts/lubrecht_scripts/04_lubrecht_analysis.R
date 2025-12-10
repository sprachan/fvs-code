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

# 2018 actual tree list
lub_treelist2018 <- readxl::read_xlsx(here('data', 'fvs_ready', 
                                           'FVS_Lubrecht_2018.xlsx'),
                                      sheet = 'FVS_TreeInit')

# Spatial information
## Fia plot locations
fia_locs <- readRDS(here('data', 'raw_data', 'lubrecht', 'fia_plots.RData')) |>
  terra::vect(geom = c('LONGITUDE', 'LATITUDE'),
              crs = '+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs +type=crs') 

## Study site outline
lub_outline <- terra::vect(here('data', 'raw_data', 'lubrecht', 'shape')) |>
  terra::project(fia_locs)

## Montana outline
mt_outline <- usmap::us_map(regions = 'county', include = 'Montana') |> # state outline 
  terra::vect() |>
  terra::project(fia_locs)

mt_cropped <- terra::crop(mt_outline, terra::ext(-114.75, -113, 46, 47.5))

## Basemaps
basemap_site <- basemaps::basemap_terra(ext = sf::st_bbox(mt_cropped),
                                    map_service = 'esri',
                                    map_type = 'natgeo_world_map') |>
  terra::project(fia_locs)

basemap_mt <- basemaps::basemap_terra(ext = sf::st_bbox(mt_outline),
                                      map_service = 'esri',
                                      map_type = 'natgeo_world_map') |>
  terra::project(fia_locs) |>
  terra::mask(mt_outline)
  
# Visualize FIA plots ----------------------------------------------------------
site_map <- basemaps::gg_raster(basemap_site)+
  tidyterra::geom_spatvector(data = lub_outline, fill = 'red')+
  tidyterra::geom_spatvector(data = fia_locs)+
  labs(x = 'Longitude',
       y = 'Latitude')+
  theme_minimal()

overview_map <- ggplot()+
  tidyterra::geom_spatvector(data = mt_outline)+
  tidyterra::geom_spatvector(data = fia_locs, size = 0.05)+
  theme(panel.background = element_rect(fill = 'white'),
        plot.background = element_rect(fill = NA,
                                       color = NA),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

composite_map <- site_map+inset_element(overview_map, left = 0, right = 0.5,
                       bottom = -0.1, top = 0.5)

# Comparison dataframes --------------------------------------------------------
# DBHs and heights
comp_df_full <- full_join(select(uncalib_tr, -cratio, -tpa, -mcuft, -bdft),
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
  filter(!is.na(dbh_meas)) |>
  left_join(select(lub_treelist2018, STAND_ID, TREE_ID, DIAMETER, HT),
            by = dplyr::join_by(STAND_ID, TREE_ID)) |>
  rename(dbh_init = DIAMETER,
         ht_init = HT)
  
comp_df <- dplyr::filter(comp_df_full, dbh_init >= 3)

lg <- split(comp_df, comp_df$dbh_class)$LG
sm <- split(comp_df_full, comp_df_full$dbh_class)$SM
  
# Multipliers
comp_mults <- dplyr::full_join(dplyr::select(fia_mults, TreeSize, SpeciesPLANTS, 
                                             n, avgScaleFactor, avgReadCorMult),
                               dplyr::select(self_mults, TreeSize, SpeciesPLANTS, 
                                             n, avgScaleFactor, avgReadCorMult),
                               by = dplyr::join_by(TreeSize, SpeciesPLANTS),
                               suffix = c('_fia', '_self')) 

# Visualize comparisons --------------------------------------------------------
comp_uc_plt <- ggplot(data = comp_df)+
  geom_point(aes(x = dbh_meas, y = dbh_uc), alpha = 0.25)+
  geom_abline(slope = 1, intercept = 0, col = 'red')+
  labs(x = 'Measured DBH (in)', y = 'Projected DBH (in)')

comp_sc_plt <- ggplot(data = comp_df)+
  geom_point(aes(x = dbh_meas, y = dbh_sc), alpha = 0.25)+
  geom_abline(slope = 1, intercept = 0, col = 'red')+
  labs(x = 'Measured DBH (in)', y = 'Projected DBH (in)')

comp_fia_plt <- ggplot(data = comp_df)+
  geom_point(aes(x = dbh_meas, y = dbh_fia), alpha = 0.25)+
  geom_abline(slope = 1, intercept = 0, col = 'red')+
  labs(x = 'Measured DBH (in)', y = 'Projected DBH (in)')

comp_plt <- comp_uc_plt/comp_sc_plt/comp_fia_plt+plot_layout(axes = 'collect')+
  plot_annotation(tag_levels = 'a', tag_suffix = ')', tag_prefix = '(')

# Visualize DBH error distributions --------------------------------------------
uc_err_plt <- ggplot(data = comp_df)+
  geom_histogram(aes(x = err_uc), bins = 20, col = 'black', fill = 'lightgrey')+
  geom_vline(xintercept = 0, col = 'red')+
  geom_vline(xintercept = mean(comp_df$err_uc, na.rm = T), linetype = 'dashed')+
  labs(x = 'DBH Prediction Error (in)', y = 'Count')

sc_err_plt <- ggplot(data = comp_df)+
  geom_histogram(aes(x = err_sc), bins = 20, col = 'black', fill = 'lightgrey')+
  geom_vline(xintercept = 0, col = 'red')+
  geom_vline(xintercept = mean(comp_df$err_sc, na.rm = T), linetype = 'dashed')+
  labs(x = 'DBH Prediction Error (in)', y = 'Count')

fia_err_plt <- ggplot(data = comp_df)+
  geom_histogram(aes(x = err_fia), bins = 20, col = 'black', fill = 'lightgrey')+
  geom_vline(xintercept = 0, col = 'red')+
  geom_vline(xintercept = mean(comp_df$err_fia, na.rm = T), linetype = 'dashed')+
  labs(x = 'DBH Prediction Error (in)', y = 'Count')

err_hist_plt <- uc_err_plt+sc_err_plt+fia_err_plt+
  plot_layout(axes = 'collect')+
  plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')')

# Visualize prediction error vs DBH --------------------------------------------
pe_uc_plt <- ggplot(data = comp_df)+
  geom_hline(yintercept = 0, col = 'black')+
  geom_point(aes(x = dbh_init, y = err_uc), alpha = 0.25)+
  geom_smooth(aes(x = dbh_init, y = err_uc), method = 'loess', se = FALSE,
              formula = 'y~x')+
  scale_y_continuous(limits = c(-5, 5))+
  labs(x = 'Initial DBH (in)', y = 'Prediction Error (in)')

pe_sc_plt <- ggplot(data = comp_df)+
  geom_hline(yintercept = 0, col = 'black')+
  geom_point(aes(x = dbh_init, y = err_sc), alpha = 0.25)+
  geom_smooth(aes(x = dbh_init, y = err_sc), method = 'loess', se = FALSE,
              formula = 'y~x')+
  scale_y_continuous(limits = c(-5, 5))+
  labs(x = 'Initial DBH (in)', y = 'Prediction Error (in)')

pe_fia_plt <- ggplot(data = comp_df)+
  geom_hline(yintercept = 0, col = 'black')+
  geom_point(aes(x = dbh_init, y = err_fia), alpha = 0.25)+
  geom_smooth(aes(x = dbh_init, y = err_fia), method = 'loess', se = FALSE,
              formula = 'y~x')+
  scale_y_continuous(limits = c(-5, 5))+
  labs(x = 'Initial DBH (in)', y = 'Prediction Error (in)')

pe_plt <- pe_uc_plt/pe_sc_plt/pe_fia_plt+plot_layout(axes = 'collect')+
  plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')')

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

