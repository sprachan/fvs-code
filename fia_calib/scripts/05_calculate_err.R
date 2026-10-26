# DESCRIPTION ==================================================================
#>
#> Purpose: Calculate various tree-level FVS prediction error metrics and save
#> result to a dataframe.
#> 
#> Outputs: data/sim_outputs/all_IE_uncalib_err.RDS
#
#> Notes:
#>
#> Data Sources: FVS run from 04_fvs_runs.R
#> 
# ==============================================================================
# Dependencies ----
library(dplyr)
library(ggplot2)
library(rFVSIEtools)

# Functions ----
calc_ba_ft <- function(diameter){
  (pi*(0.5*diameter)^2)/144
}

# FVS run results ----
fvs_conn <- DBI::dbConnect(RSQLite::SQLite(), 
                           file.path('fvs_kwd_files', 'uc', 'FVSUncalib.db'))
DBI::dbListTables(fvs_conn)
fvs_trees <- fvs_conn |>
  tbl('FVS_TreeList') |>
  select(CaseID, CID = StandID, Year, PrdLen, SpeciesFIA, TREE_ID = TreeId, 
         fvs_dbh = DBH, fvs_ht = Ht) |>
  collect() |>
  mutate(PID = substr(CID, 1, 11),
         TREE_ID = as.integer(TREE_ID))
DBI::dbDisconnect(fvs_conn)

# FIA data ----
conn <- DBI::dbConnect(RSQLite::SQLite(), file.path('data', 'fvs_ready.db'))
fia_stands <- tbl(conn, 'FVS_StandInit') |>
  select(STAND_ID, INV_YEAR, LATITUDE, LONGITUDE, LOCATION, ECOREGION, AGE,
         ASPECT, SLOPE, ELEVFT, MAX_SDI_FIA, STDORGCD, PID, CID, EMAP_HEX,PV_CODE,
         PV_FIA_HABTYPCD1, dominant_sp, matched_by, OWNCD, CONDPROP_UNADJ, BRK_DBH,
         NUM_PLOTS, PROP_BASIS) |>
  collect() |>
  group_by(CID) |>
  arrange(INV_YEAR) |>
  mutate(REM_CD_COND = row_number(),
         N_REM_COND = n(),
         REM_YEAR = case_when(N_REM_COND == 1 ~ NA,
                              N_REM_COND == 2 ~ max(INV_YEAR),
                              N_REM_COND == 3 ~ INV_YEAR[2])) |>
  ungroup()
fia_tree_init <- tbl(conn, 'FVS_TreeInit') |>
  collect()
fia_trees <- tbl(conn, 'FIA_allTrees') |>
 select(PID, TREE_CN, PLOT_ID, TUID, TREE_COUNT, HISTORY, SPECIES, HTTOPK, 
        DAMAGE1, SEVERITY1, FIA_TREE_ID, TREE_COUNT, 
        fia_dbh = DIAMETER, fia_ht = HT, INV_YEAR, EMAP_HEX, CID, calib_use) |>
  collect() |>
  mutate(SPECIES = ifelse(nchar(SPECIES) <= 2, stringr::str_pad(SPECIES, 3, side = 'left', pad = '0'), SPECIES))
tree_id_key <- tbl(conn, 'Tree_ID_KEY') |>
  collect()
DBI::dbDisconnect(conn)

# include remeasurement info to help with checks
fia_trees_rem <- fia_trees |>
  left_join(fia_stands[c('STAND_ID', 'INV_YEAR', 'REM_CD_COND', 'N_REM_COND',
                         'REM_YEAR')],
            by = join_by(CID == STAND_ID, INV_YEAR == INV_YEAR)) |>
  mutate(calib_use = as.logical(calib_use))

# Link FVS tree data with FIA tree data ----
fvs_trees_id <- fvs_trees |>
  left_join(tree_id_key)
fvs_trees_linked <- fia_trees_rem |>
  left_join(fvs_trees_id, by = join_by(CID, PID, TUID, INV_YEAR == Year)) |>
  filter(!is.na(fvs_dbh),
         # only need errors for trees we'll use for the calibration work
         calib_use) |>
  rename(fia_sp = SPECIES, fvs_sp = SpeciesFIA) 

stopifnot(length(unique(fvs_trees_linked$TREE_CN)) == nrow(fvs_trees_linked))

# check that linkage worked: DBH, height, and species should match
dbh1_diffs <- fvs_trees_linked |>
  ungroup() |>
  filter(REM_CD_COND == 1) |>
  mutate(dbh_diff = fvs_dbh - fia_dbh,
         ht_diff = fvs_ht - fia_ht)
unique(dbh1_diffs$dbh_diff) # all rounding errors
unique(dbh1_diffs$ht_diff) # 0 is the only difference

fvs_trees_linked |>
  select(fia_sp, fvs_sp) |>
  filter(fvs_sp != 998,
         fvs_sp != 299,
         as.integer(fia_sp) != as.integer(fvs_sp)) |>
  distinct() 
# mismatches are all just FVS translation: 
#> 747 --> 740 = Populus heterophylla --> Populus sp.
#> 374 --> 375 = Betula occidentalis --> Betula papyrifera
#> 015 --> 017 = Abies concolor --> Abies grandis

# Species info ----
fia_sp_info <- data.frame(fia_prefix = c('01', '06', '07', 
                                         '09', '10', '11', '12', '20', 
                                         '23', '24', '26', 
                                         '35', '37', '47', 
                                         '74', '76'),
                          fia_sp_group = c('Fir', 'Juniper', 'Larch',
                                           'Spruce','Pine', 'Pine', 'Pine', 'Douglas-fir',
                                           'Yew', 'Cedar', 'Hemlock',
                                           'Alder', 'Birch', 'Mountain-mahogany',
                                           'Populus', 'Prunus'))
fvs_sp_info <- data.frame(fvs_prefix = c('01', '06', '07', 
                                         '09', '10', '11', '12', '20', 
                                         '23', '24', '26', 
                                         '37', '74', '29',
                                         '99'),
                          fvs_sp_group = c('Fir', 'Juniper', 'Larch',
                                           'Spruce', 'Pine', 'Pine', 'Pine', 'Douglas-fir',
                                           'Yew', 'Cedar', 'Hemlock',
                                           'Birch', 'Populus', 'Softwood','Hardwood'))
species_common <- data.frame(fia_cd = c('017', '019', '066',
                                        '072', '073', '093',
                                        '101', '108', '113', 
                                        '119', '122', '202',
                                        '231', '242', '263',
                                        '264', '299', '375',
                                        '740', '746', '998'),
                             common_name = c('Grand fir', 'subalpine fir', 'Rocky mountain juniper',
                                             'subalpine larch', 'western larch', 'Engelmann spruce',
                                             'whitebark pine', 'lodgepole pine', 'limber pine',
                                             'western white pine', 'ponderosa pine', 'Douglas-fir',
                                             'Pacific yew', 'western redcedar', 'western hemlock',
                                             'mountain hemlock', 'softwood', 'paper birch',
                                             'cottonwood', 'quaking aspen', 'hardwood'))
# calculate error and save ----
stands_join <- fia_stands |>
  filter(REM_CD_COND == 1) |>
  select(-STAND_ID, -REM_YEAR, -REM_CD_COND, -N_REM_COND, -INV_YEAR,
         -STDORGCD, -PV_FIA_HABTYPCD1, -MAX_SDI_FIA)

trees_err <- fvs_trees_linked |>
  ungroup() |>
  select(-DAMAGE1, -SEVERITY1, -TREE_CN, -PLOT_ID, -CaseID, -TREE_ID,
         -FIA_TREE_ID, -REM_YEAR, -PrdLen) |>
  mutate(fvs_ba = calc_ba_ft(fvs_dbh), fia_ba = calc_ba_ft(fia_dbh)) |>
  filter(REM_CD_COND < 3, N_REM_COND >= 2) |>
  tidyr::pivot_wider(names_from = REM_CD_COND,
                     values_from = c(fvs_dbh, fvs_ba, fvs_ht, 
                                     fia_dbh, fia_ba, fia_ht, 
                                     INV_YEAR, HISTORY, HTTOPK, TREE_COUNT)) |>
  # only keep trees that were measured both times and that were alive at first
  filter(!is.na(fia_dbh_2), HISTORY_1 == 1) |>
  # calculate growth and error
  mutate(REM_INT = INV_YEAR_2-INV_YEAR_1,
         fia_dds = dob_to_dib(fvs_sp, fia_dbh_2)^2-dob_to_dib(fvs_sp, fia_dbh_1)^2,
         fvs_dds = dob_to_dib(fvs_sp, fvs_dbh_2)^2-dob_to_dib(fvs_sp, fvs_dbh_1)^2,
         fvs_hg = fvs_ht_2-fvs_ht_1,
         fia_hg = fia_ht_2-fia_ht_1,
         fia_dg = fia_dbh_2-fia_dbh_1,
         fvs_dg = fvs_dbh_2-fvs_dbh_1,
         dg_err = fia_dg-fvs_dg,
         hg_err = fia_hg-fvs_hg,
         # matches FVS-IE self calibration procedure, see ie/dgdriv.f line 479
         ldds_err_raw = log(fia_dds)-log(fvs_dds),
         ldds_err = ldds_err_raw*(10/REM_INT),
         died = HISTORY_2 != 1,
         broke = !is.na(HTTOPK_2),
         started_broken = !is.na(HTTOPK_1)) |>
  # add in stand info
  left_join(stands_join) |>
  mutate(fia_sp = stringr::str_pad(fia_sp, width = 3, pad = '0'),
         fia_prefix = substr(fia_sp, 1, 2),
         fvs_prefix = substr(fvs_sp, 1, 2)) |>
  left_join(fia_sp_info) |>
  left_join(fvs_sp_info) |>
  left_join(species_common, by = join_by(fvs_sp == fia_cd)) |>
  select(-fia_prefix, -fvs_prefix)
saveRDS(trees_err, file.path('data', 'sim_outputs', 'all_IE_uncalib_err.RDS'))

# Session information ----
sessionInfo()
# R version 4.6.1 (2026-06-24 ucrt)
# Platform: x86_64-w64-mingw32/x64
# Running under: Windows 11 x64 (build 26200)
# 
# Matrix products: default
# LAPACK version 3.12.1
# 
# locale:
# [1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8    LC_MONETARY=English_United States.utf8
# [4] LC_NUMERIC=C                           LC_TIME=English_United States.utf8    
# 
# time zone: America/Denver
# tzcode source: internal
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
# [1] ggplot2_4.0.3 dplyr_1.2.1  
# 
# loaded via a namespace (and not attached):
# [1] vctrs_0.7.3        cli_3.6.6          rlang_1.3.0        stringi_1.8.9      otel_0.2.0         DBI_1.3.0         
# [7] purrr_1.2.2        generics_0.1.4     S7_0.2.2           glue_1.8.1         bit_4.6.0          dbplyr_2.6.0      
# [13] scales_1.4.0       grid_4.6.1         tibble_3.3.1       fastmap_1.2.0      lifecycle_1.0.5    memoise_2.0.1     
# [19] stringr_1.6.0      compiler_4.6.1     RColorBrewer_1.1-3 RSQLite_3.53.3     blob_1.3.0         pkgconfig_2.0.3   
# [25] tidyr_1.3.2        rstudioapi_0.18.0  farver_2.1.2       R6_2.6.1           utf8_1.2.6         tidyselect_1.2.1  
# [31] pillar_1.11.1      magrittr_2.0.5     withr_3.0.3        tools_4.6.1        bit64_4.8.2        gtable_0.3.6      
# [37] cachem_1.1.0