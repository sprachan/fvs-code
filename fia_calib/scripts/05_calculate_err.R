# DESCRIPTION ==================================================================
#>
#> Purpose:
#> 
#> Outputs:
#> 
#> Notes:
#>
#> Data Sources: 
#> 
# ==============================================================================
# Dependencies ----
library(dplyr)
library(ggplot2)

# Functions ----
calc_ba_ft <- function(diameter){
  (pi*(0.5*diameter)^2)/144
}
# FVS run data ----
fvs_conn <- DBI::dbConnect(RSQLite::SQLite(), file.path('fvs_kwd_files', 'uc', 'FVSUncalib.db'))
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
         ASPECT, SLOPE, ELEVFT, MAX_SDI_FIA, STDORGCD, PID, CID, EMAP_HEX,PV_CODE) |>
  collect() |>
  group_by(CID) |>
  arrange(INV_YEAR) |>
  mutate(REM_CD_COND = row_number(),
         N_REM_COND = n(),
         REM_YEAR = case_when(N_REM_COND == 1 ~ NA,
                              N_REM_COND == 2 ~ max(INV_YEAR),
                              N_REM_COND == 3 ~ INV_YEAR[2])) |>
  ungroup()
fia_trees <- tbl(conn, 'FIA_allTrees') |>
 select(PID, TREE_CN, PLOT_ID, TUID, TREE_COUNT, HISTORY, SPECIES, FIA_TREE_ID,
      fia_dbh = DIAMETER, fia_ht = HT, INV_YEAR, EMAP_HEX, CID) |>
  collect() 
tree_id_key <- tbl(conn, 'Tree_ID_KEY') |>
  collect()
DBI::dbDisconnect(conn)

fia_trees_rem <- fia_trees |>
  left_join(fia_stands[c('STAND_ID', 'INV_YEAR', 'REM_CD_COND', 'N_REM_COND', 'REM_YEAR')],
            by = join_by(CID == STAND_ID, INV_YEAR == INV_YEAR))

# Link FVS tree data with FIA tree data ----
fvs_trees_linked <- fvs_trees |> 
  ungroup() |>
  left_join(tree_id_key)  |>
  left_join(fia_trees_rem, by = join_by(CID, PID, TUID, Year == INV_YEAR)) |>
  filter(!is.na(TREE_CN))
stopifnot(length(unique(fvs_trees_linked$TREE_CN)) == nrow(fvs_trees_linked))

# check that linkage worked: DBH, height, and species should match
dbh1_diffs <- fvs_trees_linked |>
  ungroup() |>
  filter(REM_CD_COND == 1) |>
  mutate(dbh_diff = fvs_dbh-fia_dbh,
         ht_diff = fvs_ht - fia_ht)
unique(dbh1_diffs$dbh_diff) # all rounding errors
unique(dbh1_diffs$ht_diff) # all 00

fvs_trees_linked |>
  ungroup() |>
  select(SpeciesFIA, SPECIES) |>
  filter(SpeciesFIA != 998,
         SpeciesFIA != 299,
         as.integer(SPECIES) != as.integer(SpeciesFIA)) |>
  distinct() 
# mismatches are all just FVS translation: 
#> 747 --> 740 = Populus heterophylla --> Populus sp.
#> 374 --> 375 = Betula occidentalis --> Betula papyrifera
#> 15 --> 017 = Abies concolor --> Abies grandis

# Calculate error and save to RDS ----
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
trees_err <- fvs_trees_linked |>
  select(CID, PID, TUID, REM_CD_COND, SPECIES, fvs_sp = SpeciesFIA,
         fvs_dbh, fvs_ht, fia_dbh, fia_ht, Year, 
         HISTORY) |>
  mutate(fvs_ba = calc_ba_ft(fvs_dbh), fia_ba = calc_ba_ft(fia_dbh)) |>
  filter(REM_CD_COND < 3) |>
  tidyr::pivot_wider(names_from = REM_CD_COND,
                     values_from = c(fvs_dbh, fvs_ba, fvs_ht, 
                                     fia_dbh, fia_ba, fia_ht, 
                                     Year, HISTORY)) |>
  filter(HISTORY_1 == 1, !is.na(HISTORY_2)) |>
  mutate(fvs_dg = fvs_dbh_2-fvs_dbh_1,
         fia_dg = fia_dbh_2-fvs_dbh_1,
         fvs_hg = fvs_ht_2-fvs_ht_1,
         fia_hg = fia_ht_2-fia_ht_1,
         fvs_bag = fvs_ba_2-fvs_ba_1,
         fia_bag = fia_ba_2-fia_ba_1,
         dg_err = fia_dg-fvs_dg,
         hg_err = fia_hg-fvs_hg,
         ba_err = fia_bag-fvs_bag,
         sf_dg = fia_dg/fvs_dg,
         sf_hg = fia_hg/fvs_hg,
         sf_bg = fia_bag/fvs_bag,
         died = HISTORY_2 != 1) |>
  left_join(filter(fia_stands, REM_CD_COND == 1)) |>
  select(-REM_CD_COND, -N_REM_COND, -REM_YEAR, -STAND_ID) |>
  mutate(SPECIES = stringr::str_pad(SPECIES, width = 3, pad = '0'),
         fia_prefix = substr(SPECIES, 1, 2),
         fvs_prefix = substr(fvs_sp, 1, 2)) |>
  left_join(fia_sp_info) |>
  left_join(fvs_sp_info) |>
  left_join(species_common, by = join_by(fvs_sp == fia_cd)) 
saveRDS(trees_err, file.path('data', 'sim_outputs', 'all_IE_uncalib.RDS'))
