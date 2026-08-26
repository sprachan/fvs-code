# DESCRIPTION ==================================================================
#>
#> Purpose: Prepare FIA stand and tree init data for running in FVS by:
#> ********* Stand Tables ***********
#> * Matching habitat types to IE-recognized ones
#> * Filtering out stands (conditions) with IE-unrecognized habitat types in 
#>   their first measurement
#> * Filtering out stands (conditions) that had only been measured once
#> * Adding a "cycleat" column that reflects the year that FVS ought to project
#>   to i.e., the measurement year after the first measurement
#> * Replacing STAND_ID values (see def in FIA2FVS document) with CID to improve
#>   post-run processing.
#> 
#>   
#> ********* Tree Tables ***********
#> * Removing trees whose species ID changed between measurements
#> * Removing seedlings, AKA trees with <= 0.1" diameter
#> * Only keeping trees that are in stands that made it through the filtering
#>   process
#> * Creating a dataframe that allows PID/TUID combos to be mapped to integer
#>   FVS tree IDs.
#> 
#> Outputs: FVS-ready database: data/fvs_ready.db, which contains:
#> * FVS_StandInit: Filtered stands
#> * FVS_TreeInit: First measurement of filtered trees
#> * FVS_allTrees: All measurements of filtered trees
#> * Tree_ID_KEY: Links PID/TUID combinations to integer FVS tree IDs
#> 
#> Notes: fvs_ready.db/FVS_StandInit.STAND_ID == CID
#>
#> Data Sources: 
#> - FIA data
#> https://research.fs.usda.gov/products/dataandtools/fia-datamart
#> - PV/PA lookup tables from FVS-IE Variant Overview (Appendix tables 11.1.1 and 11.1.2)
# ==============================================================================
library(dplyr)
library(ggplot2)
# Get FVS-ready "stand" (condition) and tree data ----
fia_conds <- readRDS(file.path('data', 'fia_condspat_filt.RDS')) |>
  select(-STATECD, -UNITCD, -COUNTYCD, -KINDCD, -INTENSITY, -STDORGCD, -x, -y, -epsg)

fvs_stand_init_list <- vector('list', 4)
fvs_tree_init_list <- vector('list', 4)
states <- c('MT', 'WA', 'ID', 'OR')

for(i in seq_along(states)){
  db_loc <- file.path('raw_data', 'fia', 
                      paste0('SQLite_FIADB_', states[i], '.db'))
  conn <- DBI::dbConnect(RSQLite::SQLite(), db_loc)
  
  copy_to(conn, df = fia_conds, name = 'FILT_CONDS', 
          overwrite = TRUE, temporary = TRUE)
  
  fvs_stand_init_list[[i]] <- tbl(conn, 'FVS_STANDINIT_COND') |>
    inner_join(tbl(conn, 'FILT_CONDS'), by = join_by(STAND_CN == CN)) |>
    collect()
  fvs_tree_init_list[[i]] <- tbl(conn, 'FVS_TREEINIT_COND') |>
    inner_join(tbl(conn, 'FILT_CONDS'), by = join_by(STAND_CN == CN)) |>
    collect()
  
  DBI::dbDisconnect(conn)
}

fvs_stand_init <- bind_rows(fvs_stand_init_list) 
fvs_tree_init <- bind_rows(fvs_tree_init_list)
rm(fvs_stand_init_list, fvs_tree_init_list)


# Clean-up habitat types ----
# Goal: Minimize stands that run with default habitat types.
#> Some habitat types that ought to be recognized by IE "fall through the cracks" 
#> presumably due to region-specific codes that nonetheless refer to the same 
#> habitat types.

## Get relevant data ----
# Read in reference table so we can connect PV/PA codes to scientific names
conn <- DBI::dbConnect(RSQLite::SQLite(), db_loc)
ref_habtypes <- dplyr::tbl(conn, 'REF_HABTYP_DESCRIPTION') |>
  select(-CREATED_DATE, -MODIFIED_DATE) |>
  collect() |>
  mutate(PLANT_ASSOC = is.na(as.integer(HABTYPCD)),
         habtyp_int = as.integer(HABTYPCD),
         habtyp_series = ifelse(habtyp_int >= 100&habtyp_int<1000,
                                paste0(substr(habtyp_int, 1, 2), 0),
                                habtyp_int),
         COMMON_NAME = tolower(COMMON_NAME),
         # some common names have trailing parens like (blue mountains)
         #> that we may not need
         common_no_parens = gsub("\\s*\\([^)]*\\)\\s*$", "", COMMON_NAME))
DBI::dbDisconnect(conn)

ref_habtypes_nopub <- ref_habtypes |>
  # COMMON_NAME and PUB_CD seem to similarly count the same habitat types
  #> in different locations as being something different
  select(-PUB_CD, -CN, -COMMON_NAME) |>
  distinct()

# habitat type tables from FVS-IE overview for finding recognized hab types
ie_habtypes <- read.csv(file.path('raw_data', 'ie_habtypes.csv')) |>
  rename(HABTYPCD = Habitat.Code,
         SCIENTIFIC_NAME = Abbreviation,
         IE_COMMON_NAME = Habitat.Type.Name,
         IE_HABTYP = Original.Habitat.Type)|>
  mutate(HABTYPCD = as.character(HABTYPCD),
         IE_COMMON_NAME = tolower(IE_COMMON_NAME),
         SCIENTIFIC_NAME = sub(' ', '', SCIENTIFIC_NAME))
ie_pas <- read.csv(file.path('raw_data', 'ie_plantassocs.csv')) |>
  rename(PA_CD = Code,
         SCIENTIFIC_NAME = Plant.Association.Abbreviation,
         IE_COMMON_NAME = Description,
         HABTYPCD = Mapped.Habitat.Type) |>
  mutate(IE_COMMON_NAME = tolower(IE_COMMON_NAME),
         SCIENTIFIC_NAME = sub(' ', '', SCIENTIFIC_NAME),
         HABTYPCD = as.character(HABTYPCD)) |>
  select(-Plant.Assoc.FVS.Seq.Number)
ie_all <- ie_pas |>
  select(PA_CD, HABTYPCD) |>
  left_join(ie_habtypes) |>
  bind_rows(ie_habtypes) |>
  # need to have a column that has habitat type regardless of if it's a PA
  #> code or a PV code.
  mutate(gen_cd = ifelse(is.na(PA_CD), HABTYPCD, PA_CD))

## Find matching habitat types ----
# match all PV/PV Ref combos in the FIA data
fia_hab_ref <- fvs_stand_init |>
  select(PV_FIA_HABTYPCD1, PV_REF_CODE) |>
  filter(!is.na(PV_REF_CODE)) |>
  mutate(PV_REF_CODE = as.character(PV_REF_CODE))|>
  distinct() |>
  # there's one row that has both codes undefined which is useless to us
  filter(!(is.na(PV_REF_CODE)&is.na(PV_FIA_HABTYPCD1))) |>
  left_join(ref_habtypes, by = join_by(PV_FIA_HABTYPCD1 == HABTYPCD,
                                       PV_REF_CODE == PUB_CD)) |>
  mutate(ie_recognized = case_when(PLANT_ASSOC == FALSE ~ habtyp_series %in% ie_habtypes$HABTYPCD,
                                   PLANT_ASSOC == TRUE ~ PV_FIA_HABTYPCD1 %in% ie_pas$PA_CD))
# match all PV-only codes
fia_hab_noref <- fvs_stand_init |>
  select(PV_FIA_HABTYPCD1, PV_REF_CODE) |>
  filter(is.na(PV_REF_CODE)) |>
  distinct() |>
  # there's one row that has both codes undefined which is useless to us
  filter(!(is.na(PV_REF_CODE)&is.na(PV_FIA_HABTYPCD1))) |>
  left_join(ref_habtypes_nopub, by = join_by(PV_FIA_HABTYPCD1 == HABTYPCD)) |>
  mutate(ie_recognized = case_when(PLANT_ASSOC == FALSE ~ habtyp_series %in% ie_habtypes$HABTYPCD,
                                   PLANT_ASSOC == TRUE ~ PV_FIA_HABTYPCD1 %in% ie_pas$PA_CD),
         PV_REF_CODE = as.character(NA))

fia_hab <- bind_rows(fia_hab_ref, fia_hab_noref)

# if there was a PV code match with an IE hab code, use the IE habitat code 
#> ie., left-most column in variant overview tables 11.1.1 and 11.1.2
code_match <- fia_hab |>
  filter(ie_recognized) |>
  # use the rounded habitat type instead of phase because that's what's in 11.1.1
  mutate(join_key = ifelse(PLANT_ASSOC, PV_FIA_HABTYPCD1, habtyp_series)) |>
  left_join(ie_all[c('gen_cd', 'IE_HABTYP')], by = join_by(join_key == gen_cd)) |>
  # because some PA codes aren't recognized, fill in with the matching numeric
  #> code
  mutate(HABCODE = ifelse(PLANT_ASSOC, IE_HABTYP, PV_FIA_HABTYPCD1))

# otherwise, attempt to match by scientific name/abbreviation
abb_match <- fia_hab |>
  filter(!ie_recognized|is.na(ie_recognized), !is.na(SCIENTIFIC_NAME)) |>
  select(PV_FIA_HABTYPCD1, PV_REF_CODE, FIA_SCI = SCIENTIFIC_NAME,
         common_no_parens) |>
  # some sci names in FIA don't quite match FVS-IE sci names:
  mutate(abbr_ie = case_when(substr(FIA_SCI, 1, 10) == 'PSME/PSSPS' ~ 'PSME/AGSP',
                             substr(FIA_SCI, 1, 10) == 'PIPO/PSSPS' ~ 'PIPO/AGSP',
                             substr(FIA_SCI, 1, 10) == 'ABLA/LUGLH' ~ 'ABLA/LUHI',
                             substr(FIA_SCI, 1, 10) == 'ABLA/ALVIS' ~ 'ABLA/ALSI',
                             grepl('-.*/', FIA_SCI) ~ NA,
                             .default = substr(FIA_SCI, 1, 9))) |>
  # there are two diff ABLA series and I can't tell which this is supposed to be
  #> from FIA database alone
  filter(FIA_SCI != 'ABLA') |> 
  left_join(ie_habtypes, by = join_by(abbr_ie == SCIENTIFIC_NAME)) |>
  # assign series if the dominant tree is listed as a series in 11.1.1
  mutate(FILLED_IE_HABTYP = case_when(is.na(IE_HABTYP) & substr(abbr_ie, 1, 4) == 'ABGR' ~ 500,
                                      is.na(IE_HABTYP) & substr(abbr_ie, 1, 4) == 'PSME' ~ 260,
                                      is.na(IE_HABTYP) & substr(abbr_ie, 1, 5) == 'PICEA' ~ 420,
                                      is.na(IE_HABTYP) & substr(abbr_ie, 1, 5) == 'PIEN/' ~ 410,
                                      is.na(IE_HABTYP) & abbr_ie == 'PIEN' ~ 400,
                                      is.na(IE_HABTYP) & substr(abbr_ie, 1, 5) == 'PICO/' ~ 900,
                                      is.na(IE_HABTYP) & substr(abbr_ie, 1, 4) == 'PIPO' ~ 100,
                                      .default = IE_HABTYP),
         # retain original habitat type code unless it needed to be a series
         HABCODE = ifelse(is.na(HABTYPCD), FILLED_IE_HABTYP, HABTYPCD)) |>
  # don't need to keep rows that have no IE habitat translation
  filter(!is.na(FILLED_IE_HABTYP))

hab_key <- bind_rows(code_match, abb_match) |>
  select(PV_FIA_HABTYPCD1, PV_REF_CODE, HABCODE) |>
  distinct()

# Filter stands ----
fia_has_hab <- fvs_stand_init |>
  select(PID, CID, PV_FIA_HABTYPCD1, PV_REF_CODE, INV_YEAR) |>
  group_by(CID) |>
  arrange(INV_YEAR) |>
  mutate(PV_REF_CODE = as.character(PV_REF_CODE),
         REM_CD_COND = row_number()) |>
  left_join(hab_key) |>
  # get condition IDs for conditions w IE-recognized habitat type in first
  #> measurement
  filter(REM_CD_COND == 1,
         !is.na(HABCODE)) |>
  pull(CID) |>
  unique()

fvs_stand_init_filt <- fvs_stand_init |>
  group_by(CID) |>
  arrange(CID, INV_YEAR) |>
  mutate(PV_REF_CODE = as.character(PV_REF_CODE),
         N_MEAS_COND = n(),
         REM_CD_COND = row_number()) |>
  ungroup() |>
  left_join(hab_key) |>
  # remove stands with non-translatable habitat types
  filter(N_MEAS_COND > 1, CID %in% fia_has_hab) |>
  # drop columns that won't be processed by FVS; use translated habitat codes
  select(-ADDFILES, -COMPARTMENT, -CREATED_DATE, -DATUM, -ELEVATION, 
         -FOREST_TYPE, -FOREST_TYPE_FIA, -FVSKEYWORDS, -GIS_LINK, -GROUPS,
         -INVYR, -MAX_SDI, -MODEL_TYPE, -MODIFIED_DATE, -PHOTO_CODE,
         -PHOTO_REF, -VERSION, -PV_CODE) |>
  rename(FIA_PV_REF = PV_REF_CODE, PV_CODE = HABCODE) |>
  mutate(STAND_ID = CID)
stopifnot(nrow(fvs_stand_init_filt) == length(unique(fvs_stand_init_filt$STAND_CN)))

fvs_trees_all <- fvs_tree_init |>
  select(-STAND_ID) |>
  # exclude seedlings
  filter(DIAMETER > 0.1) |>
  # only keep trees associated with retained stands
  inner_join(distinct(fvs_stand_init_filt[c('STAND_CN', 'STAND_ID', 'INV_YEAR')])) |>
  # drop unnecessary columns
  select(-STANDPLOT_CN, -TAG_ID, -DISTANCE, -SITE_TREE_FLAG, 
         -PV_CODE, -PV_REF_CODE, -BH_YEARS,
         -CREATED_DATE, -MODIFIED_DATE, -VERSION, -PLT_CN, -CONDID,
         -COND_STATUS_CD, -RESERVCD, -OWNCD, -CONDPROP_UNADJ, -PROP_BASIS,
         -INVYR) |>
  mutate(FIA_TREE_ID = TREE_ID,
         PLT_NUM = substr(STANDPLOT_ID, nchar(STANDPLOT_ID), nchar(STANDPLOT_ID)),
         TUID = paste0(PID, PLT_NUM, TREE_ID)) |>
  select(-TREE_ID)
stopifnot(nrow(fvs_trees_all) == length(unique(fvs_trees_all$TREE_CN)))

# Filter trees ----
## Check for mismatched species ----
# Presumably due to measurement error (though I'm worried it's because the 
#> tree unique identifier is in fact not unique...)
sp_mismatches <- fvs_trees_all |>
  group_by(TUID) |>
  arrange(INV_YEAR) |>
  mutate(N_MEAS_TRE = n(),
         REM_CD_TRE = row_number(),
         INV_YEAR = as.character(INV_YEAR)) |>
  filter(N_MEAS_TRE >= 2) |>
  select(TUID, SPECIES, REM_CD_TRE, INV_YEAR) |>
  tidyr::pivot_wider(names_from = REM_CD_TRE, values_from = c(SPECIES, INV_YEAR)) |>
  mutate(match12 = SPECIES_1 == SPECIES_2,
         match23 = SPECIES_2 == SPECIES_3, 
         match13 = SPECIES_1 == SPECIES_3) |>
  filter(!match12|!match23|!match13) # if there was ever any disagreement, omit

## Assign TREE_IDs that can be mapped to TUID ----
tree_id_key <- fvs_trees_all |>
  filter(!TUID %in% sp_mismatches$TUID) |>
  select(PID, TUID) |>
  distinct() |>
  group_by(PID) |>
  arrange(TUID) |>
  mutate(TREE_ID = row_number()) |>
  ungroup() 
stopifnot(nrow(tree_id_key) == length(unique(tree_id_key$TUID)))

## Final tree output ----
fvs_tree_init_out <- fvs_trees_all |>
  left_join(tree_id_key) |>
  filter(REM_CD == 1,
         !TUID %in% sp_mismatches$TUID)
stopifnot(nrow(fvs_tree_init_out) == length(unique(fvs_tree_init_out$TREE_CN)))
stopifnot(nrow(fvs_tree_init_out) == length(unique(fvs_tree_init_out$TUID)))

# Write stand and tree tables to SQLite database for FVS ease of access ----
conn <- DBI::dbConnect(RSQLite::SQLite(), file.path('data', 'fvs_ready.db'))
DBI::dbWriteTable(conn, name = 'FVS_StandInit', value = fvs_stand_init_filt, overwrite = TRUE)
DBI::dbWriteTable(conn, name = 'FVS_TreeInit', value = fvs_tree_init_out, overwrite = TRUE)
DBI::dbWriteTable(conn, name = 'FIA_allTrees', 
                  value = filter(fvs_trees_all,
                                 !TUID %in% sp_mismatches$TUID), overwrite = TRUE)
DBI::dbWriteTable(conn, name = 'Tree_ID_KEY', value = tree_id_key, overwrite = TRUE)
DBI::dbDisconnect(conn)
