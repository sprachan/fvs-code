# DESCRIPTION ==================================================================
#>
#> Purpose: Prepare FIA stand and tree init data for running in FVS by:
#> * Adding a "cycleat" column that reflects the year that FVS ought to project
#> to i.e., the year after the first measurement
#> * Match habitat types to IE-recognized ones and filter out unrecognized
#>  habitat types
#> * Filter out stands (conditions) that had only been measured once.
#> 
#> Outputs: FVS-ready database: data/fvs_ready.db
#> 
#> Notes:
#>
#> Data Sources: 
#> - FIA data
#> https://research.fs.usda.gov/products/dataandtools/fia-datamart
#> - PV/PA lookup tables from FVS-IE Variant Overview
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

fvs_stand_init <- bind_rows(fvs_stand_init_list) |>
  select(-VARIANT) |>
  group_by(CID) |>
  mutate(N_REM_COND = n(),
         cycleat = ifelse(N_REM_COND > 1, INV_YEAR[2], NA)) |>
  ungroup() 
fvs_tree_init <- bind_rows(fvs_tree_init_list)

rm(fvs_stand_init_list, fvs_tree_init_list)

# Filter tree and stand data to reduce FVS warnings and speed FVS run-times ----
# FVS gives a warning if "too few projectable trees", so we can filter out
#> stands that fall in that category to get more accurate stand sample size
#> and speed things up.
#> Also, only interested in large trees for now.

fvs_tree_init_filt <- fvs_tree_init |>
  filter(DIAMETER >= 3,
         HISTORY == 1) |>
  inner_join(distinct(fvs_stand_init['STAND_ID']))
fvs_stand_init_filt <- fvs_stand_init |>
  inner_join(distinct(fvs_tree_init_filt['STAND_ID'])) 

# Clean-up habitat types ----
# This is necessary because I've seen some habitat types that ought to be
#> recognized by IE "fall through the cracks" presumably due to region-specific
#> PA/PV codes that nonetheless refer to the same habitat types. I want to
#> minimize the number of stands that are run using default habitat type.

## Get relevant data ----
# Read in reference table so we can connect PV/PA codes to scientific names
conn <- DBI::dbConnect(RSQLite::SQLite(), db_loc)
ref_habtypes <- dplyr::tbl(conn, 'REF_HABTYP_DESCRIPTION') |>
  select(-CREATED_DATE, -MODIFIED_DATE) |>
  collect() |>
  filter(HABTYPCD %in% fvs_stand_init$PV_FIA_HABTYPCD1,
         VALID == 'Y') |>
  mutate(PLANT_ASSOC = is.na(as.integer(HABTYPCD)),
         habtyp_int = as.integer(HABTYPCD),
         habtyp_series = ifelse(habtyp_int >= 100&habtyp_int<1000,
                                paste0(substr(habtyp_int, 1, 2), 0),
                                habtyp_int),
         COMMON_NAME = tolower(COMMON_NAME))
DBI::dbDisconnect(conn)

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
  mutate(gen_cd = ifelse(is.na(PA_CD),HABTYPCD, PA_CD))

## Find matching habitat types ----
# which habitat types in the FIA data have PV/PA codes matching FVS-IE tables?
fia_hab <- fvs_stand_init_filt |>
  ungroup() |>
  select(PV_FIA_HABTYPCD1, PV_REF_CODE) |>
  mutate(PV_REF_CODE = as.character(PV_REF_CODE))|>
  distinct() |>
  filter(!(is.na(PV_REF_CODE)&is.na(PV_FIA_HABTYPCD1))) |>
  left_join(ref_habtypes, by = join_by(PV_FIA_HABTYPCD1 == HABTYPCD,
                                       PV_REF_CODE == PUB_CD)) |>
  mutate(ie_recognized = case_when(PLANT_ASSOC == FALSE ~ habtyp_series %in% ie_habtypes$HABTYPCD,
                                   PLANT_ASSOC == TRUE ~ PV_FIA_HABTYPCD1 %in% ie_pas$PA_CD))

# if there's a PV/PA code match, use the associated IE habitat code (left-most column)
code_match <- fia_hab |>
  filter(ie_recognized) |>
  mutate(join_key = ifelse(PLANT_ASSOC, PV_FIA_HABTYPCD1, habtyp_series)) |>
  left_join(ie_all[c('gen_cd', 'IE_HABTYP')], by = join_by(join_key == gen_cd)) |>
  mutate(HABCODE = ifelse(PLANT_ASSOC, IE_HABTYP, PV_FIA_HABTYPCD1))
# otherwise, match by scientific name/abbreviation
abb_match <- fia_hab |>
  filter(!ie_recognized) |>
  select(PV_FIA_HABTYPCD1, PV_REF_CODE, FIA_SCI = SCIENTIFIC_NAME, COMMON_NAME) |>
  # due to nomenclature changes, some sci names in FIA don't quite match FVS-IE 
  #> sci names; and all FVS-IE sci names omit any trailing numbers i.e., 
  #> ABGR/LIBO3 is recorded in the overview as ABGR/LIBO. Hence the substring.
  mutate(abbr_ie = case_when(FIA_SCI == 'PSME/PSSPS' ~ 'PSME/AGSP',
                             FIA_SCI == 'PIPO/PSSPS' ~ 'PIPO/AGSP',
                             FIA_SCI == 'ABLA/LUGLH' ~ 'ABLA/LUHI',
                             FIA_SCI == 'ABLA/ALVIS' ~ 'ABLA/ALSI',
                             .default = substr(FIA_SCI, 1, 9))) |>
  filter(abbr_ie != 'ABLA') |> # there are two diff ABLA series and I can't tell which this is supposed to be
  left_join(ie_habtypes, by = join_by(abbr_ie == SCIENTIFIC_NAME)) |>
  mutate(FILLED_IE_HABTYP = case_when(is.na(IE_HABTYP) & substr(abbr_ie, 1, 4) == 'ABGR' ~ 500,
                                      is.na(IE_HABTYP) & substr(abbr_ie, 1, 4) == 'PSME' ~ 260,
                                      is.na(IE_HABTYP) & substr(abbr_ie, 1, 5) == 'PICEA' ~ 420,
                                      is.na(IE_HABTYP) & substr(abbr_ie, 1, 5) == 'PIEN/' ~ 410,
                                      is.na(IE_HABTYP) & abbr_ie == 'PIEN' ~ 400,
                                      is.na(IE_HABTYP) & substr(abbr_ie, 1, 4) == 'PICO' ~ 900,
                                      is.na(IE_HABTYP) & substr(abbr_ie, 1, 4) == 'PIPO' ~ 100,
                                      .default = IE_HABTYP),
         HABCODE = ifelse(is.na(HABTYPCD), FILLED_IE_HABTYP, HABTYPCD)) |>
  filter(!is.na(FILLED_IE_HABTYP))

hab_key <- bind_rows(code_match, abb_match) |>
  mutate(HABCODE = ifelse(is.na(HABCODE), FILLED_IE_HABTYP, HABCODE))

# Create cleaned-up output tables ---
fvs_stand_init_out <- fvs_stand_init_filt |>
  mutate(PV_REF_CODE = as.character(PV_REF_CODE)) |>
  left_join(hab_key[c('HABCODE', 'PV_REF_CODE', 'PV_FIA_HABTYPCD1')]) |>
  filter(!is.na(HABCODE), N_REM_COND > 1) |>
  select(-ADDFILES, -COMPARTMENT, -CREATED_DATE, -DATUM, -ELEVATION, 
         -FOREST_TYPE, -FOREST_TYPE_FIA, -FVSKEYWORDS, -GIS_LINK, -GROUPS,
         -INVYR, -MAX_SDI, -MODEL_TYPE, -MODIFIED_DATE, -PHOTO_CODE,
         -PHOTO_REF, -VERSION, -PV_CODE) |>
  rename(FIA_PV_REF = PV_REF_CODE, PV_CODE = HABCODE)

fvs_tree_init_out <- fvs_tree_init_filt |>
  inner_join(fvs_stand_init_out['STAND_CN'])

# Write stand and tree tables to SQLite database for FVS ease of access ----
conn <- DBI::dbConnect(RSQLite::SQLite(), file.path('data', 'fvs_ready.db'))
DBI::dbWriteTable(conn, name = 'FVS_StandInit', value = fvs_stand_init_out, overwrite = TRUE)
DBI::dbWriteTable(conn, name = 'FVS_TreeInit', value = fvs_tree_init_filt, overwrite = TRUE)
DBI::dbDisconnect(conn)
