# DESCRIPTION ==================================================================
#>
#> Purpose: Run all (filtered) FIA conditions ("stands") through FVS to generate 
#> the dataset that will be used to fit the calibration model.
#> 
#> Outputs: Database of FVS runs (fvs_kwd_files/FVSOut.db) and keyword files.
#> 
#> Notes:
#>
#> Data Sources: 
#> - FIA data from FIA datamart
#> 
# ==============================================================================
# Dependencies ----
library(dplyr)
library(ggplot2)

# Functions ----
get_errs <- function(db, stands){
  out_conn <- DBI::dbConnect(RSQLite::SQLite(), db)
  errs <- dplyr::tbl(out_conn, 'FVS_Error') |>
    collect()|>
    mutate(warncd = as.integer(substr(Message, 4, 5)),
           warn_type = case_when(warncd == 1|warncd == 4|warncd == 16 ~ 'Keyword',
                                 warncd == 3 ~ 'Forest Bounds',
                                 warncd == 8 ~ 'Insufficient Trees',
                                 warncd == 9 ~ 'Plot Counts',
                                 warncd == 14|warncd == 23|(warncd>31&warncd<36) ~ 'Hab Type',
                                 warncd == 41 ~ 'Stocking',
                                 .default = 'Other')) |>
    inner_join(stands, by = join_by(StandID == STAND_ID))
  DBI::dbDisconnect(out_conn)
  errs
}

view_errs <- function(db, stands){
  errs <- get_errs(db, stands)
  message('Error messages: \n', paste(sort(unique(errs$Message)), collapse = '\n'))
  ggplot2::ggplot(errs)+ggplot2::geom_bar(ggplot2::aes(x = warn_type))
}

# File paths and data ----
fvs_bin <- 'C:/FVS/FVSSoftware/FVSbin'
conn <- DBI::dbConnect(RSQLite::SQLite(), file.path('data', 'fvs_ready.db'))
fvs_stand_init <- collect(tbl(conn, 'FVS_StandInit'))
DBI::dbDisconnect(conn)
fvs_outdb <- file.path('fvs_kwd_files', 'FVSUncalib.db')
## Reference tables
conn <- DBI::dbConnect(RSQLite::SQLite(), file.path('raw_data', 'fia', 'SQLite_FIADB_MT.db'))
ref_habtypes <- dplyr::tbl(conn, 'REF_HABTYP_DESCRIPTION') |>
  select(-CREATED_DATE, -MODIFIED_DATE) |>
  collect() |>
  filter(HABTYPCD %in% fvs_stand_init$PV_FIA_HABTYPCD1) 
DBI::dbDisconnect(conn)

# Run FVS ----
## Region 1 first, by location code ----
r1_stands <- filter(fvs_stand_init, REGION == 1, REM_CD == 1)
r1_stands_by_loc <- split(r1_stands$STAND_ID, r1_stands$LOCATION)
prog <- 0
pb <- txtProgressBar(min = 0, max = length(names(r1_stands_by_loc)), style = 3)
for(loc in names(r1_stands_by_loc)){
  sts <- r1_stands_by_loc[[loc]]
  cycleat <- r1_stands |>
    filter(STAND_ID %in% sts) |>
    pull(cycleat)
  kwd <- rFVSIEtools::write_multistand_key(STDIDENTs = sts, 
                                           out_dir = 'fvs_kwd_files',
                                           out_db = 'FVSUncalib.db',
                                           database = file.path('data', 'fvs_ready.db'),
                                           file_prefix = paste0('loc_', loc),
                                           calibrate = FALSE,
                                           triple = FALSE,
                                           add_regen = FALSE,
                                           n_years = 21,
                                           CYCLEAT = cycleat)
  rFVSIEtools::run_FVS(fvs_bin = fvs_bin, variant = 'ie', keyword_file = kwd)
  prog <- prog+1
  setTxtProgressBar(pb, prog)
}
close(pb)

view_errs(fvs_outdb, r1_stands)

## Region 6 ----
r6_stands <- filter(fvs_stand_init, REGION == 6, REM_CD == 1)
r6_stands_by_loc <- split(r6_stands$STAND_ID, r6_stands$LOCATION)
prog <- 0
pb <- txtProgressBar(min = 0, max = length(names(r1_stands_by_loc)), style = 3)
for(loc in names(r6_stands_by_loc)){
  sts <- r6_stands_by_loc[[loc]]
  cycleat <- r6_stands |>
    filter(STAND_ID %in% sts) |>
    pull(cycleat)
  kwd <- rFVSIEtools::write_multistand_key(STDIDENTs = sts, 
                                           out_dir = 'fvs_kwd_files',
                                           out_db = 'FVSUncalib.db',
                                           database = file.path('data', 'fvs_ready.db'),
                                           file_prefix = paste0('loc_', loc),
                                           calibrate = FALSE,
                                           triple = FALSE,
                                           add_regen = FALSE,
                                           n_years = 21,
                                           CYCLEAT = cycleat)
  rFVSIEtools::run_FVS(fvs_bin = fvs_bin, variant = 'ie', keyword_file = kwd)
  prog <- prog+1
  setTxtProgressBar(pb, prog)
}
close(pb)

view_errs(fvs_outdb, r6_stands)

## Region 4 ----
r4_stands <- filter(fvs_stand_init, REGION == 4)
r4_stands_by_loc <- split(r4_stands$STAND_ID, r4_stands$LOCATION)
prog <- 0
pb <- txtProgressBar(min = 0, max = length(names(r4_stands_by_loc)), style = 3)
for(loc in names(r4_stands_by_loc)){
  sts <- r4_stands_by_loc[[loc]]
  cycleat <- r4_stands |>
    filter(STAND_ID %in% sts) |>
    pull(cycleat)
  kwd <- rFVSIEtools::write_multistand_key(STDIDENTs = sts, 
                                           out_dir = 'fvs_kwd_files',
                                           out_db = 'FVSUncalib.db',
                                           database = file.path('data', 'fvs_ready.db'),
                                           file_prefix = paste0('loc_', loc),
                                           calibrate = FALSE,
                                           triple = FALSE,
                                           add_regen = FALSE,
                                           n_years = 21,
                                           CYCLEAT = cycleat)
  rFVSIEtools::run_FVS(fvs_bin = fvs_bin, variant = 'ie', keyword_file = kwd)
  prog <- prog+1
  setTxtProgressBar(pb, prog)
}
close(pb)
view_errs(fvs_outdb, r4_stands)
