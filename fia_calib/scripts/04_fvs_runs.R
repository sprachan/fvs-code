# DESCRIPTION ==================================================================
#>
#> Purpose: Run all (filtered) FIA conditions ("stands") through FVS to generate 
#> the dataset that will be used to fit the calibration model.
#> 
#> Outputs: Database of FVS runs (fvs_kwd_files/FVSOut.db) and keyword files.
#> 
#> Notes: FVS Stand ID = CID; stands run from first to second measurement
#>
#> Data Sources: 
#> - FIA data from FIA datamart
#> 
# ==============================================================================
# Dependencies ----
library(dplyr)
library(ggplot2)

# Functions ----
run_by_loc <- function(regional_stands, stands_by_loc, out_dir, out_db, calibrate, ...){
  prog <- 0
  pb <- txtProgressBar(min = 0, max = length(names(stands_by_loc)), style = 3)
  for(loc in names(stands_by_loc)){
    sts <- sort(stands_by_loc[[loc]])
    cycleat <- regional_stands |>
      filter(STAND_ID %in% sts) |>
      arrange(STAND_ID) |>
      pull(cycleat)
    kwd <- rFVSIEtools::write_multistand_key(STDIDENTs = sts, 
                                             out_dir = out_dir,
                                             out_db = out_db,
                                             database = file.path('data', 'fvs_ready.db'),
                                             file_prefix = paste0('loc_', loc),
                                             calibrate = calibrate,
                                             triple = FALSE,
                                             add_regen = FALSE,
                                             n_years = 21,
                                             CYCLEAT = cycleat,
                                             ...)
    rFVSIEtools::run_FVS(fvs_bin = fvs_bin, variant = 'ie', keyword_file = kwd)
    prog <- prog+1
    setTxtProgressBar(pb, prog)
  }
  close(pb)
  
  rFVSIEtools::plot_FVS_messages(out_dir, out_db, regional_stands, verbose = TRUE)
}
# File paths and data ----
fvs_bin <- 'C:/FVS/FVSSoftware/FVSbin'
conn <- DBI::dbConnect(RSQLite::SQLite(), file.path('data', 'fvs_ready.db'))
fvs_stand_init <- collect(tbl(conn, 'FVS_StandInit')) |>
  group_by(CID) |>
  arrange(CID, INV_YEAR) |>
  mutate(REM_CD_COND = row_number(),
         cycleat = case_when(N_MEAS_COND == 1 ~ NA,
                             N_MEAS_COND == 2 ~ max(INV_YEAR),
                             N_MEAS_COND == 3 ~ INV_YEAR[2])) |>
  # only want to project from t1 -> t2
  filter(REM_CD_COND == 1) |>
    ungroup()
DBI::dbDisconnect(conn)
if(!dir.exists(file.path('fvs_kwd_files', 'uc'))) dir.create(file.path('fvs_kwd_files', 'uc'))
if(!dir.exists(file.path('fvs_kwd_files', 'sc'))) dir.create(file.path('fvs_kwd_files', 'sc'))

# Run FVS ----
regions <- c(1, 6, 4)
for(r in regions){
  regional_stands <- filter(fvs_stand_init, REGION == r, REM_CD == 1)
  stands_by_loc <- split(regional_stands$STAND_ID, regional_stands$LOCATION)
  run_by_loc(regional_stands = regional_stands,
             stands_by_loc = stands_by_loc,
             out_db = 'FVSUncalib.db', out_dir = file.path('fvs_kwd_files', 'uc'),
             calibrate = FALSE, random_seed = 2025)
  run_by_loc(regional_stands = regional_stands,
             stands_by_loc = stands_by_loc,
             out_db = 'FVSSelfCalib.db', out_dir = file.path('fvs_kwd_files', 'sc'),
             calibrate = TRUE, random_seed = 2025)
}

