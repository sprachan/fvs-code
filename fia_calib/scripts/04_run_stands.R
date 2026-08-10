# DESCRIPTION ==================================================================
#>
#> Purpose: Run all (filtered) FIA conditions ("stands") through FVS to generate 
#> the dataset that will be used to fit the calibration model.
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
# run FVS in batches for ease of troubleshooting ----
fvs_bin <- 'C:/FVS/FVSSoftware/FVSbin'
conn <- DBI::dbConnect(RSQLite::SQLite(), file.path('data', 'fvs_ready.db'))
fvs_stand_init <- collect(tbl(conn, 'FVS_StandInit'))
DBI::dbDisconnect(conn)
# Reference tables ----
conn <- DBI::dbConnect(RSQLite::SQLite(), file.path('raw_data', 'fia', 'SQLite_FIADB_MT.db'))
ref_habtypes <- dplyr::tbl(conn, 'REF_HABTYP_DESCRIPTION') |>
  select(-CREATED_DATE, -MODIFIED_DATE) |>
  collect() |>
  filter(HABTYPCD %in% fvs_stand_init$PV_FIA_HABTYPCD1) 
DBI::dbDisconnect(conn)

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
  kwd <- rFVSIEtools::write_multistand_key(STDIDENTs = sts, out_dir = 'fvs_kwd_files',
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

out_conn <- DBI::dbConnect(RSQLite::SQLite(), 'fvs_kwd_files/FVSOut.db')
DBI::dbListTables(out_conn)
errs_r1 <- dplyr::tbl(out_conn, 'FVS_Error') |>
  collect()|>
  mutate(warncd = as.integer(substr(Message, 4, 5)),
         warn_type = case_when(warncd == 3 ~ 'ForestBounds',
                               warncd == 9 ~ 'PlotCounts',
                               warncd == 14|warncd>29&warncd<40 ~ 'HabType',
                               warncd == 41 ~ 'Stocking'))
DBI::dbDisconnect(out_conn)
sort(unique(errs_r1$Message))

ggplot(errs_r1)+geom_bar(aes(x = warn_type))

# Lewis and Clark NF (300 stands, NF 115) is out of IE bounds so uses St Joe's NF coefficients
fvs_stand_init |>
  inner_join(filter(errs_r1, warn_type == 'ForestBounds')['StandID'],
             by = join_by('STAND_ID' == 'StandID')) |>
  pull(LOCATION) |>
  unique()

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
  kwd <- rFVSIEtools::write_multistand_key(STDIDENTs = sts, out_dir = 'fvs_kwd_files',
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

out_conn <- DBI::dbConnect(RSQLite::SQLite(), 'fvs_kwd_files/FVSOut.db')
DBI::dbListTables(out_conn)
errs_r6 <- dplyr::tbl(out_conn, 'FVS_Error') |>
  collect()|>
  mutate(warncd = as.integer(substr(Message, 4, 5)),
         warn_type = case_when(warncd == 3 ~ 'ForestBounds',
                               warncd == 9 ~ 'PlotCounts',
                               warncd == 14|warncd>29&warncd<40 ~ 'HabType',
                               warncd == 41 ~ 'Stocking')) |>
  inner_join(r6_stands, by = join_by(StandID == STAND_ID))
DBI::dbDisconnect(out_conn)
sort(unique(errs_r6$Message))

ggplot(errs_r6)+geom_bar(aes(x = warn_type))

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
  kwd <- rFVSIEtools::write_multistand_key(STDIDENTs = sts, out_dir = 'fvs_kwd_files',
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

out_conn <- DBI::dbConnect(RSQLite::SQLite(), 'fvs_kwd_files/FVSOut.db')
DBI::dbListTables(out_conn)
errs_r4 <- dplyr::tbl(out_conn, 'FVS_Error') |>
  collect()|>
  mutate(warncd = as.integer(substr(Message, 4, 5)),
         warn_type = case_when(warncd == 3 ~ 'ForestBounds',
                               warncd == 9 ~ 'PlotCounts',
                               warncd == 14|warncd>29&warncd<40 ~ 'HabType',
                               warncd == 41 ~ 'Stocking')) |>
  inner_join(r4_stands, by = join_by(StandID == STAND_ID))
DBI::dbDisconnect(out_conn)
sort(unique(errs_r6$Message))

ggplot(errs_r6)+geom_bar(aes(x = warn_type))

