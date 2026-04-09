# DESCRIPTION ==================================================================
#>
#> Purpose:
#> Extract FVS-ready data from locally-saved, state-level FIA databases.
#> 
#> Outputs:
#> Save data for future use as .rds files.
#> 
#> Notes:
#> Run once, or as often as new data are downloaded. 
#>
# ==============================================================================

library(rFVSIEtools)
library(here)
library(dplyr)

# Data directories and files
fia_path <- here('data', 'raw_data', 'fia')

# Subset the FIA data ----
fvs_ready <- list(MT = NULL, ID = NULL)
states <- c('MT', 'ID')

for(i in seq_along(states)){
  db_path <- here(fia_path, paste0('SQLite_FIADB_', states[i], '.db'))
  conn <- DBI::dbConnect(RSQLite::SQLite(),
                         db_path)
  cond_subset <- conn |>
    tbl('COND') |> # condition table: tells us condition
    filter(COND_STATUS_CD == 1, 
                  CONDPROP_UNADJ == 1, 
                  INVYR > 2001,  
                  DSTRBCD1 == 0,
                  TRTCD1 == 0) |>
    collect()
  DBI::dbDisconnect(conn)
  
  fvs_ready[[i]] <- get_FIA_state(db_path, cond_subset, add_identifier = TRUE)
}

# Process
FVS_standInit <- bind_rows(fvs_ready$MT$FVS_StandInit,
                           fvs_ready$ID$FVS_StandInit) |>
  arrange(PID, INV_YEAR) |>
  group_by(PID) |>
  mutate(REM_CD = seq(0, n()-1),
         N_REM = max(REM_CD),
         FOREST = stringr::str_pad(FOREST, 2, pad = '0', side = 'left')) |>
  ungroup()

FVS_treeInit <- bind_rows(fvs_ready$MT$FVS_TreeInit,
                                 fvs_ready$ID$FVS_TreeInit) |>
  left_join(FVS_standInit[c('PID', 'INV_YEAR', 'REM_CD',  'N_REM')],
            by = c('PID', 'INV_YEAR'))

# Save processed data
saveRDS(FVS_standInit, file = here('data', 'fvs_ready', 'FVS_StandInit_MTID.rds'))
saveRDS(FVS_treeInit, file = here('data', 'fvs_ready', 'FVS_TreeInit_MTID.rds'))
