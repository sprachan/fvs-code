# DESCRIPTION ==================================================================
#>
#> Purpose: Get and cache FIA data to save computation time in Quarto document.
#> 
#> Outputs: .rda file containing two dataframes: FVS_StandInit and FVS_TreeInit
#> 
#> Notes: No species processing, just filtering
#>
# ==============================================================================`
library(dplyr)
library(here)
library(rFVSIEtools)


fvs_ready <- list(MT = NULL, ID = NULL, WA = NULL) # pre-allocate storage
states <- c('MT', 'ID', 'WA') # states that we want data from

for(i in seq_along(states)){
  db_path <- here('data', 'raw_data', 'fia', paste0('SQLite_FIADB_', states[i], '.db'))
  # all subplots must be forested; only undisturbed/untreated plots
  #> that were sampled using the newer protocol (post 2001)
  fvs_ready[[i]] <- fetch_cond(db_path, 'COND_STATUS_CD == 1, INVYR >= 2001, CONDPROP_UNADJ == 1, DSTRBCD1 == 0, TRTCD1 == 0') |>
    get_FIA_state(db_path, fia_cond_subset = _, add_identifier = TRUE)
}

# Process
FVS_standInit <- bind_rows(fvs_ready$MT$FVS_StandInit,
                           fvs_ready$ID$FVS_StandInit,
                           fvs_ready$WA$FVS_StandInit) |>
  arrange(PID, INV_YEAR) |>
  group_by(PID) |>
  # FVS forest codes are in the "location" column of FIA FVS_StandInit table
  mutate(FOREST = LOCATION,
         REM_CD = seq(0, n()-1),
         N_REM = max(REM_CD),
         REM_YEAR = ifelse(N_REM == 1, max(INV_YEAR), INV_YEAR[2])) |>
  filter(VARIANT %in% c('IE', 'EM'), # need EM to cover our ecoregion
         N_REM >= 1,
         NUM_PLOTS == 4) |>
  ungroup()

FVS_treeInit <- bind_rows(fvs_ready$MT$FVS_TreeInit,
                          fvs_ready$ID$FVS_TreeInit,
                          fvs_ready$WA$FVS_TreeInit) |>
  filter(STAND_CN %in% FVS_standInit$STAND_CN,
         DIAMETER >= 3) |>
  left_join(FVS_standInit[c('STAND_CN', 'REM_CD', 'N_REM')])

save(FVS_standInit, FVS_treeInit, file = here('fia.rda'))