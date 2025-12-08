# FIA2FVS formatting -----------------------------------------------------------
#>
#> Description: Get plot and tree info for requested conditions and save as
#> .RDS file that can be loaded into a new session. This is run once 
#> (or as needed)
#>
#-------------------------------------------------------------------------------

source('01_setup.R') # directories and packages
source('00_functions.R') # functions
library(ggplot2)
library(patchwork)

fia_conn <- DBI::dbConnect(RSQLite::SQLite(), fia_sql)

# FYI: FIA database version
fia_vers <- dplyr::tbl(fia_conn, 'REF_FIADB_VERSION') |>
  dplyr::arrange(desc(VERSION))

head(fia_vers, 3)

# Subset the FIA data ----
fia_subset <- fia_conn |>
  dplyr::tbl('COND') |> # condition table: tells us condition
  dplyr::filter(COND_STATUS_CD == 1, # accessible forest land only
                CONDPROP_UNADJ >= 0.75, # at least 75% of the plot is in condition X
                INVYR > 2001, # plots that follow 
                OWNGRPCD < 40) |>  # non-private land only
  dplyr::collect()

# Get plot and tree info for each condition ----
DBI::dbDisconnect(fia_conn)

# All plots 
FVS_standInitAll <- NULL
FVS_treeInitAll <- NULL
# use a progress bar because this takes a while
pb <- txtProgressBar(min = 1, max = nrow(fia_subset), initial = 1, style = 3)

for(i in 1:nrow(fia_subset)){
  plt <- get_FIA(fia_sql, fia_subset[i,])
  FVS_standInitAll <- rbind(FVS_standInitAll, plt[[1]])
  FVS_treeInitAll <- rbind(FVS_treeInitAll, plt[[2]])
  setTxtProgressBar(pb, i)
}
close(pb)

# generate unique plot identifier that is persistent year to year
FVS_standInitAll$PID <- paste0(FVS_standInitAll$STATECD,
                               # make sure counties are coded with 3 digits
                               stringr::str_pad(FVS_standInitAll$COUNTYCD,
                                                width = 3,
                                                pad = '0'),
                               FVS_standInitAll$UNITCD,
                               FVS_standInitAll$PLOT)

saveRDS(FVS_standInitAll,
        file = file.path('..', 'data', 'fvs_ready', 'FVSstandInitAll.rds'))

# use STAND_CN as a key to associate tree w/ PID. Give each tree
#> a unique identifier (TUID = TreeUniqueIDentifier) that is the same year to year.
#> Use plot identifier, subplot # (PLOT_ID), and tree # within that subplot --
#> this is a unique combination.

FVS_treeInitAll <- dplyr::select(FVS_standInitAll, PID, STAND_CN) |>
  dplyr::right_join(FVS_treeInitAll, by = dplyr::join_by(STAND_CN)) |>
  dplyr::mutate(TUID = paste0(PID, PLOT_ID, TREE_ID))
  

saveRDS(FVS_treeInitAll,
        file = file.path('..', 'data', 'fvs_ready', 'FVStreeInitAll.rds'))
