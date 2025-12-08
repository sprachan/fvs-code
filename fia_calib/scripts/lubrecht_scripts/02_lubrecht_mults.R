# DESCRIPTION ------------------------------------------------------------------
#>
#> Use FVS to calculate diameter and height growth scale factors using FIA data
#> in Lolo NF ecoregions that overlap with Lubrecht.
#> 
#-------------------------------------------------------------------------------

# Packages and functions -------------------------------------------------------
library(here) # makes file paths much easier

source(here('src', '00_functions.R'))

# File paths -----------------------------------------------------
## FVS directories ----
fvs_dir <- file.path('C:', 'FVS', 'FVSSoftware')
fvs_bin <- file.path(fvs_dir, 'FVSbin')
fvs_rVer <- list.files(file.path(fvs_dir, 'R')) # R version used for naming dirs
rFVS_dir <- file.path(fvs_dir, 'R', fvs_rVer, 'library') # rFVS location

library('rFVS', lib.loc = rFVS_dir) # rFVS

## Data directories and files ----
fia_sql <- here('data', 'raw_data', 'SQLite_FIADB_MT.db')
fvs_ready <- here('data', 'fvs_ready')
fvs_runs <- here('data', 'fvs_runFiles')
sim_out <- here('data', 'sim_outputs')

# Get FIA data -----------------------------------------------------------------
# Lolo NF only and ecoregions that overlap with lubrecht. Take only stands
#> that have been remeasured once.
stand_init_lolo <- readRDS(file.path(fvs_ready, 'FVSstandInitAll.rds')) |>
  dplyr::filter(VARIANT == 'IE',
                FOREST == 16,
                ECOREGION %in% c('M332Bg', 'M332Bl'))|>
  dplyr::arrange(PID, INV_YEAR) |>
  dplyr::group_by(PID) |>
  dplyr::mutate(
    # REMeasurement code
    REM_CD = dplyr::case_when(INV_YEAR == min(INV_YEAR) ~ 0, # first measurement
                              dplyr::n() == 2 ~ 1, # first remeasurement
                              INV_YEAR == max(INV_YEAR) ~ 2, # second remeasurement
                              INV_YEAR != max(INV_YEAR) ~ 1, # first remeasurement
                              .default = NA),
    # Number of REMeasurements
    N_REM = max(REM_CD),
    # string padding for FVS compatibility
    FOREST = as.numeric(stringr::str_pad(FOREST, 
                              width = 2, 
                              pad = '0', 
                              side = 'left')),
    REM_INT = DG_MEASURE) |>
  dplyr::ungroup() |>
  dplyr::filter(N_REM == 1,
                REM_CD == 1) |>
  dplyr::arrange(PID)  |>
  as.data.frame()

tree_init_lolo <- readRDS(file.path(fvs_ready, 'FVStreeInitAll.rds')) |>
  dplyr::filter(STAND_CN %in% stand_init_lolo$STAND_CN) |>
  dplyr::left_join(dplyr::select(stand_init_lolo, STAND_CN, INV_YEAR, REM_CD, N_REM),
                   by = dplyr::join_by(STAND_CN)) |>
  clean_FIA_treeList(standinfo = stand_init_lolo) |>
  as.data.frame()

nrow(stand_init_lolo) # 70 stands
nrow(tree_init_lolo) # 2755 trees

# FVS run ----------------------------------------------------------------------
# make directory. Use date and time to help with ID later
dt <- paste0('lc_', format(Sys.Date(), '%b%d%Y'), '_', 
             format(Sys.time(), '%H.%M'), collapse = '')
outdir <- file.path(fvs_runs, dt)
dir.create(outdir)
filenames <- rep(NA, nrow(tree_init_lolo))

# projection parameters
num_years <- 11 # number of years into the future to project
use_tripling <- FALSE
use_calibration <- TRUE
use_regenmodel <- FALSE

# FVS will  overwrite each stand, so we need storage
selfcalib_proj <- list()
selfcalib_proj$treelist <- NULL
selfcalib_proj$summary <- NULL
selfcalib_proj$calibstats <- NULL

for(i in 1:nrow(stand_init_lolo)){
  st <- stand_init_lolo[i,]
  tr <- tree_init_lolo[tree_init_lolo$STAND_CN == st$STAND_CN,] |>
    as.data.frame()
  if(sum(tr$HISTORY %in% 6:9) == nrow(tr)){
    cat('Skipping stand ', st$STAND_CN, ' (PID ', st$PID, ') \n', sep = '')
  }else{
    tr$fvs.TREE_ID <- 1:nrow(tr)
    rFVS::fvsLoad("FVSie", fvs_bin)
    filenames[i] <- write.FVSfiles(trees=tr,
                                   stand=st,
                                   years_out=num_years,
                                   calibrate=use_calibration,
                                   triple=use_tripling,
                                   add_regen=use_regenmodel,
                                   file_prefix = paste0('lubcalib_stand', i),
                                   outdir = outdir,
                                   STDIDENT = paste0('LUBCALB', i))
    
    # grow the stand
    fvsSetCmdLine(paste0("--keywordfile=",filenames[i],".key"))

    # get grown tree list
    fvs_output <- fvsInteractRun(AfterEM1='fetchTrees()',
                                 SimEnd=fvsGetSummary)
    names(fvs_output)
    
    # FIRST tree list in element 1, just to check
    tl1 <- fvs_output[[1]]$AfterEM1 |>
      dplyr::left_join(dplyr::select(tr, fvs.TREE_ID, TUID, PID),
                       by = dplyr::join_by(id == fvs.TREE_ID))
    nrow(tl1)
    
    # REMEASURE tree list output in element 2. use left_join to match up with TUID
    tl2 <- fvs_output[[2]]$AfterEM1 |>
      dplyr::left_join(dplyr::select(tr, fvs.TREE_ID, TUID, PID),
                       by = dplyr::join_by(id == fvs.TREE_ID))
    nrow(tl2)
    tl <- rbind(tl1, tl2)
    nrow(tl)
    
    
    selfcalib_proj$treelist <- rbind(selfcalib_proj$treelist, tl)
    
    # similar with plot summaries
    selfcalib_proj$summary <- rbind(selfcalib_proj$summary,
                                    cbind(fvs_output[[length(fvs_output)]],
                                          data.frame(PID = st$PID)))
    # Get calibration statistics
    conn <- DBI::dbConnect(RSQLite::SQLite(), 'FVSOut.db')
    calib <- dplyr::tbl(conn, 'FVS_CalibStats') |>
      dplyr::collect()
    
    DBI::dbDisconnect(conn)
    
    selfcalib_proj$calibstats <- rbind(selfcalib_proj$calibstats, calib)
    remove(conn)
    file.remove('FVSOut.db')
  }
  
  
  rFVS::fvsLoad("FVSie", fvs_bin)
}

# Save outputs
saveRDS(selfcalib_proj$treelist,
        file = here(sim_out, 'lubcalib_trees.RData'))
saveRDS(selfcalib_proj$summary,
        file = here(sim_out, 'lubcalib_summary.RData'))
saveRDS(selfcalib_proj$calibstats,
        file = here(sim_out, 'lubcalib_calbstat.RData'))
