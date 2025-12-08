# DESCRIPTION ------------------------------------------------------------------
#>
#> Run three projections growing Lubrecht trees from 2018 to 2023:
#> (1) Project with calibration turned off
#> (2) Project with self-calibration (calibration on)
#> (32) Project with FIA-derived multipliers
#> 
#-------------------------------------------------------------------------------

# Packages and functions -------------------------------------------------------
library(here) # makes file paths much easier

source(here('scripts', '00_functions.R'))

# File paths -------------------------------------------------------------------
## FVS directories ----
fvs_dir <- file.path('C:', 'FVS', 'FVSSoftware')
fvs_bin <- file.path(fvs_dir, 'FVSbin')
fvs_rVer <- list.files(file.path(fvs_dir, 'R')) # R version used for naming dirs
rFVS_dir <- file.path(fvs_dir, 'R', fvs_rVer, 'library') # rFVS location

library('rFVS', lib.loc = rFVS_dir) # rFVS

## Data directories and files ----
fvs_ready <- here('data', 'fvs_ready')
fvs_runs <- here('data', 'fvs_runFiles')
sim_out <- here('data', 'sim_outputs')

lub_standlist <- readxl::read_xlsx(here('data', 'fvs_ready', 
                                        'FVS_Lubrecht_2018.xlsx'),
                                   sheet = 'FVS_StandInit') |>
  dplyr::filter(substr(STAND_ID, 1, 4) == 'CARB') |>
  dplyr::mutate(TOPO = '')
  

lub_treelist <- readxl::read_xlsx(here('data', 'fvs_ready', 
                                       'FVS_Lubrecht_2018.xlsx'),
                                  sheet = 'FVS_TreeInit') |>
  dplyr::filter(STAND_ID %in% lub_standlist$STAND_ID) |>
  dplyr::rename(TOPO = TOPOCODE) |>
  dplyr::mutate(FIA_SPECIES = SPECIES)

# Get FIA multipliers ----------------------------------------------------------
species_tab <- fvs_spcd()

calib_stats <- readRDS(file.path(sim_out, 'lubcalib_calbstat.RData'))
str(calib_stats)
mults <- dplyr::select(calib_stats, 
                       TreeSize, 
                       SpeciesPLANTS, 
                       SpeciesFIA,
                       NumTrees, 
                       ScaleFactor, 
                       ReadCorMult) |>
  dplyr::summarize(.by = c(TreeSize, SpeciesPLANTS, SpeciesFIA),
                   n = sum(NumTrees),
                   avgScaleFactor = mean(ScaleFactor, na.rm = TRUE),
                   avgReadCorMult = mean(ReadCorMult, na.rm = TRUE))

# Separate large and small tree multipliers
ltdg_mults <- dplyr::filter(mults, TreeSize == 'LG') |>
  # use full join to get EVERY species in FVS IE, regardless of if they're
  #> in FIA calib data or not
  dplyr::full_join(dplyr::select(species_tab, fvs_cd, sp_abb),
                   by = dplyr::join_by(SpeciesPLANTS == sp_abb)) |>
  dplyr::arrange(fvs_cd) |>
  dplyr::mutate(READCORD = stringr::str_pad(
    ifelse(is.na(avgReadCorMult),
           # default multiplier is 1.0
           '1.00', 
           round(avgReadCorMult, 2)),
    width = 10,
    pad = ' ',
    side = 'left')
  )

sthg_mults <- dplyr::filter(mults, TreeSize == 'SM') |>
  dplyr::full_join(dplyr::select(species_tab, fvs_cd, sp_abb),
                   by = dplyr::join_by(SpeciesPLANTS == sp_abb)) |>
  dplyr::arrange(fvs_cd) |>
  dplyr::mutate(READCORR = stringr::str_pad(
    ifelse(is.na(avgReadCorMult)|avgReadCorMult == 1,
           # default multiplier is 1.0
           '1.00', 
           round(avgReadCorMult, 2)),
    width = 10,
    pad = ' ',
    side = 'left')
  )

# 1. Uncalibrated projection ---------------------------------------------------

# make directory. Use date and time to help with ID later
dt <- paste0('lub_uc_', format(Sys.Date(), '%b%d%Y'), '_', 
             format(Sys.time(), '%H.%M'), collapse = '')
outdir <- file.path(fvs_runs, dt)
dir.create(outdir)
filenames <- rep(NA, nrow(lub_standlist))

# projection parameters
num_years <- 5 # number of years into the future to project
use_tripling <- FALSE
use_calibration <- FALSE # turn off calibration
use_regenmodel <- FALSE

# FVS will  overwrite each stand, so we need storage
lub_uncalib <- list()
lub_uncalib$treelist <- NULL
lub_uncalib$summary <- NULL

for(i in 1:nrow(lub_standlist)){
  st <- lub_standlist[i,]
  tr <- lub_treelist[lub_treelist$STAND_ID == st$STAND_ID,] |>
    as.data.frame()
  if(sum(tr$HISTORY %in% 6:9) == nrow(tr)){
    cat('Skipping stand ', st$STAND_ID, '\n', sep = '')
  }else{
    tr$fvs.TREE_ID <- 1:nrow(tr)
    rFVS::fvsLoad("FVSie", fvs_bin)
    filenames[i] <- write.FVSfiles(trees=tr,
                                   stand=st,
                                   years_out=num_years,
                                   calibrate=use_calibration,
                                   triple=use_tripling,
                                   add_regen=use_regenmodel,
                                   file_prefix = paste0('lub_uncalib', i),
                                   outdir = outdir,
                                   STDIDENT = paste0('LUBUNC', i))
    
    # grow the stand
    fvsSetCmdLine(paste0("--keywordfile=",filenames[i],".key"))
    
    # get grown tree list
    fvs_output <- fvsInteractRun(AfterEM1='fetchTrees()',
                                 SimEnd=fvsGetSummary)
    names(fvs_output)
    
    # FIRST tree list in element 1
    tl1 <- fvs_output[[1]]$AfterEM1 |>
      dplyr::left_join(dplyr::select(tr, fvs.TREE_ID, TREE_ID, STAND_ID, FIA_SPECIES),
                       by = dplyr::join_by(id == fvs.TREE_ID))
    nrow(tl1)
    
    # REMEASURE tree list output in element 2
    tl2 <- fvs_output[[2]]$AfterEM1 |>
      dplyr::left_join(dplyr::select(tr, fvs.TREE_ID, TREE_ID, STAND_ID, FIA_SPECIES),
                       by = dplyr::join_by(id == fvs.TREE_ID))
    nrow(tl2)
    tl <- rbind(tl1, tl2)
    nrow(tl)
    
    
    lub_uncalib$treelist <- rbind(lub_uncalib$treelist, tl)
    
    # similar with plot summaries
    lub_uncalib$summary <- rbind(lub_uncalib$summary,
                                  cbind(fvs_output[[length(fvs_output)]],
                                        data.frame(STAND_ID = st$STAND_ID)))
    file.remove('FVSOut.db')
  }
  
  
  rFVS::fvsLoad("FVSie", fvs_bin)
}

# 2. Self-calibrated projection ------------------------------------------------

# make directory. Use date and time to help with ID later
dt <- paste0('lub_sc_', format(Sys.Date(), '%b%d%Y'), '_', 
             format(Sys.time(), '%H.%M'), collapse = '')
outdir <- file.path(fvs_runs, dt)
dir.create(outdir)
filenames <- rep(NA, nrow(lub_standlist))

# projection parameters
use_calibration <- TRUE # turn ON calibration

# FVS will  overwrite each stand, so we need storage
lub_selfcalib <- list()
lub_selfcalib$treelist <- NULL
lub_selfcalib$summary <- NULL

for(i in 1:nrow(lub_standlist)){
  st <- lub_standlist[i,]
  tr <- lub_treelist[lub_treelist$STAND_ID == st$STAND_ID,] |>
    as.data.frame()
  if(sum(tr$HISTORY %in% 6:9) == nrow(tr)){
    cat('Skipping stand ', st$STAND_ID, '\n', sep = '')
  }else{
    tr$fvs.TREE_ID <- 1:nrow(tr)
    rFVS::fvsLoad("FVSie", fvs_bin)
    filenames[i] <- write.FVSfiles(trees=tr,
                                   stand=st,
                                   years_out=num_years,
                                   calibrate=use_calibration,
                                   triple=use_tripling,
                                   add_regen=use_regenmodel,
                                   file_prefix = paste0('lubselfcalib_stand', i),
                                   outdir = outdir,
                                   STDIDENT = paste0('LUBSCALB', i))
    
    # grow the stand
    fvsSetCmdLine(paste0("--keywordfile=",filenames[i],".key"))
    
    # get grown tree list
    fvs_output <- fvsInteractRun(AfterEM1='fetchTrees()',
                                 SimEnd=fvsGetSummary)
    names(fvs_output)
    
    # FIRST tree list in element 1, just to check
    tl1 <- fvs_output[[1]]$AfterEM1 |>
      dplyr::left_join(dplyr::select(tr, fvs.TREE_ID, TREE_ID, STAND_ID, FIA_SPECIES),
                       by = dplyr::join_by(id == fvs.TREE_ID))
    nrow(tl1)
    
    # REMEASURE tree list output in element 2. use left_join to match up with TUID
    tl2 <- fvs_output[[2]]$AfterEM1 |>
      dplyr::left_join(dplyr::select(tr, fvs.TREE_ID, TREE_ID, STAND_ID, FIA_SPECIES),
                       by = dplyr::join_by(id == fvs.TREE_ID))
    nrow(tl2)
    tl <- rbind(tl1, tl2)
    nrow(tl)
    
    
    lub_selfcalib$treelist <- rbind(lub_selfcalib$treelist, tl)
    
    # similar with plot summaries
    lub_selfcalib$summary <- rbind(lub_selfcalib$summary,
                                    cbind(fvs_output[[length(fvs_output)]],
                                          data.frame(STAND_ID = st$STAND_ID)))
    # Get calibration statistics
    conn <- DBI::dbConnect(RSQLite::SQLite(), 'FVSOut.db')
    calib <- dplyr::tbl(conn, 'FVS_CalibStats') |>
      dplyr::collect()
    
    DBI::dbDisconnect(conn)
    
    lub_selfcalib$calibstats <- rbind(lub_selfcalib$calibstats, calib)
    remove(conn)
    file.remove('FVSOut.db')
  }
  
  
  rFVS::fvsLoad("FVSie", fvs_bin)
}


self_mults <- dplyr::select(lub_selfcalib$calibstats, 
                       TreeSize, 
                       SpeciesPLANTS, 
                       SpeciesFIA,
                       NumTrees, 
                       ScaleFactor, 
                       ReadCorMult) |>
  dplyr::summarize(.by = c(TreeSize, SpeciesPLANTS, SpeciesFIA),
                   n = sum(NumTrees),
                   avgScaleFactor = mean(ScaleFactor, na.rm = TRUE),
                   avgReadCorMult = mean(ReadCorMult, na.rm = TRUE))


# 3. FIA-calibrated projection -------------------------------------------------
# make directory. Use date and time to help with ID later
dt <- paste0('lub_fiacalib_', format(Sys.Date(), '%b%d%Y'), '_', 
             format(Sys.time(), '%H.%M'), collapse = '')
outdir <- file.path(fvs_runs, dt)
dir.create(outdir)
filenames <- rep(NA, nrow(lub_standlist))

# projection parameters
use_calibration <- FALSE # turn off calibration

# FVS will  overwrite each stand, so we need storage
lub_fiacalib <- list()
lub_fiacalib$treelist <- NULL
lub_fiacalib$summary <- NULL

for(i in 1:nrow(lub_standlist)){
  st <- lub_standlist[i,]
  tr <- lub_treelist[lub_treelist$STAND_ID == st$STAND_ID,] |>
    as.data.frame()
  if(sum(tr$HISTORY %in% 6:9) == nrow(tr)){
    cat('Skipping stand ', st$STAND_ID, '\n', sep = '')
  }else{
    tr$fvs.TREE_ID <- 1:nrow(tr)
    rFVS::fvsLoad("FVSie", fvs_bin)
    filenames[i] <- write.FVSfiles(trees=tr,
                                   stand=st,
                                   years_out=num_years,
                                   calibrate=use_calibration,
                                   triple=use_tripling,
                                   add_regen=use_regenmodel,
                                   file_prefix = paste0('lub_fiacalib', i),
                                   outdir = outdir,
                                   STDIDENT = paste0('LUBFIA', i),
                                   READCORD = ltdg_mults$READCORD, # large tree
                                   READCORR = sthg_mults$READCORR) # small tree
    
    # grow the stand
    fvsSetCmdLine(paste0("--keywordfile=",filenames[i],".key"))
    
    # get grown tree list
    fvs_output <- fvsInteractRun(AfterEM1='fetchTrees()',
                                 SimEnd=fvsGetSummary)
    names(fvs_output)
    
    # FIRST tree list in element 1
    tl1 <- fvs_output[[1]]$AfterEM1 |>
      dplyr::left_join(dplyr::select(tr, fvs.TREE_ID, TREE_ID, STAND_ID, FIA_SPECIES),
                       by = dplyr::join_by(id == fvs.TREE_ID))
    nrow(tl1)
    
    # REMEASURE tree list output in element 2
    tl2 <- fvs_output[[2]]$AfterEM1 |>
      dplyr::left_join(dplyr::select(tr, fvs.TREE_ID, TREE_ID, STAND_ID, FIA_SPECIES),
                       by = dplyr::join_by(id == fvs.TREE_ID))
    nrow(tl2)
    tl <- rbind(tl1, tl2)
    nrow(tl)
    
    
    lub_fiacalib$treelist <- rbind(lub_fiacalib$treelist, tl)
    
    # similar with plot summaries
    lub_fiacalib$summary <- rbind(lub_fiacalib$summary,
                                 cbind(fvs_output[[length(fvs_output)]],
                                       data.frame(STAND_ID = st$STAND_ID)))
    file.remove('FVSOut.db')
  }
  
  
  rFVS::fvsLoad("FVSie", fvs_bin)
}

# Save out all results ---------------------------------------------------------
# Uncalibrated run
saveRDS(lub_uncalib$treelist, 
        file.path(sim_out, 'lubrecht', 'lub_uncalib_trees.RData'))
saveRDS(lub_uncalib$summary, 
        file.path(sim_out, 'lubrecht', 'lub_uncalib_summary.RData'))

# Self calibration run
saveRDS(lub_selfcalib$treelist, 
        file.path(sim_out, 'lubrecht', 'lub_selfcalib_trees.RData'))
saveRDS(lub_selfcalib$summary, 
        file.path(sim_out, 'lubrecht', 'lub_selfcalib_summary.RData'))
saveRDS(lub_selfcalib$calibstats,
        file.path(sim_out, 'lubrecht', 'lub_selfcalib_calbstat.RData'))
saveRDS(self_mults,
        file.path(sim_out, 'lubrecht', 'self_multipliers.RData'))

# FIA calibration run
saveRDS(lub_fiacalib$treelist, 
        file.path(sim_out, 'lubrecht', 'lub_fiacalib_trees.RData'))
saveRDS(lub_fiacalib$summary, 
        file.path(sim_out, 'lubrecht', 'lub_fiacalib_summary.RData'))
saveRDS(mults, file.path(sim_out, 'lubrecht', 'fia_multipliers.RData'))
