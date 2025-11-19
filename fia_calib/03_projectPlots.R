# projectPlots -----------------------------------------------------------------
#>
#> Description: Given stand and tree data, use FVS to project future conditions
#>
#-------------------------------------------------------------------------------
# Required scripts
source('00_functions.R')
source('01_setup.R')

# Data
standInit <- readRDS(file.path(fvs_ready, 'FVSstandInit.RDS'))
treeInit <- readRDS(file.path(fvs_ready, 'FVStreeInit.RDS'))

# Test on just one stand
testStand <- standInit[1,]
testTrees <- treeInit[treeInit$STAND_CN == testStand$STAND_CN,] |>
  clean_FIA_treeList(standinfo = testStand)
str(testStand)
str(testTrees)

# load the FVS variant
rFVS::fvsLoad("FVSie", fvs_bin)

# make directory for these .key and .tre files
dt <- paste0(format(Sys.Date(), '%b%d%Y'), '_', format(Sys.time(), '%H.%M'), collapse = '')
outdir <- file.path(fvs_runs, dt)
dir.create(outdir)

filename <- write_FVS_files(trees = testTrees, stand = testStand,
                            ncycles = 6, reporting_ints = 10,
                            calibrate = FALSE, triple = FALSE, add_regen = FALSE,
                            STDIDENT = 'FVS_Test',
                            out_dir = outdir)


# grow the stand
fvsSetCmdLine(paste0("--keywordfile=",filename,".key"))

# get grown tree list
fvs_output <- fvsInteractRun(AfterEM1="fetch_trees()",
                             SimEnd=fvsGetSummary) 
names(fvs_output)

# combine the tree lists and map the species
spp <- as.data.frame(fvsGetSpeciesCodes())
spp$spp_num <- 1:nrow(spp)

fvs_tree_list <- NULL 
for (j in 1:(length(fvs_output)-1)){
  new_trees <- fvs_output[[j]]$AfterEM1
  new_trees <- merge(new_trees,spp,
                     by.x="species",by.y="spp_num")
  fvs_tree_list <- rbind(fvs_tree_list,new_trees)
}

# basic summary output
stand_summary <- fvs_output[[length(fvs_output)]]
stand_summary

# end projection
fvsLoad( "FVSie", fvs_bin)
# output would be: stand_summary, fvs_tree_list
# end loop

