# functions
source("00.initialize.R")
source("02a.write_FVS_files.R")
load(file="fia_to_fvs.Rdata",verbose=T)

# projection parameters
num_years <- 55 # number of years into the future to project
use_tripling <- FALSE
use_calibration <- TRUE
use_regenmodel <- FALSE


# pick a particular plot, or loop over them
i <- 4
  standinit <- FVS_StandInit[i,]
  treeinit <- FVS_TreeInit[FVS_TreeInit$STAND_CN==standinit$STAND_CN,]

  # load the FVS variant
  fvsLoad( "FVSie", fvs_path )
  
  # assign tree numbers used by fvs for linking
  treeinit$fvs.TREE_ID <- 1:nrow(treeinit)
  
  # create FVS keyword and tree list input files
  filename <- write.FVSfiles(trees=treeinit,
                            stand=standinit,
                            years_out=num_years,
                            calibrate=use_calibration,
                            triple=use_tripling,
                            add_regen=use_regenmodel) #SDImax=SDImax,
  
  # grow the stand
  fvsSetCmdLine(paste0("--keywordfile=",filename,".key"))
  
  # get grown tree list
  fvs_output <- fvsInteractRun(AfterEM1="fetchTrees()",
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

  # end projection
  fvsLoad( "FVSie", fvs_path )
  
  # clean up temp files
  file.remove(paste0(filename, c(".key",".tre",".trl",".out") ))
  
  # output would be: stand_summary, fvs_tree_list
# end loop
  
  
stand_summary
head(fvs_tree_list)


# tripling turned off, so here we can link trees
treeinit
tree_input_vs_projected <- merge(treeinit[,c("PLOT_ID","TREE_ID","fvs.TREE_ID",
                                             "DIAMETER","SPECIES","HISTORY")],
                                 fvs_tree_list[fvs_tree_list$year==2006,],
                                 by.x="fvs.TREE_ID",by.y="id",all.x=T)
head(tree_input_vs_projected)


