
# FVS directories
fvs_dir <- file.path('C:', 'FVS', 'FVSSoftware')
fvs_path <- file.path(fvs_dir, 'FVSbin')
fvs_rVer <- list.files(file.path(fvs_dir, 'R')) # R version used for naming dirs
rFVS_dir <- file.path(fvs_dir, 'R', fvs_rVer, 'library') # rFVS location

# packages
library( DBI )

#library(rFVS)
library("rFVS", lib.loc = rFVS_dir)



# paths & files

fia_path <- file.path('..', '..', 'data', 'fia')
fia_sql <- file.path(fia_path, 'SQLite_FIADB_MT.db')


# function for getting tree lists from FVS
fetchTrees <- function(){
  tree_list <- fvsGetTreeAttrs(c("id","plot","age","species","dbh","ht","cratio","tpa","mcuft","bdft"))
  tree_list$year <- fvsGetEventMonitorVariables("year")
  tree_list
}
