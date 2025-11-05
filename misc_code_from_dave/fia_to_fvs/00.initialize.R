# libraries
library( DBI )

#library(rFVS)
library("rFVS", lib.loc = 'C:\\FVS\\FVSSoftware\\R\\R-4.5.0\\library')



# paths & files
fia_path <- file.path("C:","DLRA","FIAdata")
fia_sql <- "SQLite_FIADB_ID.db"

fvs_path <- file.path("C:","FVS","FVSSoftware","FVSbin")


# function for getting tree lists from FVS
fetchTrees <- function(){
  tree_list <- fvsGetTreeAttrs(c("id","plot","age","species","dbh","ht","cratio","tpa","mcuft","bdft"))
  tree_list$year <- fvsGetEventMonitorVariables("year")
  tree_list
}
