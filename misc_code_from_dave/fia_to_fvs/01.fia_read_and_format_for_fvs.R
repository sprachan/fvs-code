# functions
source("00.initialize.R")
source("01a.fia_data_formatting_for_fvs.R")


# 1 open FIADB connection
fia_db <- file.path(fia_path,fia_sql)
fia.con <- dbConnect( RSQLite::SQLite(), fia_db ) 

# 1a get FIA database version just fyi
fia.vers <- dbReadTable(fia.con,"REF_FIADB_VERSION")
fia.vers[order(fia.vers$VERSION,decreasing=T),][1,]

# 1b read a subset of the data, here determined by land condition
#    -for example, to two particular counties (Shoshone and Clearwater),
#       to state gov't land, to forest land, and to plots not chopped up 
#       into many different conditions
fia_subset <- dbGetQuery(fia.con, 
                     "select * from COND WHERE COUNTYCD IN ( 35,79 )" ) 
fia_subset <- fia_subset[fia_subset$COND_STATUS_CD==1,] # subset accessible forest land
fia_subset <- fia_subset[fia_subset$OWNGRPCD==30,] # subset to state and local gov't land
fia_subset <- fia_subset[fia_subset$CONDPROP_UNADJ>=.75,] # subset to cases where the condition makes
                                                        #  up more 75% of the total plot area
fia_subset <- fia_subset[fia_subset$INVYR>2001,] # only new annualized design plots
dim(fia_subset)

# 1c disconnect
dbDisconnect(fia.con)


# 2. get the plot and tree info for each of the conditions

FVS_StandInit <- FVS_TreeInit <- NULL
for( i in 1:10){ #nrow(fia_subset) ){
  next_plot <- get.fia(fia_db,fia_subset$PLT_CN[i]) 
  FVS_StandInit <- rbind(FVS_StandInit,next_plot[[1]])
  FVS_TreeInit <- rbind(FVS_TreeInit,next_plot[[2]])
}
nrow(FVS_StandInit)

# 3. save fvs ready results
save(FVS_StandInit, FVS_TreeInit,file="fia_to_fvs.Rdata")
