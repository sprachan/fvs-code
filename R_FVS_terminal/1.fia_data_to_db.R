# libraries
library(RSQLite)

# source data
fia_dir <- file.path("/media/affleck/SourisX/FIAdata")

# define some queries
cond_query <- paste0("SELECT * FROM COND WHERE ",
                     "COND_STATUS_CD IN (1) AND ",
                     "ADFORCD IN (203)")
plot_query <- paste0("SELECT * FROM PLOT WHERE ",
                     "KINDCD IN (1,2,3) AND ",
                     "DESIGNCD IN (1,501,502) AND ",
                     "P2PANEL IN (1,2,3,4,5) AND ",
                     "PLOT_STATUS_CD IN (1,2)")

# connect to FIA state db
con <- dbConnect(SQLite(),file.path(fia_dir,"SQLite_FIADB_SD.db"))

# print db version
dbver <- dbReadTable(con,'REF_FIADB_VERSION')
version_info <- dbver[order(dbver$CREATED_DATE,decreasing = TRUE),
                      c("VERSION","CREATED_DATE")][1,]
version_info

# list tables
dbListTables(con)

# get forested conditions
for_cond <- dbGetQuery(con,cond_query)

# get plot data for forested conditions
fiaplots <- dbGetQuery(con,plot_query)
fiaplots <- fiaplots[fiaplots$CN %in% unique(for_cond$PLT_CN),]

# subset conditions to those on the annualized plot design
for_cond_ann <- for_cond[for_cond$PLT_CN %in% fiaplots$CN,]

# extract fia ready tables
fvs_stand <- dbReadTable(con,"FVS_STANDINIT_COND")
fvs_stand <- fvs_stand[fvs_stand$STAND_CN %in% unique(for_cond_ann$CN),]

fvs_tree <- dbReadTable(con,"FVS_TREEINIT_COND")
fvs_tree <- fvs_tree[fvs_tree$STAND_CN %in% unique(fvs_stand$STAND_CN),]

# Disconnect from FIA database
dbDisconnect(con)


# add whole plot cn to the fvs_stand table
fvs_stand <- merge(fvs_stand,
                   for_cond_ann[,c("CN","PLT_CN","STATECD","COUNTYCD","PLOT",
                             "CONDID","CONDPROP_UNADJ","MICRPROP_UNADJ",
                             "SUBPPROP_UNADJ",
                             "DSTRBCD1","DSTRBCD2","DSTRBCD3",
                             "DSTRBYR1","DSTRBYR2","DSTRBYR3")],
                   by.x="STAND_CN",by.y="CN")

# add whole plot intensity and panel info
fvs_stand <- merge(fvs_stand,
                   fiaplots[,c("CN","P2PANEL","SUBPANEL","INTENSITY",
                               "DESIGNCD",
                               "KINDCD","INVYR","MEASYEAR")],
                   by.x="PLT_CN",by.y="CN")
names(fvs_stand)[names(fvs_stand)=="INVYR"] <- "fia_INVYR"


# remap full plots to single plot
# use plot fraction as the sampling weight
fvs_stand$SAM_WT <- ifelse(fvs_stand$NUM_PLOTS==4,1,fvs_stand$CONDPROP_UNADJ)
# idetnify whole plots
full_plots <- unique(fvs_stand$STAND_ID[fvs_stand$NUM_PLOTS==4])
# correct the tree count for an error on the SD FIA dataset
for (full_plot in full_plots){
  # change plot ids on tree to 1
  fvs_tree$PLOT_ID[fvs_tree$STAND_ID==full_plot] <- 1
  # calculate tree expansion factors
  fvs_tree$TREE_COUNT[fvs_tree$STAND_ID==full_plot] <- 43560/(pi*4*
                                                                fvs_tree$TREE_COUNT[fvs_tree$STAND_ID==full_plot]*
                                                                ifelse(fvs_tree$DIAMETER[fvs_tree$STAND_ID==full_plot]>=5,
                                                                       24,6.8)^2)
  # update design info on stand table
  fvs_stand$NUM_PLOTS[fvs_stand$STAND_ID==full_plot] <- 1
  fvs_stand$BRK_DBH[fvs_stand$STAND_ID==full_plot] <- 999
  fvs_stand$BASAL_AREA_FACTOR[fvs_stand$STAND_ID==full_plot] <- 0
  fvs_stand$INV_PLOT_SIZE[fvs_stand$STAND_ID==full_plot] <- 1
}


# look for cases of no trees
num_live <- aggregate(fvs_tree$TREE_ID[fvs_tree$HISTORY<6],
                      by=list(STAND_ID=fvs_tree$STAND_ID[fvs_tree$HISTORY<6]),
                      length)
names(num_live)[2] <- "num_live"
fvs_stand <- merge(fvs_stand,num_live,all.x=T)
fvs_stand$num_live <- ifelse(is.na(fvs_stand$num_live),
                             0,fvs_stand$num_live)



# save all
FVS_Stand <- fvs_stand
FVS_Tree <- fvs_tree
save(FVS_Stand,
     FVS_Tree,
     file=file.path("temp","bhnf_FIA_FVS.Rdata"))

fia_out <- RSQLite::dbConnect(RSQLite::SQLite(), 
                              file.path("temp","bhnf_FIA_FVS.db"))
dbWriteTable(fia_out,"FVS_STANDINIT",FVS_Stand)
dbWriteTable(fia_out,"FVS_TREEINIT",FVS_Tree)
dbDisconnect(fia_out)
