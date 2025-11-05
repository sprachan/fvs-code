
# retrieve a plot with tree list from FIA database.
# Returns a list with plot-level data (FVS_StandInit) and tree-level data (FVS_TreeInit)
get.fia <- function( db_loc, plot.condition.code ){
  #db_loc <- fia_db
  #plot.condition.code <- fia_subset$PLT_CN[1]
  
  fiadb.con <- dbConnect( RSQLite::SQLite(), db_loc ) 
  
  # 1 read one particular plot measurement 
  #    -need documentation, but I believe INV_YEAR/MONTH/DAY are derived from MEASYEAR, etc (not plot INVYR)
  plot.condition.code <- as.character( plot.condition.code )
  standinit <- dbGetQuery( fiadb.con,
                           paste( "select STAND_CN, STAND_ID, VARIANT, INV_DAY, INV_YEAR, INV_MONTH, ",
                                  "LATITUDE, LONGITUDE, REGION, FOREST,",
                                  "PV_CODE, ECOREGION, BASAL_AREA_FACTOR, INV_PLOT_SIZE, BRK_DBH,",
                                  "AGE, ASPECT, SLOPE, TOPO, ELEVFT, NUM_PLOTS, MAX_SDI,",
                                  "DG_TRANS, DG_MEASURE, HTG_TRANS, HTG_MEASURE,MORT_MEASURE,",
                                  "SITE_SPECIES, SITE_INDEX, STATE, COUNTY FROM", 
                                  "FVS_STANDINIT_PLOT where STAND_CN = ", plot.condition.code ))
  
  if( nrow(standinit) == 0 )
    return( NULL )

  #  note: can set INV_PLOT_SIZE to 1 acre (BRK_DBH=999) and just use TPA measures for each tree
  
  # 2 compute/adj growth year
  standinit$INV_YEAR <- standinit$INV_YEAR + ifelse( standinit$INV_MONTH >= 7, 1, 0 )
  
  # 3 get the tree info
  fia.tree <- dbGetQuery( fiadb.con,
                          paste( "select STAND_CN, STAND_ID, STANDPLOT_ID, PLOT_ID, TREE_ID, HISTORY,", 
                                 "TREE_COUNT, SPECIES, DIAMETER, DG, HT, HTTOPK, HTG, ",
                                 "HT_TO_CROWN_BASE, CRRATIO, DEFECT_CUBIC, DEFECT_BOARD, ",
                                 "DAMAGE1, SEVERITY1, DAMAGE2, SEVERITY2, DAMAGE3, SEVERITY3, ",
                                 "AGE, BH_YEARS FROM ",
                                 "FVS_TREEINIT_PLOT where STAND_CN = ", plot.condition.code ))
  fia.tree$SPECIES <- as.numeric(fia.tree$SPECIES)
  
  if( nrow(fia.tree) == 0 )
    return( NULL )
  
  # 4 close connection and output
  dbDisconnect(fiadb.con)
  
  result <- list( FVS_StandInit=standinit, FVS_TreeInit=fia.tree )
}
