# Functions --------------------------------------------------------------------
#>
#> Description: standalone script that contains functions used for FVS
#> calibration scripts.
#-------------------------------------------------------------------------------

# Accessing FIA data ----
#> Arguments
#> - db_loc: character vector. File path where FIA database (.db) is located
#> - pcn: plot control number. String.
get_FIA <- function(db_loc, pcn){
  
  fiadb_conn <- DBI::dbConnect(RSQLite::SQLite(), db_loc) 
  pcn <- as.character(pcn)
  
  # construct and execute SQL query using tidyverse-style piping
  stand_initQ <- dplyr::tbl(fiadb_conn, 'FVS_STANDINIT_PLOT')|> 
    dplyr::filter(STAND_CN == pcn) |>
    dplyr::select(STAND_CN,
                  STAND_ID,
                  VARIANT,
                  INV_DAY,
                  INV_YEAR,
                  INV_MONTH,
                  LATITUDE,
                  LONGITUDE,
                  REGION,
                  FOREST,
                  PV_CODE,
                  ECOREGION,
                  BASAL_AREA_FACTOR,
                  INV_PLOT_SIZE,
                  BRK_DBH,
                  AGE,
                  ASPECT,
                  SLOPE,
                  TOPO,
                  ELEVFT,
                  NUM_PLOTS,
                  MAX_SDI,
                  DG_TRANS,
                  DG_MEASURE,
                  HTG_TRANS,
                  HTG_MEASURE,
                  MORT_MEASURE,
                  SITE_SPECIES,
                  SITE_INDEX,
                  STATE,
                  COUNTY)
  message('SQL query:', dplyr::show_query(stand_initQ))
  
  stand_init <- dplyr::collect(stand_initQ) # execute query

  # did we return anything?
  if(nrow(stand_init) == 0){
    warning('No matching stands found, returning NULL')
    return(NULL)
  }else{
    # adjust growth  year. If later than July, add 1 (b/c in next growth year)
    stand_init$INV_YEAR <- stand_init$INV_YEAR + ifelse(stand_init$INV_MONTH >= 7, 
                                                        yes = 1, 
                                                        no = 0 )
    # get tree info
    fia_treeQ <- dplyr::tbl(fiadb_conn, 'FVS_TREEINIT_PLOT') |>
      dplyr::filter(STAND_CN == pcn) |>
      dplyr::select(STAND_CN,
                    STAND_ID,
                    STANDPLOT_ID,
                    PLOT_ID,
                    TREE_ID,
                    HISTORY,
                    TREE_COUNT,
                    SPECIES,
                    DIAMETER,
                    DG,
                    HT,
                    HTTOPK,
                    HTG,
                    HT_TO_CROWN_BASE,
                    CRRATIO,
                    DEFECT_CUBIC,
                    DEFECT_BOARD,
                    DAMAGE1,
                    SEVERITY1,
                    DAMAGE2,
                    SEVERITY2,
                    DAMAGE3,
                    SEVERITY3,
                    AGE,
                    BH_YEARS)
    message('SQL query to get tree info: ', dplyr::show_query(fia_treeQ))
    fia_tree <- dplyr::collect(fia_treeQ)
    fia_tree$SPECIES <- as.numeric(fia_tree$SPECIES)
    
    if(nrow(fia_tree) == 0){
      warning('No matching tree info found, returning NULL')
      return(NULL)
    }
    
    out <- list(FVS_StandInit = stand_init,
                FVS_TreeInit = fia_tree)
    return(out)
  }
  DBI::dbDisconnect(fia_db_conn)
}

# Accessing FVS data ----
# function for getting tree lists from FVS
fetch_trees <- function(){
  tree_list <- rFVS::fvsGetTreeAttrs(c("id",
                                       "plot",
                                       "age",
                                       "species",
                                       "dbh",
                                       "ht",
                                       "cratio",
                                       "tpa",
                                       "mcuft",
                                       "bdft"))
  tree_list$year <- rFVS::fvsGetEventMonitorVariables("year")
  return(tree_list)
}
