# Functions --------------------------------------------------------------------
#>
#> Description: standalone script that contains functions used for FVS
#> calibration scripts.
#-------------------------------------------------------------------------------

# ===== Functions for dealing with FIA data ====================================
## get_FIA(): Accessing FIA data ----
#> Arguments
#> - db_loc: character vector. File path where FIA database (.db) is located
#> - verbose: print queries (TRUE) or no (FALSE)?
#> Modified w/ Gemini 3, prompt: How can I make this R code more efficient?

get_FIA <- function(db_loc, fia_condSubset, verbose = FALSE){
  
  fia_db_conn <- DBI::dbConnect(RSQLite::SQLite(), db_loc) 
  
  # from Gemini: safer way to make sure database is disconnected
  on.exit(DBI::dbDisconnect(fia_db_conn))
  
  # from Gemini: use a temporary table in SQLite instead of massive 'IN'
  #> clause to improve performance.
  
  pcn_remote <- dplyr::copy_to(dest = fia_db_conn,
                               df = dplyr::select(fia_condSubset, PLT_CN,
                                                  COUNTYCD, UNITCD, PLOT),
                               name = 'temp_pcn',
                               overwrite = TRUE,
                               temporary = TRUE)
  
  # construct and execute SQL query using tidyverse-style piping
  # from Gemini: use inner_join with temporary remote table. It's faster than
  #> filtering by a large vector.
  stand_initQ <- dplyr::tbl(fia_db_conn, 'FVS_STANDINIT_PLOT')|> 
    dplyr::inner_join(pcn_remote, by = c('STAND_CN' = 'PLT_CN')) |> # replace filter()
    dplyr::select(STAND_CN, STAND_ID,
                  VARIANT, STATE, COUNTYCD, UNITCD, PLOT,
                  INV_DAY, INV_YEAR, INV_MONTH,
                  LATITUDE, LONGITUDE, REGION, FOREST,
                  PV_CODE, ECOREGION,
                  BASAL_AREA_FACTOR, INV_PLOT_SIZE,
                  BRK_DBH,
                  AGE,
                  ASPECT, SLOPE, TOPO, ELEVFT,
                  NUM_PLOTS,
                  MAX_SDI,
                  DG_TRANS, DG_MEASURE,
                  HTG_TRANS, HTG_MEASURE,
                  MORT_MEASURE,
                  SITE_SPECIES, SITE_INDEX)
  if(verbose){
    message('SQL query:', dplyr::show_query(stand_initQ))
  }
  
  stand_init <- dplyr::collect(stand_initQ)
  
  # did we return anything?
  if(nrow(stand_init) == 0){
    warning('No matching stands found, returning NULL')
    return(NULL)
  }else{
    # adjust growth  year. If later than July, add 1 (b/c in next growth year)
    stand_init$INV_YEAR <- stand_init$INV_YEAR + as.integer(stand_init$INV_MONTH >= 7)
    # get tree info
    fia_treeQ <- dplyr::tbl(fia_db_conn, 'FVS_TREEINIT_PLOT') |>
      dplyr::inner_join(pcn_remote, by = c('STAND_CN' = 'PLT_CN')) |>
      dplyr::select(STAND_CN, STAND_ID, STANDPLOT_ID, PLOT_ID, PLOT_CN,
                    TREE_ID, HISTORY, TREE_COUNT,
                    SPECIES,
                    DIAMETER, DG,
                    HT, HTTOPK, HTG, HT_TO_CROWN_BASE,
                    CRRATIO,
                    DEFECT_CUBIC, DEFECT_BOARD,
                    DAMAGE1, SEVERITY1, DAMAGE2, SEVERITY2, DAMAGE3, SEVERITY3,
                    AGE,
                    BH_YEARS)
    if(verbose){
      message('SQL query to get tree info: ', dplyr::show_query(fia_treeQ))
    }
    fia_tree <- dplyr::collect(fia_treeQ)
    fia_tree$SPECIES <- as.numeric(fia_tree$SPECIES)
    
    if(nrow(fia_tree) == 0){
      warning('No matching tree info found, returning NULL')
      return(NULL)
    }else{
      out <- list(FVS_StandInit = stand_init,
                  FVS_TreeInit = fia_tree)
      return(out) 
    }
  }
}


# ==== Functions for setting up FVS ============================================
## set_FVSie_defaults(): set missing information in a stand to IE defaults ----
#> Arguments
#> - stand: dataframe of stand information from FIA

set_FVSie_defaults <- function(stand){
  out <- stand |>
    dplyr::mutate(ASPECT = ifelse(is.na(ASPECT),
                                  yes = 0,
                                  no = ASPECT),
                  SLOPE = ifelse(is.na(SLOPE),
                                 yes = 5,
                                 no = SLOPE),
                  # need elevation in 100s of feet for FVS, FIA gives in ft
                  ELEVFT = ifelse(is.na(ELEVFT),
                                  yes = 38,
                                  no = ELEVFT/100), 
                  FOREST = ifelse(is.na(FOREST),
                                  yes = 18,
                                  no = FOREST)
                  )
  return(out)
}

## clean_FIA_treeList():  Clean an FIA tree list to prepare for FVS ----
#> Arguments
#> - standinfo: (row of a) dataframe of information for the stand 
#>   that will be modeled.
#> - treelist: tree list associated with the stand given in standinfo.

clean_FIA_treeList <- function(treelist, standinfo){
  # copy over information from the stand list
  treelist <- dplyr::select(standinfo,
                            SLOPE, ASPECT, PV_CODE, TOPO,
                            STAND_CN) |>
    dplyr::right_join(treelist, by = dplyr::join_by(STAND_CN))
  
  # add other values
  treelist$SPREP <- 0
  treelist$TVAL <- 0 # value of trees
  treelist$CUT <- 0 # cut list?
  
  # make crown ratio into 10% classes, per Essential FVS p. 41:
  #> 1: 0-10%; 2: 11-20%; ..., 9: 81-100%
  #> Because they say 0-10%, 11-20%, I assume that e.g., 10.5% counts as 10%...
  treelist$CRcode <- cut(treelist$CRRATIO, breaks = c(0, 11, 21, 31, 41, 51, 61, 71, 81, 100),
                         labels = FALSE,
                         right = FALSE,
                         include.lowest = TRUE)
  
  # if height to point of top-kill exists, use damage code 97 for dead top
  treelist$DAMAGE1 <- ifelse(!is.na(treelist$HTTOPK),
                             yes = 97,
                             no = 0) 
  # set all other damage and severity stuff to 0
  treelist$DAMAGE2 <- 0
  treelist$DAMAGE3 <- 0
  treelist$SEVERITY1 <- 0
  treelist$SEVERITY2 <- 0
  treelist$SEVERITY3 <- 0
  treelist$fvs.TREE_ID <- 1:nrow(treelist)
  return(treelist)
}


## str_padParam(): helper function for keyword formatting ----
#> Arguments
#> - string: the string to pad. The function adds spaces to the string so
#>   that it takes up 10 spaces total.
str_padParam <- function(string, right = FALSE){
  if(!right){
    out <- stringr::str_pad(string, 10, side = 'left')
  }else{
    out <- stringr::str_pad(string, 10, side = 'right')
  }
  
  return(out)
}


## from Dave: write.FVSfiles ----
#> Write FVS keyword and treelist files to use in projections.
write.FVSfiles <- function(  trees, stand, 
                             years_out=100, 
                             calibrate=TRUE,
                             triple=FALSE,
                             add_regen=FALSE,
                             customSDImax=NULL,
                             randomseed=2025,
                             outdir,
                             file_prefix = NULL,
                             STDIDENT = 'FVSProjection',
                             ...){
  
  #stand <- standinit
  #trees <- treeinit
  opt_args <- list(...)
  # Set variant defaults if stuff is missing
  stand$ASPECT <- ifelse( is.na(stand$ASPECT),  0, stand$ASPECT )        # aspect degrees
  stand$SLOPE <- ifelse( is.na(stand$SLOPE),    5, stand$SLOPE )         # slope percent
  stand$ELEVFT <- ifelse( is.na(stand$ELEVFT ), 38, stand$ELEVFT/100 ) # elevation
  stand$FOREST <- ifelse( is.na(stand$FOREST),  118, stand$FOREST) 
  
  ### generate file names
  if(is.null(file_prefix)){
    filename <- tempfile(tmpdir = outdir)
  }else{
    filename <- file.path(outdir, file_prefix)
  }
  
  keyfilename <- paste0( filename, ".key")
  treefilename <- paste0( filename, ".tre")
  
  ### write fvs .tre file
  write.fvs.tree.file( trees, stand, treefilename )    
  
  ###
  ### Keyword file creation
  ###
  
  ### stand identification
  write("STDIDENT", file=keyfilename, append=T)  
  write(STDIDENT,  file=keyfilename, append=T)  
  t1 <- sprintf("RANNSEED  %10.0f",randomseed)
  write(t1, file=keyfilename, append=T )
  
  t1 <- sprintf("STDINFO   %10s%10s%10.1f%10.1f%10.1f%10.0f",   
                stand$FOREST, stand$PV_CODE, stand$AGE, stand$ASPECT, stand$SLOPE, stand$ELEVFT )
  write( t1, file=keyfilename, append=T )   
  
  ### site index (not needed for FVSie)
  t1 <- sprintf("SITECODE  %10s%10i%10i", stand$SITE_SPECIES, as.integer(stand$SITE_INDEX+0.5), 1 )
  # write(t1, file=keyfilename, append=T)
  
  ### tree list output file (with no headers = column 3 = -1)
  t1 <- "TREELIST           0         3         0         0         0         0         0"    
  write( t1, file=keyfilename, append=T ) 
  
  ### tree list output file (with headers = column 3 = 0)
  t1 <- "TREEFMT"
  t2 <- "(I4,I4,F8.3,I1,A3,F5.1,F5.1,2F5.1,F5.1,I1,6I2,2I1,I2,2I3,2I1,F3.0)"   # use FIA species codes
  
  write( t1, file=keyfilename, append=T )           
  write( t2, file=keyfilename, append=T )          
  write( " ", file=keyfilename, append=T )       
  
  ### sample design
  t1 <- sprintf( "DESIGN          -1.0         0         0%10i         0         0       1.0", stand$NUM_PLOTS  )
  write( t1, file=keyfilename, append=T )           
  
  ### inventory year
  t1 <- sprintf("INVYEAR   %10i", stand$INV_YEAR )
  write( t1, file=keyfilename, append=T )            
  
  # variant default cycle length
  cycle.length <- 10
  cycles <- ceiling( years_out / cycle.length ) 
  
  ### add user supplied reporting years
  t1 <- sprintf("CYCLEAT   %10i", stand$INV_YEAR + years_out )
  write( t1, file=keyfilename, append=T )    
  
  ### bar tripling if necessary
  if (!triple){
    write("NOTRIPLE", file=keyfilename, append=T )
  }
  
  ### ingrowth(regeneration)
  if (!add_regen){
    write( "NOAUTOES", file=keyfilename, append=T)
  } else {
    write( "ESTAB", file=keyfilename, append=T )
    t1 <- sprintf("RANNSEED  %10.0f",randomseed)
    write(t1, file=keyfilename, append=T )
    write( "NOINGROWTH", file=keyfilename, append=T )   
    write( "END", file=keyfilename, append=T )
  }
  
  ### SDI maximum 
  if( !is.null(customSDImax) )
  {
    j <- sprintf("SDIMAX  %10s%10i", SDImax$SP, SDImax$MaxSDI )
    cat( j, sep="\n", file=keyfilename, append=T )
  } 
  
  ## calibration
  
  write(t1, file=keyfilename, append=T )
  if (!calibrate){
    write( "NOCALIB", file=keyfilename, append=T)
    write( "NOHTDREG", file=keyfilename, append=T)
  }else{
    # GROWTH keyword:
    #> field 1: measurement method, diam
    #> field 2: length of diameter measurement period
    #> field 3: measurement method, ht
    #> field 4: length of height growth measurement period
    #> field 5: length of mortality observation period
    t1 <- sprintf("GROWTH    %10.0f%10.0f%10.0f%10.0f%10.0f",
                  stand$DG_TRANS, stand$DG_MEASURE,
                  stand$HTG_TRANS, stand$HTG_MEASURE,
                  stand$MORT_MEASURE)
    write(t1, file = keyfilename, append = T)
    
    # Get calibration statistics in DB
    write('', file = keyfilename, append = T)
    
    t1 <- sprintf('DATABASE  ')
    write(t1, file = keyfilename, append = T)
    
    t1 <- sprintf('DSNOut     ')
    write(t1, file = keyfilename, append = T)
    t1 <- paste0(stringr::str_pad('FVSOut.db', width = 10, side = 'right'), 
                 sprintf('%10s%10s', '', ''))
    write(t1, file = keyfilename, append = T)
    
    t1 <- sprintf('CALBSTDB  ')
    write(t1, file = keyfilename, append = T)
    t1 <- sprintf('INVSTATS   ')
    write(t1, file = keyfilename, append = T)
    
    t1 <- sprintf('END       ')
    write(t1, file = keyfilename, append = T)
  }
  
  if(!is.null(opt_args$READCORD)){
    t1 <- sprintf('READCORD  ')
    write(t1, file = keyfilename, append = T)
    # 23 species --> 3 lines of 8 entries
    cat(opt_args$READCORD, sep = '', fill = 80, file = keyfilename, append = T)
    write(' ', file = keyfilename, append = T)
  }
  
  if(!is.null(opt_args$READCORR)){
    t1 <- sprintf('READCORR  ')
    write(t1, file = keyfilename, append = T)
    # 23 species --> 3 lines of 8 entries
    cat(opt_args$READCORR, sep = '', fill = 80, file = keyfilename, append = T)
    write(' ', file = keyfilename, append = T)
  }
  
  write('', file = keyfilename, append = T)
  
  # number of cycles 
  t1 <- sprintf("NUMCYCLE  %10i", cycles )
  write(t1, file=keyfilename, append=T ) 
  
  
  write( "PROCESS", file=keyfilename, append=T ) 
  write( "STOP", file=keyfilename, append=T )  
  
  return(filename)
}


## write.fvs.tree.file() ----
#> Create a tree list and write to file.
write.fvs.tree.file <- function( tl, std, treefilename )
{
  # add FVS-specific fields that are connected with regen
  tl$SLOPE <- std$SLOPE 
  tl$ASPECT <- std$ASPECT 
  tl$PV_CODE <- as.numeric(std$PV_CODE)
  tl$TOPO <- as.numeric(std$TOPO)
  tl$SPREP <- 0
  # add some values that are not used
  tl$TVAL <- 0
  tl$CUT <- 0
  
  # turn crown ratio into a class (see essential fvs p43
  tl$CRcode <- ceiling(tl$CRRATIO/10)
  tl$CRcode <- pmin(tl$CRcode,9)
  tl$CRcode <- pmax(tl$CRcode,1)
  
  # not sure how the FVS tables are handling damage, so for now
  tl$DAMAGE1 <- ifelse(!is.na(tl$HTTOPK),97,0)
  tl$DAMAGE2 <- tl$DAMAGE3 <- tl$SEVERITY1 <- tl$SEVERITY2 <- tl$SEVERITY3 <- 0
  
  # replace missing values with empties
  fvs_formats <- data.frame(tree_var=c("PLOT_ID","fvs.TREE_ID","TREE_COUNT","HISTORY","SPECIES",
                                       "DIAMETER","DG","HT","HTTOPK","HTG","CRcode",
                                       "DAMAGE1","SEVERITY1","DAMAGE2","SEVERITY2","DAMAGE3","SEVERITY3",
                                       "TVAL","CUT","SLOPE","ASPECT","PV_CODE","TOPO","SPREP","AGE"),
                            format=c("%4.0f","%4.0f","%8.3f","%1.0f","%3.0f",
                                     "%5.1f","%5.1f","%5.1f","%5.1f","%5.1f","%1.0f",
                                     "%2.0f","%2.0f","%2.0f","%2.0f","%2.0f","%2.0f",
                                     "%1.0f","%1.0f","%2.0f","%3.0f","%3.0f","%1.0f","%1.0f","%3.0f"))
  fvs_formats$txt_format <- with(fvs_formats,paste(substring(format,1,2),"s",sep=""))
  
  
  for (var in 1:nrow(fvs_formats)){ #fvs_formats$tree_var){
    if(fvs_formats$tree_var[var] == 'TREE_COUNT'){
      dec <- 4-nchar(tl$TREE_COUNT) # get number of 0s to put after decimal
      tl$TREE_COUNT <- ifelse(is.na(tl$TREE_COUNT),
                              paste(rep(' ', substring(fvs_formats$format[var],2,2)), collapse = ''),
                              sprintf(paste0('%8.', dec, 'f'), tl$TREE_COUNT))
    }else{
      tl[,fvs_formats$tree_var[var]] <- ifelse(is.na(tl[,fvs_formats$tree_var[var]]),
                                               paste(rep(" ",substring(fvs_formats$format[var],2,2)),collapse=""),
                                               sprintf(fvs_formats$format[var],tl[,fvs_formats$tree_var[var]]))
    }
  }
  tl$SPECIES <- ifelse(as.numeric(tl$SPECIES)<100,
                       paste("0",as.numeric(tl$SPECIES),sep=""),tl$SPECIES)
  
  # write text file
  flat_format <- sprintf(paste(fvs_formats$txt_format,collapse = ""),
                         tl$PLOT_ID,tl$fvs.TREE_ID,tl$TREE_COUNT,tl$HISTORY,tl$SPECIES,
                         tl$DIAMETER,tl$DG,tl$HT,tl$HTTOPK,tl$HTG,tl$CRcode,
                         tl$DAMAGE1,tl$SEVERITY1,tl$DAMAGE2,tl$SEVERITY2,tl$DAMAGE3,tl$SEVERITY3,
                         tl$TVAL,tl$CUT,
                         tl$SLOPE,tl$ASPECT,tl$PV_CODE,tl$TOPO,tl$SPREP,tl$AGE)
  cat(flat_format, file=treefilename, sep="\n")              
  
}

# ==== Functions for running FVS ===============================================

## run_FVS() ----
#> Arguments:
#> ... = optional arguments to pass to write.FVSfiles

run_FVS <- function(st, tr, outdir, num_years, use_tripling = FALSE,
                    use_calibration = FALSE, use_regenmodel = FALSE,
                    ..., file_prefix = NULL, customSDImax = NULL, 
                    randomseed = 2025, fvs_bin, STIDENT = 'FVSProjection',
                    verbose = FALSE){
  
  tr$fvs.TREE_ID <- 1:nrow(tr)
  rFVS::fvsLoad("FVSie", fvs_bin)
  f <- write.FVSfiles(trees = tr, stand = st, years_out = num_years,
                      calibrate = use_calibration, triple = use_tripling,
                      add_regen = use_regenmodel, outdir = outdir,
                      file_prefix = file_prefix,
                      STIDENT = STIDENT, ...)
  
  rFVS::fvsSetCmdLine(paste0('--keywordfile=', f, '.key'))
  
  y <- c(st$INV_YEAR, st$INV_YEAR+num_years-1)
  
  fvs_output <- rFVS::fvsInteractRun(AfterEM1 = 'fetchTrees()',
                                     SimEnd = fvsGetSummary)
  
  if(verbose){
    print(names(fvs_output))
  }
  
  # year 0 tree list
  tl0 <- dplyr::left_join(fvs_output[[1]]$AfterEM1,
                          dplyr::select(tr, fvs.TREE_ID, TUID, PID),
                          by = dplyr::join_by(id == fvs.TREE_ID))
  
  # end of projection cycle tree list
  tl1 <- dplyr::left_join(fvs_output[[2]]$AfterEM1,
                          dplyr::select(tr, fvs.TREE_ID, TUID, PID),
                          by = dplyr::join_by(id == fvs.TREE_ID))
  
  if(verbose){
    cat('Year 0 nrow: ', nrow(tl0), '\n Year N nrow: ', nrow(tl1))
  }
  
  tl <- rbind(tl0, tl1)
  plt_summary <- cbind(fvs_output[[length(fvs_output)]],
                       data.frame(PID = st$PID))
  
  rFVS::fvsLoad('FVSie', fvs_bin)
  
  list(treelist = tl, summary = plt_summary)
}


# ==== Functions for accessing FVS outputs =====================================
## fetch_trees(): Accessing FVS outputs ----
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

# Reference functions ----

## FVS species codes
fvs_spcd <- function(fia_sql = file.path('..', 'data', 'fia', 'SQLite_FIADB_MT.db')){
  out <- data.frame(fvs_cd = seq(1, 23),
                    fia_cd = c(119, 73, 202, 017, 263, 242, 108, 93, 19, 
                               122, 264, 101, 113, 072, 133, 066, 231, 746, 740, 
                               321, 375, 998, 299))
  conn <- DBI::dbConnect(RSQLite::SQLite(), fia_sql)
  species_tab <- dplyr::tbl(conn, 'REF_SPECIES') |>
    dplyr::filter(SPCD %in% out$fia_cd) |>
    dplyr::select(SPCD, COMMON_NAME, SPECIES_SYMBOL) |>
    dplyr::collect()
  DBI::dbDisconnect(conn)
  out <- merge(out, species_tab, by.x = 'fia_cd', by.y = 'SPCD') |>
    dplyr::rename(sp_abb = SPECIES_SYMBOL)
  return(out)
}


# Dave functions ----




# function for getting tree lists from FVS
fetchTrees <- function(){
  tree_list <- fvsGetTreeAttrs(c("id","plot","age","species","dbh","ht","cratio","tpa","mcuft","bdft"))
  tree_list$year <- fvsGetEventMonitorVariables("year")
  tree_list
}

fetchTreesWiki <- function (captureYears)
{
  curYear <- fvsGetEventMonitorVariables("year")
  if (is.na(match(curYear,captureYears))) NULL else
    fvsGetTreeAttrs(c("dbh","ht","species", 'id', "plot", "tpa"))
}
