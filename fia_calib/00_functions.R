# Functions --------------------------------------------------------------------
#>
#> Description: standalone script that contains functions used for FVS
#> calibration scripts.
#-------------------------------------------------------------------------------

# ===== Functions for dealing with FIA data ====================================
## get_FIA(): Accessing FIA data ----
#> Arguments
#> - db_loc: character vector. File path where FIA database (.db) is located
#> - pcn: plot control number. String.
#> - verbose: print queries (TRUE) or no (FALSE)?

get_FIA <- function(db_loc, fia_condSubset, verbose = FALSE){
  
  fia_db_conn <- DBI::dbConnect(RSQLite::SQLite(), db_loc) 
  pcn <- fia_condSubset$PLT_CN
  # get subset columns that will be used to create unique identifier
  j <- dplyr::select(fia_condSubset, PLT_CN, STATECD, COUNTYCD, UNITCD, PLOT)
  
  # construct and execute SQL query using tidyverse-style piping
  stand_initQ <- dplyr::tbl(fia_db_conn, 'FVS_STANDINIT_PLOT')|> 
    dplyr::filter(STAND_CN == fia_condSubset$PLT_CN) |>
    dplyr::select(STAND_CN, STAND_ID,
                  VARIANT, STATE, COUNTY,
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
  
  stand_init <- dplyr::collect(stand_initQ) |> # execute query
    dplyr::right_join(j, by = dplyr::join_by(STAND_CN == PLT_CN))

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
    fia_treeQ <- dplyr::tbl(fia_db_conn, 'FVS_TREEINIT_PLOT') |>
      dplyr::filter(STAND_CN == pcn) |>
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
      DBI::dbDisconnect(fia_db_conn)
      return(NULL)
    }
    
    out <- list(FVS_StandInit = stand_init,
                FVS_TreeInit = fia_tree)
    DBI::dbDisconnect(fia_db_conn)
    return(out)
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
    dplyr::right_join(treelist, by = join_by(STAND_CN))
  
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
str_padParam <- function(string){
  out <- stringr::str_pad(string, 10, side = 'right')
  return(out)
}

## write_FVS_keywords(): generate an FVS keyword file for a given stand. ---
#> Arguments
#> - stand: the single stand that FVS will be run on. Used to populate standinfo
#>   and other important keywords.
#> - ncycles: how many cycles to run
#> - reporting_ints: additional reporting interval(s) e.g., 1 means 1 year after
#> - calibrate: should FVS use calibration?
#> - triple: should FVS use tripling?
#> - add_regen: should FVS automatically add regeneration/ingrowth?
#> - key_fileName: name, including .key suffix, to use when writing keyword file
#> - ...: optional arguments that will be added as keywords. Work on adding
#>   more support to these.
write_FVS_keywords <- function(stand, ncycles = 10, reporting_ints, 
                               calibrate = TRUE, triple = TRUE, add_regen = TRUE,
                               key_fileName, ...){
  opt_args <- list(...)
  lines <- list()
  if(!is.null(opt_args$SDIMAX)){
    stopifnot(names(opt_args$SDIMAX) == c('Spp', 'Max'))
  }
  
  # first 2 lines: Stand identity (STDIDENT)
  lines[[1]] <- 'STDIDENT'
  if(is.null(opt_args$STDIDENT)){
    lines[[2]] <- ''
  }else{
    lines[[2]] <- paste0(substr(opt_args$STDIDENT, 1, 26), '')
  }
  
  # Specify sampling information:
  #> Inventory year (INVYEAR)
  lines[[3]] <- paste0(str_padParam('INVYEAR'),
                       str_padParam(stand$INV_YEAR))
                       
  
  #> Sampling design (DESIGN)
  lines[[4]] <- paste0(str_padParam('DESIGN'),
                       str_padParam(-1), # 1 inverse of Basal Area factor (1/1 acre)
                       str_padParam(0), # 2 inverse of small tree plot
                       str_padParam('5.0'), # 3 diameter breakpoint b/w large and small
                       str_padParam(stand$NUM_PLOTS), # 4 number of plots per stand
                       str_padParam(0), # 5 nonstockable plots in stand
                       str_padParam(0), # 6 sampling weight
                       str_padParam('1.0') # 7 proportion of stand considered stockable
                      ) 
  
  # Specify stand information:
  lines[[5]] <- paste0(str_padParam('STDINFO'),
                       str_padParam(stand$FOREST), # 1 national forest code
                       str_padParam(stand$PV_CODE), # 2 plant community code
                       str_padParam(stand$AGE), # 3 stand age
                       str_padParam(stand$ASPECT), # 4 aspect in degrees
                       str_padParam(stand$SLOPE), # 5 slope, %
                       str_padParam(stand$ELEVFT)) # 6 elevation, 100s of feet
  
  # Specify tree list outputs
  lines[[6]] <- paste0(str_padParam('TREELIST'),
                       str_padParam(0), # 1 cycles to print. 0 = every cycle
                       str_padParam(3), # 2 file reference,
                       str_padParam(0), # 3 print header? 0 = yes, human readable
                       str_padParam(0), # 4 print both cycle 0 and 1
                       str_padParam(0), # 5 live/dead treelist
                       str_padParam(''), # 6 not used
                       str_padParam(1) # 7 print cycle 0 input and estimated dbh increments
                       )
  
  # Specify tree data format
  lines[[7]] <- str_padParam('TREEFMT')
  lines[[8]] <- '(I4,I4,F8.3,I1,A3,F5.1,F5.1,2F5.1,F5.1,I1,6I2,2I1,I2,2I3,2I1,F3.0)'
  
  # Running FVS
  
  #> Number of cycles
  lines[[9]] <- paste0(str_padParam('NUMCYCLE'),
                       str_padParam(ncycles))
  #> Reporting years
  lines[[10]] <- paste0(str_padParam('CYCLEAT'),
                        str_padParam(stand$INV_YEAR+reporting_ints))
  
  # Optional arguments. I'll start these at list[[20]] so there is room for
  #> multiple reporting years
  
  if(!triple){
    lines[[20]] <- str_padParam('NOTRIPLE')
  }
  
  if(!is.null(opt_args$RANN_SEED)){
    lines[[21]] <- paste0(str_padParam('RANNSEED'),
                          str_padParam(opt_args$RANNSEED))
  }
  
  if(!is.null(opt_args$SDIMax)){
    lines[[22]] <- paste0(str_padParam('SDIMAX'),
                          str_padParam(opt_args$SDIMax$Spp), # 1 which species? defaults to all)
                          str_padParam(opt_args$SDIMax$Max) # 2 max SDI
                          )
  }
  
  if(!calibrate){
    lines[[23]] <- str_padParam('NOCALIB')
    lines[[24]] <- str_padParam('NOHTDREG')
  }
  
  if(!add_regen){
    lines[[25]] <- str_padParam('NOAUTOES')
  }else{
    lines[[25]] <- str_padParam('ESTAB')
    lines[[26]] <- paste0(str_padParam('RANNSEED'),
                          str_padParam(opt_args$RANNSEED))
    lines[[27]] <- str_padParam('END')
  }
  
  # database output arguments
  if(!is.null(opt_args$DATABASE)){
    stopifnot(sum(names(opt_args$DATABASE) %in% c('DSNOUT',
                                                  'COMPUTDB',
                                                  'CALBSTDB',
                                                  'INVSTATS',
                                                  'SUMMARY',
                                                  'TREELIDB'))>0)
      lines[[30]] <- str_padParam('DATABASE')
      if(!is.null(opt_args$DATABASE$DSNOUT)){
        lines[[31]] <- str_padParam('DSNOUT')
        lines[[32]] <- str_padParam(opt_args$DATABASE$DSNOUT)
      }
      if(!is.null(opt_args$DATABASE$COMPUTDB)){
        if(opt_args$DATABASE$COMPUTDB) lines[[33]] <- str_padParam('COMPUTDB')
      }
      if(!is.null(opt_args$DATABASE$CALBSTDB)){
        if(opt_args$DATABASE$CALBSTDB) lines[[34]] <- str_padParam('CALBSTDB')
      }
      if(!is.null(opt_args$DATABASE$INVSTATS)){
        if(opt_args$DATABASE$INVSTATS) lines[[35]] <- str_padParam('INVSTATS')
      }
      if(!is.null(opt_args$DATABASE$SUMMARY)){
        if(opt_args$DATABASE$SUMMARY) lines[[36]] <- str_padParam('SUMMARY')
      }
      if(!is.null(opt_args$DATABASE$TREELIDB)){
        lines[[37]] <- paste0(str_padParam('TREELIDB'),
                              str_padParam(opt_args$DATABASE$TREELIDB[1]),
                              str_padParam(opt_args$DATABASE$TREELIDB[2]))
      }
      lines[[40]] <- str_padParam('END')
    }
  
  
  # End file
  lines[[50]] <- str_padParam('PROCESS')
  lines[[51]] <- str_padParam('STOP')
  
  out <- unlist(lapply(Filter(Negate(is.null), lines), \(x) sprintf('%80-s', x)))
  write(out, file = key_fileName)
}

## format_FVS_treeList(): format the tree list ----
#> Arguments
#> - tl: tree list. This must be a "clean" tree list, e.g., outputted from 
#>   clean_FIA_treeList()
#> - tree_fileName: file name, including '.tre', to write tree list file to.
write_FVS_treeList <- function(tl, tree_fileName){
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
    tl[,fvs_formats$tree_var[var]] <- ifelse(is.na(tl[,fvs_formats$tree_var[var]]),
                                             paste(rep(" ",substring(fvs_formats$format[var],2,2)),collapse=""),
                                             sprintf(fvs_formats$format[var],tl[,fvs_formats$tree_var[var]]))
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
  cat(flat_format, file=tree_fileName, sep="\n")           
  
}

# write_FVS_files(): wrapper for keyword and tree list formatting scripts.
#> Arguments:
#> - trees: cleaned tree list.
#> - stand: stand information dataframe.
#> - ncycles, reporting_ints, calibrate, triple, add_regen, ...: passed to
#>   write_FVS_keywords()
#> - out_dir: directory to write .tre and .key files to
write_FVS_files <- function(trees, stand, ncycles, reporting_ints,
                            calibrate, triple, add_regen, ...,
                            out_dir){
  filepath <- tempfile(tmpdir = out_dir)
  write_FVS_treeList(tl = trees, tree_fileName = paste0(filepath, '.tre'))
  write_FVS_keywords(stand = stand,
                     ncycles = ncycles,
                     reporting_ints = reporting_ints,
                     calibrate = calibrate,
                     triple = triple,
                     add_regen = add_regen,
                     key_fileName = paste0(filepath, '.key'),
                     ...)
  return(filepath)
  
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

# Dave functions ----

write.FVSfiles <- function(  trees, stand, 
                             years_out=100, 
                             calibrate=TRUE,
                             triple=FALSE,
                             add_regen=FALSE,
                             customSDImax=NULL,
                             randomseed=2025){
  
  #stand <- standinit
  #trees <- treeinit
  
  # Set variant defaults if stuff is missing
  stand$ASPECT <- ifelse( is.na(stand$ASPECT),  0, stand$ASPECT )        # aspect degrees
  stand$SLOPE <- ifelse( is.na(stand$SLOPE),    5, stand$SLOPE )         # slope percent
  stand$ELEVFT <- ifelse( is.na(stand$ELEVFT ), 38, stand$ELEVFT/100 ) # elevation
  stand$FOREST <- ifelse( is.na(stand$FOREST),  18, stand$FOREST ) 
  
  ### generate file names
  filename <- tempfile(,tmpdir="temp")
  keyfilename <- paste0( filename, ".key")
  treefilename <- paste0( filename, ".tre")
  
  ### write fvs .tre file
  write.fvs.tree.file( trees, stand, treefilename )    
  
  ###
  ### Keyword file creation
  ###
  
  ### stand identification
  write("STDIDENT", file=keyfilename, append=T)  
  write("FVSProjection",  file=keyfilename, append=T)  
  t1 <- sprintf("RANNSEED  %10.0f",randomseed)
  write(t1, file=keyfilename, append=T )
  
  t1 <- sprintf("STDINFO   %10.1f%10s%10.1f%10.1f%10.1f%10.0f",   
                stand$FOREST, stand$PV_CODE, stand$AGE, stand$ASPECT, stand$SLOPE, stand$ELEVFT )
  write( t1, file=keyfilename, append=T )   
  
  ### site index (not needed for FVSie)
  t1 <- sprintf("SITECODE  %10s%10i%10i", stand$SITE_SPECIES, as.integer(stand$SITE_INDEX+0.5), 1 )
  write(t1, file=keyfilename, append=T)
  
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
  t1 <- sprintf("GROWTH    %10.0f%10.0f%10.0f%10.0f%10.0f",
                stand$DG_TRANS,stand$DG_MEASURE,
                stand$HG_TRANS,stand$HG_MEASURE,stand$MORT_MEASURE)
  write(t1, file=keyfilename, append=T )
  if (!calibrate){
    write( "NOCALIB", file=keyfilename, append=T)
    write( "NOHTDREG", file=keyfilename, append=T)
  }
  
  # number of cycles 
  t1 <- sprintf("NUMCYCLE  %10i", cycles )
  write(t1, file=keyfilename, append=T ) 
  
  
  write( "PROCESS", file=keyfilename, append=T ) 
  write( "STOP", file=keyfilename, append=T )  
  
  return(filename)
}


write.fvs.tree.file <- function( tl, std, treefilename )
{
  ###
  ### tree list file creation with provision for missing heights and crowns
  ###
  
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
    tl[,fvs_formats$tree_var[var]] <- ifelse(is.na(tl[,fvs_formats$tree_var[var]]),
                                             paste(rep(" ",substring(fvs_formats$format[var],2,2)),collapse=""),
                                             sprintf(fvs_formats$format[var],tl[,fvs_formats$tree_var[var]]))
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
