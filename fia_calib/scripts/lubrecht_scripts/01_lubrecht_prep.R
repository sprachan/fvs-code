# DESCRIPTION ------------------------------------------------------------------
#>
#> Take raw lubrecht stand and tree information and put it into 
#> an FVS-ready format (Excel workbook) to run in FVS online.
#> 
#-------------------------------------------------------------------------------

# Packages ---------------------------------------------------------------------
library(here) # makes file paths much easier

# Data directories and files ----
fvs_ready <- here('data', 'fvs_ready')
lub_path <- here('data', 'raw_data', 'lubrecht')


# Read in data ----
raw_standlist <- readxl::read_xlsx(here(lub_path, 'FVS_Lubrecht_2023.xlsx'),
                               sheet = 'FVS_StandInit') |>
  dplyr::select(-PlotID, -m2018, -m2023, -analysis_set, -Strata, -stratum)

raw_treelist <- readxl::read_xlsx(here(lub_path, 'lubrecht_tree_list_partial.xlsx'), 
                                  sheet = 'lef_sample') |>
  dplyr::filter(Year == 2018) 

# Populate stand and tree init tables using Lubrecht information ---------------

tree_init <- data.frame(STAND_ID = paste0('CARB_', raw_treelist$PlotID),
                        PLOT_ID = 1,
                        STANDPLOT_ID = '',
                        TREE_ID = raw_treelist$unique_tree,
                        TAG_ID = '',
                        TREE_COUNT = 1,
                        HISTORY = dplyr::case_when(raw_treelist$Status == 1 ~ 1,
                                                   raw_treelist$Status == 2 ~ 6),
                        SPECIES = raw_treelist$Species,
                        DIAMETER = raw_treelist$DBH,
                        DIAMETER_HT = '',
                        DG = '',
                        HT = raw_treelist$TotalLen,
                        HTG = '',
                        HTTOPK = ifelse(!is.na(raw_treelist$ActualLen),
                                        raw_treelist$ActualLen,
                                        ''),
                        HT_TO_LIVE_CROWN = '',
                        CRRATIO = '',
                        DAMAGE1 = ifelse(!is.na(raw_treelist$ActualLen),
                                         96,
                                         ''),
                        SEVERITY1 = '',
                        DAMAGE2 = '',
                        SEVERITY2 = '',
                        DAMAGE3 = '',
                        SEVERITY3 = '',
                        DEFECT_CUBIC = ifelse(!is.na(raw_treelist$Defect),
                                              raw_treelist$Defect,
                                              ''),
                        DEFECT_BOARD = '',
                        TREEVALUE = '',
                        PRESCRIPTION = '',
                        AGE = '',
                        SLOPE = '',
                        ASPECT = '',
                        PV_CODE = '',
                        PV_REF_CODE = '',
                        TOPOCODE = '',
                        SITEPREP = '',
                        CULL = '',
                        DECAYCD = ifelse(!is.na(raw_treelist$DecayClass),
                                         raw_treelist$DecayClass,
                                         ''),
                        WDLND_STEMS = '') 

# Assign tree tag ID
for(i in unique(tree_init$STAND_ID)){
  tree_init$TAG_ID[tree_init$STAND_ID == i] <- 1:nrow(tree_init[tree_init$STAND_ID == 1])
}

stand_init <- raw_standlist |>
  dplyr::filter(STAND_ID %in% unique(tree_init$STAND_ID)) |>
  dplyr::mutate(INV_YEAR = 2018)

stand_init[is.na(stand_init)] <- ''

# Save out results -------------------------------------------------------------
openxlsx::write.xlsx(list(stand_init, tree_init),
                     file = here(fvs_ready, 'lubrecht_FVS.xlsx'),
                     sheetName = c('FVS_StandInit', 'FVS_TreeInit'))
