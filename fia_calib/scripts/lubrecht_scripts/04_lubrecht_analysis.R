# DESCRIPTION ------------------------------------------------------------------
#>
#> Use equivalence testing and NHST approaches to compare efficacy of various 
#> FVS calibration methods.
#> 
#-------------------------------------------------------------------------------

# Packages and data ------------------------------------------------------------
library(here)
sim_out <- here('data', 'sim_outputs', 'lubrecht')

# Read in data -----------------------------------------------------------------
# Tree lists
uncalib_tr <- readRDS(file.path(sim_out, 'lub_uncalib_trees.RData')) |>
  dplyr::arrange(STAND_ID, TREE_ID, year)
selfcalib_tr <- readRDS(file.path(sim_out, 'lub_selfcalib_trees.RData')) |>
  dplyr::arrange(STAND_ID, TREE_ID, year)
fiacalib_tr <- readRDS(file.path(sim_out, 'lub_fiacalib_trees.RData')) |>
  dplyr::arrange(STAND_ID, TREE_ID, year)

# Multipliers
fia_mults <- readRDS(file.path(sim_out, 'fia_multipliers.RData'))
self_mults <- readRDS(file.path(sim_out, 'self_multipliers.RData'))

# 2023 actual tree list
lub_treelist2023 <- readxl::read_xlsx(here('data', 'fvs_ready', 
                                       'FVS_Lubrecht_2023.xlsx'),
                                  sheet = 'FVS_TreeInit') |>
  dplyr::filter(STAND_ID %in% uncalib_tr$STAND_ID) 

# Comparison dataframes --------------------------------------------------------
# DBHs
comp_df <- data.frame()
  
# Multipliers
comp_mults <- dplyr::full_join(dplyr::select(fia_mults, TreeSize, SpeciesPLANTS, 
                                             n, avgScaleFactor, avgReadCorMult),
                               dplyr::select(self_mults, TreeSize, SpeciesPLANTS, 
                                             n, avgScaleFactor, avgReadCorMult),
                               by = dplyr::join_by(TreeSize, SpeciesPLANTS),
                               suffix = c('_fia', '_self')) 
# 