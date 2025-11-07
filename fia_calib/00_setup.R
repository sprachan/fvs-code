# Setup ------------------------------------------------------------------------
#>
#> Description: define file paths and directories to be used in all scripts
#> interfacing with rFVS. Load packages that will be used in all scripts.
#>
#-------------------------------------------------------------------------------

# Directories ----

# FVS directories
fvs_dir <- file.path('C:', 'FVS', 'FVSSoftware')
fvs_bin <- file.path(fvs_dir, 'FVSbin')
fvs_rVer <- list.files(file.path(fvs_dir, 'R')) # R version used for naming dirs
rFVS_dir <- file.path(fvs_dir, 'R', fvs_rVer, 'library') # rFVS location

# Data directories and files
fia_path <- file.path('..', 'data', 'fia')
fia_sql <- file.path(fia_path, 'SQLite_FIADB_MT.db')

# Packages -----
library(dbplyr) # use tidy-style data pipelines with SQL
library('rFVS', lib.loc = rFVS_dir) # rFVS

