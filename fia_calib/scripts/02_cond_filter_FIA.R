# DESCRIPTION ==================================================================
#>
#> Purpose: Associate the spatially filtered FIA data (at the PLOT level) with 
#> the CONDITION associated with those plots. Filter to only retain forested
#> conditions that make up at least 75% of the plot area so that characteristics
#> calculated at the condition level are based on a suitable amount of data.
#> 
#> Outputs: data/fia_condspat_filt.RDS
# 'data.frame':	11641 obs. of  23 variables:
# $ CN            : chr  "11846100010690" "474177709489998" "31426183010690" "684730344126144" ...
# $ PLT_CN        : chr  "11846097010690" "188766018020004" "31426180010690" "188769212020004" ...
# $ CONDID        : int  1 1 1 1 1 1 1 1 1 1 ...
# $ COND_STATUS_CD: int  1 1 1 1 1 1 1 1 1 1 ...
# $ RESERVCD      : int  0 0 0 0 0 0 0 0 0 0 ...
# $ OWNCD         : int  11 11 11 11 11 11 11 11 11 11 ...
# $ STDORGCD      : int  0 0 0 0 0 0 0 0 0 0 ...
# $ CONDPROP_UNADJ: num  1 1 1 1 1 1 1 1 1 1 ...
# $ PROP_BASIS    : chr  "SUBP" "SUBP" "SUBP" "SUBP" ...
# $ INVYR         : int  2006 2016 2008 2018 2005 2015 2007 2017 2007 2017 ...
# $ STATECD       : int  30 30 30 30 30 30 30 30 30 30 ...
# $ UNITCD        : int  5 5 5 5 5 5 5 5 5 5 ...
# $ COUNTYCD      : int  1 1 1 1 1 1 1 1 1 1 ...
# $ PLOT          : int  80080 80080 80146 80146 80185 80185 80314 80314 80378 80378 ...
# $ KINDCD        : int  1 2 1 2 1 2 1 2 1 2 ...
# $ INTENSITY     : chr  "1" "1" "1" "1" ...
# $ PID           : chr  "30001580080" "30001580080" "30001580146" "30001580146" ...
# $ N_MEAS        : int  2 2 2 2 2 2 2 2 2 2 ...
# $ REM_CD        : int  1 2 1 2 1 2 1 2 1 2 ...
# $ EMAP_HEX      : num  23198 23198 22922 22922 23061 ...
# $ x             : num  -1382664 -1382664 -1340663 -1340663 -1374389 ...
# $ y             : num  2644195 2644195 2622667 2622667 2632726 ...
# $ epsg          : chr  "epsg:102039" "epsg:102039" "epsg:102039" "epsg:102039" ...
#> 
#> Notes: CN in the output = COND.CN = FVS_STANDINIT_COND.STAND_CN
#>
#> Data Sources: 
#> - FIA data
#> https://research.fs.usda.gov/products/dataandtools/fia-datamart
# ==============================================================================

# Dependencies ----
library(dplyr)
library(ggplot2)

# FIA data ---
# start with the spatially filtered data
fia_plots <- readRDS(file.path('data', 'fia_spatial_filt.RDS')) 

fia_cond_list <- vector('list', 4)
states <- c('MT', 'WA', 'ID', 'OR')

for(i in seq_along(states)){
  fname <- paste0('SQLite_FIADB_', states[i], '.db')
  conn <- DBI::dbConnect(RSQLite::SQLite(), 
                         file.path('raw_data', 'fia', fname))
  
  copy_to(conn, df = fia_plots, name = 'SPAT_PLOT', 
          overwrite = TRUE, temporary = TRUE)
  
  fia_cond_list[[i]] <- tbl(conn, 'COND') |>
    select(CN, PLT_CN, CONDID, COND_STATUS_CD, RESERVCD, OWNCD, STDORGCD, 
           CONDPROP_UNADJ, PROP_BASIS) |>
    inner_join(dplyr::tbl(conn, 'SPAT_PLOT'), by = join_by(PLT_CN == PLOT_CN)) |>
    collect()
  
  DBI::dbDisconnect(conn)
}

fia_cond <- bind_rows(fia_cond_list)

# Condition distribution ----
# most plots by far are 100% one-condition
fia_cond |>
  ggplot()+geom_histogram(aes(x = CONDPROP_UNADJ))

# visualize how well this holds for JUST forested conditions:
condprop_ecdf <- fia_cond |>
  filter(COND_STATUS_CD == 1) |>
  group_by(CONDPROP_UNADJ) |>
  tally() |>
  mutate(contrib = cumsum(n),
         quant = contrib/sum(n))
ggplot(condprop_ecdf, aes(x = CONDPROP_UNADJ, y = quant))+geom_line()
# 30% of observations fall below 0.75 condprop --> don't expect too much data loss 
#> from filtering out conditions that take up <75% of plot area.
condprop_ecdf$quant[which(condprop_ecdf$CONDPROP_UNADJ == 0.75)] 

# Filter by condition properties and save ----
fia_cond_filt <- fia_cond |>
  filter(COND_STATUS_CD == 1,
         CONDPROP_UNADJ >= 0.75) |>
  as.data.frame()
saveRDS(fia_cond_filt, file = file.path('data', 'fia_condspat_filt.RDS'))

# Session Info ----
sessionInfo()
# R version 4.6.1 (2026-06-24 ucrt)
# Platform: x86_64-w64-mingw32/x64
# Running under: Windows 11 x64 (build 26200)
# 
# Matrix products: default
# LAPACK version 3.12.1
# 
# locale:
# [1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8    LC_MONETARY=English_United States.utf8
# [4] LC_NUMERIC=C                           LC_TIME=English_United States.utf8    
# 
# time zone: America/Denver
# tzcode source: internal
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
# [1] ggplot2_4.0.3 dplyr_1.2.1  
# 
# loaded via a namespace (and not attached):
# [1] vctrs_0.7.3        cli_3.6.6          rlang_1.3.0        otel_0.2.0         DBI_1.3.0          purrr_1.2.2       
# [7] generics_0.1.4     S7_0.2.2           labeling_0.4.3     glue_1.8.1         bit_4.6.0          dbplyr_2.6.0      
# [13] scales_1.4.0       grid_4.6.1         tibble_3.3.1       fastmap_1.2.0      lifecycle_1.0.5    memoise_2.0.1     
# [19] compiler_4.6.1     RSQLite_3.53.3     blob_1.3.0         RColorBrewer_1.1-3 pkgconfig_2.0.3    rstudioapi_0.18.0 
# [25] farver_2.1.2       R6_2.6.1           tidyselect_1.2.1   pillar_1.11.1      magrittr_2.0.5     tools_4.6.1       
# [31] withr_3.0.3        bit64_4.8.2        gtable_0.3.6       cachem_1.1.0 