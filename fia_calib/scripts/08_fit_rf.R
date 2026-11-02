# DESCRIPTION ==================================================================
#>
#> Purpose: Fit a random forest model to the training data.
#> 
#> Outputs: Ranger random forest object and lm object from a species-specific
#> intercept model (no slope).
#> 
#> Notes:
#>
#> Data Sources: 
#> 
# ==============================================================================

library(ranger)
library(dplyr)

# Read in tree-level error data and subset to training data alone ----
train_CIDs <- read.csv(file.path('data', 'train_CIDs.csv')) |>
  mutate(CID = as.character(CID))
trees_err <- readRDS(file.path('data', 'sim_outputs', 'all_IE_uncalib_err.RDS')) |>
  mutate(fia_dbh_2_adj = ifelse(fia_dg == 0, fia_dbh_2+0.025, fia_dbh_2),
         fia_dds_adj = rFVSIEtools::dob_to_dib(fvs_sp, fia_dbh_2_adj)^2-
           rFVSIEtools::dob_to_dib(fvs_sp, fia_dbh_1)^2,
         ldds_err_adj = (10/REM_INT)*log(fia_dds_adj)-log(fvs_dds))|>
  filter(calib_use, !is.infinite(ldds_err_adj)) |> # 3 trees where fvs_dds = 0
  inner_join(train_CIDs) |>
  select(ldds_err_adj, ECOREGION, ASPECT, ELEVFT, SLOPE, PV_CODE, common_name)

sp_table <- sort(table(trees_err$common_name), decreasing = TRUE)
trees_err$common_name <- factor(trees_err$common_name, 
                                levels = names(sp_table),
                                ordered = FALSE)

# Fit RF ----
err_rf <- ranger(ldds_err_adj ~ ., num.trees = 2000,
                 data = trees_err, mtry = 3, importance = 'impurity')

# Fit linear model ----
# Douglas-fir is the baseline
err_lm <- lm(ldds_err_adj ~ common_name, data = trees_err) 

# Save out models for evaluation
saveRDS(err_rf, file = file.path('model_outputs', 'err_rf.RDS'))
saveRDS(err_lm, file = file.path('model_outputs', 'err_lm.RDS'))
