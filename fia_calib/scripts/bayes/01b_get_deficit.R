# DESCRIPTION ==================================================================
#>
#> Purpose:
#> Extract climatic water deficit data at each FIA plot location.
#> 
#> Outputs: 
#> .rds file containing a dataframe where each row corresponds to an
#> FIA plot.
#> 
#> Notes:
#> Run once, or as often as new data are downloaded. 
# ==============================================================================
library(here)
library(terra)
library(dplyr)

# The NFI database uses the NAD83 datum: 
# https://research.fs.usda.gov/sites/default/files/2025-08/wo-v9-4_Aug2025_UG_FIADB_database_description_NFI.pdf

crs <- 'epsg:4269' # nad83

# Read in data -----------------------------------------------------------------
stands <- readRDS(here('data', 'fvs_ready', 'FVS_StandInit_MTID.rds')) |>
  filter(VARIANT == 'IE')|>
  select(LATITUDE, LONGITUDE, PID, OWNCD, INV_YEAR, N_REM, REM_CD)

# some stands may have changed ownership!  
changed_own <- stands |>
  select(PID, OWNCD) |>
  summarize(changed = ifelse(mean(OWNCD) != OWNCD[1], TRUE, FALSE),
            .by = PID)
 
 stands_vect <- stands |>
   left_join(changed_own) |>
   group_by(PID) |>
   # use max OWNCD bc private is always larger than public, and I'm not sure
   #> whether they update coordinates to be more precise or not.
   #> coordinates are the same across all remeasurements
   mutate(OWNCD_adj = ifelse(changed, max(OWNCD), OWNCD)) |> 
   filter(REM_CD == 0) |>
   ungroup() |>
   vect(geom = c('LONGITUDE', 'LATITUDE'))
crs(stands_vect) <- crs
  
study_region <- rnaturalearth::ne_states(country = c('united states of america',
                                                     'canada')) |>
  filter(iso_3166_2 %in% c('US-MT', 'US-ID')) |>
  vect() |>
  terra::project(y = crs)

deficit <- rast(here('data', 'raw_data', 
                     'V_1_5_annual_gridmet_historical_Deficit_1980_2019_annual_means_cropped_units_mm.tif')) |>
  terra::project(y = crs) |>
  crop(ext(study_region))
names(deficit) <- 'water_deficit'

plot(deficit)
plot(study_region, add = TRUE, fill = NA)
plot(stands_vect, add = TRUE)

# Buffer and extract -----------------------------------------------------------
# buffer each plot locations 0.5mi (if on public land) or 1mi (if on private land) 
pub_radius <- 0.5*1609.344 # convert mi to m
priv_radius <- 1609.344
# OWNCD < 40 is public, >= 40 is private
pub_buffer <- stands_vect[stands_vect$OWNCD_adj < 40,] |>
  buffer(width = pub_radius, quadsegs = 16) #quadsegs makes these a little smoother
priv_buffer <- stands_vect[stands_vect$OWNCD_adj >= 40,] |>
  buffer(width = priv_radius, quadsegs = 16)

plot(deficit)
plot(pub_buffer, add = TRUE)
plot(priv_buffer, add = TRUE)

pub_deficits <- extract(deficit, pub_buffer, fun = mean, touches = TRUE, bind = TRUE) |>
  as.data.frame()
priv_deficits <- extract(deficit, priv_buffer, fun = mean, touches = TRUE, bind = TRUE) |>
  as.data.frame()
# Save results -----------------------------------------------------------------
deficit_df <- bind_rows(pub_deficits, priv_deficits)
saveRDS(deficit_df, here('data', 'env_data', 'climatic_water_deficit.rds'))
