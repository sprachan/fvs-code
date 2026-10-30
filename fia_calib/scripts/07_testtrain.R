# DESCRIPTION ==================================================================
#>
#> Purpose: Split FIA data into a testing set and a training set, using EPA EMAP
#> hexes to spatially stratify the sample.
#> 
#> Outputs: train_CIDs.csv. A CSV file containing one column ("CID") where each
#> row is the CID.
#> 
#> Notes:
#>
#> Data Sources: 
#> - Error data: 04_fvs_runs.R and 05_calculate_err.R
#> - EPA EMAP hexes: https://www.earthdata.nasa.gov/data/catalog/ornl-cloud-fia-forest-biomass-estimates-1873-1
#> 
# ==============================================================================

library(sf)
library(dplyr)
library(here)
library(ggplot2)
library(viridis)
library(patchwork)

# Spatial Data ----
variants <- here('raw_data', 'FVSVariantMap20210525', 
                 'FVS_Variants_and_Locations.shp') |>
  st_read() |>
  filter(FVSVariant != 'AK') |>
  mutate(ie_locs = factor(ifelse(FVSVariant == 'IE'|FVSVariant == 'EM', FVSLocName, NA))) |>
  group_by(ie_locs, FVSVariant) |>
  summarize() |>
  ungroup()

ie_buffered <- filter(variants, FVSVariant == 'IE') |>
  select(geometry) |>
  summarize() |>
  st_buffer(dist = 100*1000) |>
  st_intersection(variants) |># don't want to go up into Canada
  summarize() |>
  st_convex_hull()
ie_hexes <- st_read(here('raw_data', 
                          'FIA_Forest_Biomass_Estimates_1873',
                          'data',
                          'CONUSbiohex2020.shp')) |>
  select(EMAP_HEX, geometry) |>
  st_transform(crs = st_crs(variants)) |>
  st_filter(ie_buffered,
            .predicate = st_intersects)

states <- usmap::us_map(regions = 'states', include = c('ID', 'MT', 'OR', 'WA')) |>
  st_transform(crs = st_crs(ie_buffered))

# Error data ----
trees_err <- readRDS(here('data', 'sim_outputs', 'all_IE_uncalib_err.RDS')) |>
  filter(calib_use) |>
  mutate(rm = fia_dg == 0)

stand_info <- readRDS(here('data', 'sim_outputs', 'all_IE_uncalib_err.RDS')) |>
  filter(!died, !broke, !started_broken) |>
  group_by(CID) |>
  summarize() |>
  ungroup() |>
  # want to get plot information back
  left_join(distinct(trees_err[c('CID', 'LATITUDE', 'LONGITUDE',
                                 'EMAP_HEX',  'LOCATION', 'ECOREGION', 
                                 'AGE', 'ASPECT', 'SLOPE', 'ELEVFT', 'PV_CODE', 
                                 'dominant_sp', 'matched_by')]))

fia_per_hex <- stand_info |>
  distinct(CID, EMAP_HEX) |>
  group_by(EMAP_HEX) |>
  tally() |>
  right_join(ie_hexes) |> # includes hexes without any data, which will get n = NA
  mutate(n = ifelse(is.na(n), 0, n)) |>
  st_as_sf() 

plot(fia_per_hex)

# Test/train split ----
N <- length(unique(stand_info$CID))
sample_size <- ceiling(0.3*N)
hexes <- unique(fia_per_hex$EMAP_HEX)

fia_per_hex$prop <- ceiling(fia_per_hex$n*sample_size/N)

train_CIDs <- character(0)

for(h in seq_along(hexes)){
  curr_hex <- hexes[h]
  n_strat <- fia_per_hex$prop[h]
  cids <- stand_info$CID[stand_info$EMAP_HEX == curr_hex]
  samp <- sample(cids, size = n_strat, replace = FALSE)
  train_CIDs <- c(train_CIDs, samp)
}


train <- stand_info |>
  filter(CID %in% train_CIDs) |>
  distinct(CID, EMAP_HEX) |>
  group_by(EMAP_HEX) |>
  tally() |>
  right_join(ie_hexes) |>
  mutate(n = ifelse(is.na(n), 0, n)) |>
  st_as_sf()

test <- stand_info |>
  filter(!CID %in% train_CIDs) |>
  distinct(CID, EMAP_HEX) |>
  group_by(EMAP_HEX) |>
  tally() |>
  right_join(ie_hexes) |>
  mutate(n = ifelse(is.na(n), 0, n)) |>
  st_as_sf() 

ggplot(train)+geom_sf(aes(fill = n))+scale_fill_viridis(option = 'mako')+labs(title = 'Train (n = 1548)')+
  ggplot(test)+geom_sf(aes(fill = n))+scale_fill_viridis(option = 'rocket')+labs(title = 'Test (n = 2986)')+
  ggplot(fia_per_hex)+geom_sf(aes(fill = n))+scale_fill_viridis(option = 'magma')+labs(title = 'All (n = 4534)')&
  theme(legend.position = 'bottom', legend.title.position = 'bottom',
        legend.title = element_text(hjust = 0.5))&
  labs(fill = '# of plots')
ggsave(here('figures', 'test_train.png'))

# Save training set CIDs ----
write.csv(data.frame(CID = train_CIDs), 
          file = here('data', 'train_CIDs.csv'),
          row.names = FALSE)
