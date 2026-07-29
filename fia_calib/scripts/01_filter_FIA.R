# DESCRIPTION ==================================================================
#>
#> Purpose: FIA data for training the IE calibration model must be constrained
#>  in some way to make fitting the model feasible given my computing power.
#>  This initial approach is to download the data for every state covered by the
#>  IE variant, then take plots that fall within the variant outline (provided
#>  by the USFS) and plots that fall within 100km of the outline. Plots are
#>  further filtered to only retain plots that had been established under the 
#>  new protocol (post-2000), fell under the usual FIA sampling intensity,
#>  and had been measured at least 2 times total. When going through each state
#>  database, I also filtered to only plots within that state (matching STATECD
#>  to state of interest) to avoid double-including plots that were near state
#>  borders.
#> 
#> Outputs: data/fia_spatial_filt.RDS
# Simple feature collection with 22509 features and 12 fields
# Geometry type: POINT
# Dimension:     XY
# Bounding box:  xmin: -1771961 ymin: 2534706 xmax: -1165271 ymax: 3096578
# Projected CRS: USA_Contiguous_Albers_Equal_Area_Conic_USGS_version + NAVD88 height
# # A tibble: 22,509 × 13
# PLOT_CN          INVYR STATECD UNITCD COUNTYCD  PLOT KINDCD INTENSITY           geometry          PID   N_MEAS REM_CD EMAP_HEX
# <chr>             <int>   <int>  <int>    <int> <int>  <int>    <chr>          <POINT [m]>       <chr>  <int>  <int>    <dbl>
# 1  117900370106…  2006      16      2        3 80059      1         1   (-1614278 2633682) 16003280059      2      1    24392
# 2  188772234020…  2016      16      2        3 80059      2         1   (-1614278 2633682) 16003280059      2      2    24392
# 3  372754220106…  2010      16      2        3 80113      1         1   (-1623740 2611215) 16003280113      2      1    24393
# 4  355078214489…  2020      16      2        3 80113      2         1   (-1623740 2611215) 16003280113      2      2    24393
# # ... etc
#> 
#> Notes: FIA data is in the NAD83 datum, per FIA NFI database documentation. 
#> 
#>
#> Data Sources:
#> - Variant shapefile
#> https://www.fs.usda.gov/fmsc/ftp/fvs/docs/overviews/FVSVariantMap20210525.zip
#> - FIA data
#> https://research.fs.usda.gov/products/dataandtools/fia-datamart
#> - EPA hex shapefile
#> The only available shapefiles I found were for a 40km tessellation, whereas
#> FIA references hex IDs from the 648km2 tessellation. I found a different dataset that had
#> the EMAP hex IDs for the 648km2 tessellation (plus much other stuff) at:
#> https://www.earthdata.nasa.gov/data/catalog/ornl-cloud-fia-forest-biomass-estimates-1873-1
#> Which required the creation of an Earthdata account.
# ==============================================================================

# Dependencies ----
library(rFVSIEtools)
library(dplyr)
library(sf)
library(usmap)
library(ggplot2)
library(viridis)

# Spatial data ----
variants <- st_read(file.path('raw_data', 'FVSVariantMap20210525', 'FVS_Variants_and_Locations.shp')) |>
  filter(FVSVariant != 'AK') |>
  mutate(ie_locs = factor(ifelse(FVSVariant == 'IE'|FVSVariant == 'EM', FVSLocName, NA))) |>
  group_by(ie_locs, FVSVariant) |>
  summarize() |>
  ungroup()

ie <- filter(variants, FVSVariant == 'IE') |>
  select(FVSVariant, geometry) |>
  summarize()


# Buffer IE polygon by 100km ----
ie_buffered <- st_buffer(ie, dist = 100*1000) |>
  st_intersection(variants) # don't want to go up into Canada

# check intersecting states
usmap::us_map(regions = 'states') |>
  st_transform(crs = st_crs(ie_buffered)) |>
  st_intersection(ie_buffered) |>
  group_by(full) |>
  summarize() # ID, MT, OR, WA

# FIA data ----
fia_geom <- vector('list', 4)
states <- c('MT', 'WA', 'ID', 'OR')
state_cds <- c(30, 53, 16, 41)
for(i in seq_along(states)){
  fname <- paste0('SQLite_FIADB_', states[i], '.db')
  conn <- DBI::dbConnect(RSQLite::SQLite(), 
                         file.path('raw_data', 'fia', fname))

  fia_geom[[i]] <- dplyr::tbl(conn, 'PLOT') |>
    select(CN, INVYR, STATECD, UNITCD, COUNTYCD, PLOT, KINDCD, LAT, LON, INTENSITY) |>
    collect() |>
    filter(STATECD == state_cds[i],
           INTENSITY == 1,
           KINDCD != 0,
           INVYR > 2000) 
  DBI::dbDisconnect(conn)
}

fia_spat <- bind_rows(fia_geom) |>
  st_as_sf(coords = c('LON', 'LAT'), crs = 'epsg:4269') |>
  st_transform(crs = st_crs(variants))

fia_filt <- st_filter(fia_spat, ie_buffered, join = st_within) |>
  rename(PLOT_CN = 'CN') |>
  mutate(PID = paste0(STATECD, 
                      stringr::str_pad(COUNTYCD, 3, pad = '0'),
                      UNITCD,
                      PLOT)) |>
  filter(INVYR >= 2000) |>
  arrange(PID, INVYR) |>
  group_by(PID) |>
  mutate(N_MEAS = n(),
         REM_CD = row_number()) |>
  filter(N_MEAS >= 2)

# find relocated plots
check <- distinct(fia_filt, geometry, PID) |>
  group_by(geometry) |>
  tally() |>
  filter(n > 1) 
#> 0 obs of 2 variables -- filtered out relocated plots by getting rid of plots
#> that had been measured less than 2x

# EPA hexagons ----
epa_hexes <- st_read(file.path('raw_data', 
                               'FIA_Forest_Biomass_Estimates_1873',
                               'data',
                               'CONUSbiohex2020.shp')) |>
  select(EMAP_HEX, geometry) |>
  st_transform(crs = st_crs(variants))

ie_hexes <- epa_hexes |>
  st_filter(ie_buffered,
            .predicate = st_intersects)

fia_with_emap <- st_join(fia_filt, ie_hexes, join = st_within)
fia_per_hex <- st_join(fia_filt, ie_hexes, join = st_within) |>
  st_drop_geometry() |>
  distinct(STATECD, UNITCD, COUNTYCD, PLOT, EMAP_HEX) |>
  group_by(EMAP_HEX) |>
  tally() |>
  right_join(ie_hexes) |>
  st_as_sf()


# Visualize ----
## FIA plots on top of variant map ----
variant_plot <- ggplot(variants)+
  geom_sf(aes(fill = ie_locs), color = 'darkgrey')+
  theme(legend.position = 'bottom')+
  geom_sf(data = ie, fill = NA, color = 'black', lwd = 1)

variant_plot+
  geom_sf(data = ie_buffered, fill = 'grey', alpha = 0.5)

# double check that the filtering worked by overlaying the outline of filtered
#> FIA plots over the IE buffer
variant_plot+
  geom_sf(data = ie_buffered, fill = 'grey', alpha = 0.5)+
  geom_sf(data = fia_outline, fill = 'blue', alpha = 0.5)

## FIA plots in context of contiguous US w states ----
st_transform(us_map(exclude = c('HI', 'AK')), crs = st_crs(fia_outline)) |>
  ggplot()+
  geom_sf(fill = 'darkgrey')+
  geom_sf(data = fia_outline, fill = 'blue', alpha = 0.5)+
  geom_sf(data = ie, color = 'white', fill = rgb(1, 1, 1, 0.25), linetype = 'dashed')

ggplot(fia_with_emap)+
  geom_sf(aes(fill = factor(N_MEAS), color = factor(N_MEAS)), alpha = 0.1, shape = 21)+
  geom_sf(data = st_transform(us_map(include = c('ID', 'WA', 'OR', 'MT')), crs = st_crs(fia_per_hex)),
          fill = NA, color = 'darkgrey', lwd = 1)

## count of plots per EPA hex
ggplot(fia_per_hex)+
  geom_sf(aes(fill = n))+
  geom_sf(data = st_transform(us_map(include = c('ID', 'WA', 'OR', 'MT')), crs = st_crs(fia_per_hex)),
          fill = NA, color = 'white', lwd = 1)+
  scale_fill_viridis()+
  theme_dark()+
  labs(fill = '# of FIA plots')

ggplot(fia_per_hex)+
  geom_sf(aes(fill = cut(n,  breaks = c(1, 20, 31, 54, 81), include.lowest = TRUE)))+
  labs(fill = 'Binned # of plots')+
  geom_sf(data = st_transform(us_map(include = c('ID', 'WA', 'OR', 'MT')), crs = st_crs(fia_per_hex)),
          fill = NA, lwd = 1)

# Write dataframe to .RDS object ----
saveRDS(fia_with_emap, file = file.path('data', 'fia_spatial_filt.RDS'))
