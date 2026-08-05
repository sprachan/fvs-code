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
# 'data.frame':	22509 obs. of  15 variables:
# $ PLOT_CN  : chr  "11790037010690" "188772234020004" "37275422010690" "355078214489998" ...
# $ INVYR    : int  2006 2016 2010 2020 2005 2015 2006 2016 2011 2021 ...
# $ STATECD  : int  16 16 16 16 16 16 16 16 16 16 ...
# $ UNITCD   : int  2 2 2 2 2 2 2 2 2 2 ...
# $ COUNTYCD : int  3 3 3 3 3 3 3 3 3 3 ...
# $ PLOT     : int  80059 80059 80113 80113 80234 80234 80239 80239 80290 80290 ...
# $ KINDCD   : int  1 2 1 2 1 2 1 2 1 2 ...
# $ INTENSITY: chr  "1" "1" "1" "1" ...
# $ PID      : chr  "16003280059" "16003280059" "16003280113" "16003280113" ...
# $ N_MEAS   : int  2 2 2 2 2 2 2 2 2 2 ...
# $ REM_CD   : int  1 2 1 2 1 2 1 2 1 2 ...
# $ EMAP_HEX : num  24392 24392 24393 24393 24264 ...
# $ x        : num  -1614278 -1614278 -1623740 -1623740 -1593990 ...
# $ y        : num  2633682 2633682 2611215 2611215 2617088 ...
# $ epsg     : chr  "epsg:102039" "epsg:102039" "epsg:102039" "epsg:102039" ... 
#> 
#> Notes: Original FIA data is in the NAD83 datum, per FIA NFI database documentation. 
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
# terra has better tools for converting spatial data back to regular dataframe
fia_out <- fia_with_emap |>
  terra::vect() |> 
  as.data.frame(geom = 'XY') |>
  mutate(epsg = paste0('epsg:', terra::crs(fia_with_emap, describe = TRUE)['code']))
saveRDS(fia_out, file = file.path('data', 'fia_spatial_filt.RDS'))

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
# [1] viridis_0.6.5          viridisLite_0.4.3      ggplot2_4.0.3          usmap_1.0.0            sf_1.1-1              
# [6] dplyr_1.2.1            rFVSIEtools_0.0.0.9000
# 
# loaded via a namespace (and not attached):
# [1] utf8_1.2.6         generics_0.1.4     class_7.3-23       KernSmooth_2.23-26 RSQLite_3.53.3     stringi_1.8.7     
# [7] magrittr_2.0.5     grid_4.6.1         RColorBrewer_1.1-3 pkgload_1.5.3      fastmap_1.2.0      blob_1.3.0        
# [13] e1071_1.7-17       DBI_1.3.0          gridExtra_2.3      purrr_1.2.2        scales_1.4.0       codetools_0.2-20  
# [19] cli_3.6.6          rlang_1.3.0        dbplyr_2.6.0       units_1.0-1        bit64_4.8.2        withr_3.0.3       
# [25] cachem_1.1.0       otel_0.2.0         tools_4.6.1        memoise_2.0.1      usmapdata_1.0.0    vctrs_0.7.3       
# [31] R6_2.6.1           proxy_0.4-29       lifecycle_1.0.5    classInt_0.4-11    stringr_1.6.0      bit_4.6.0         
# [37] pkgconfig_2.0.3    terra_1.9-34       pillar_1.11.1      gtable_0.3.6       glue_1.8.1         Rcpp_1.1.1-1.1    
# [43] tibble_3.3.1       tidyselect_1.2.1   rstudioapi_0.18.0  farver_2.1.2       compiler_4.6.1     S7_0.2.2