# FIA2FVS formatting -----------------------------------------------------------
#>
#> Description: Get plot and tree info for requested conditions and save as
#> .RDS file that can be loaded into a new session. This is run once 
#> (or as needed)
#>
#-------------------------------------------------------------------------------

source('01_setup.R') # directories and packages
source('00_functions.R') # functions
library(ggplot2)

fia_conn <- DBI::dbConnect(RSQLite::SQLite(), fia_sql)

# FYI: FIA database version
fia_vers <- dplyr::tbl(fia_conn, 'REF_FIADB_VERSION') |>
  dplyr::arrange(desc(VERSION))

head(fia_vers, 3)

# Define a subset of FIA MT data ----
# Counties we'll subset
counties <- c('Missoula', 'Mineral', 'Ravalli', 'Granite')

county_codes <- dplyr::tbl(fia_conn, 'COUNTY') |>
  dplyr::filter(COUNTYNM %in% counties) |>
  dplyr::select(COUNTYCD) |>
  dplyr::collect() |>
  unlist()

# map this subset
mt_map <- usmap::us_map(regions = c('counties'),
                        include = 'Montana') |>
  dplyr::mutate(county_short = substr(county, 1, nchar(county)-7),
                study_region = ifelse(county_short %in% counties, TRUE, FALSE))

# Subset the FIA data ----
fia_subset <- fia_conn |>
  dplyr::tbl('COND') |> # condition table: tells us condition
  dplyr::filter(COUNTYCD %in% county_codes,
         COND_STATUS_CD == 1, # accessible forest land only
         CONDPROP_UNADJ >= 0.75, # at least 75% of the plot is in condition X
         INVYR > 2001) |>  # IE plots only
  dplyr::collect()


# Visualize our subset ----
plot_locs <- dplyr::tbl(fia_conn, 'PLOTGEOM') |>
  dplyr::select(INVYR, PLOT, LAT, LON) |>
  dplyr::collect() |>
  dplyr::right_join(fia_subset, by = dplyr::join_by(PLOT, INVYR)) |>
  dplyr::select(LON, LAT, PLOT, INVYR) |>
  dplyr::distinct(PLOT, .keep_all = TRUE) |>
  terra::vect(geom = c('LON', 'LAT')) |>
  sf::st_as_sf() |>
  sf::st_set_crs('+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs +type=crs') |>
  sf::st_transform(sf::st_crs(mt_map))


subset_map <- ggplot()+
  geom_sf(data = mt_map)+
  geom_sf(data = plot_locs,
          alpha = 0.5,
          size = 1)
subset_map

# Get plot and tree info for each condition ----
DBI::dbDisconnect(fia_conn)
# 
# FVS_standInit <- NULL
# FVS_treeInit <- NULL
# 
# pb <- txtProgressBar(min = 1, max = nrow(fia_subset), initial = 1, style = 3)
# for(i in 1:nrow(fia_subset)){
#   plt <- get_FIA(fia_sql, fia_subset$PLT_CN[i])
#   FVS_standInit <- rbind(FVS_standInit, plt[[1]])
#   FVS_treeInit <- rbind(FVS_treeInit, plt[[2]])
#   setTxtProgressBar(pb, i)
# }
# close(pb)
# 
# saveRDS(FVS_standInit, 
#      file = file.path('..', 'data', 'fvs_ready', 'FVSstandInit.rds'))
# 
# saveRDS(FVS_treeInit, 
#      file = file.path('..', 'data', 'fvs_ready', 'FVStreeInit.rds'))

# Subset to plots with 3 measurements ----
plot_list <- dplyr::summarize(fia_subset, .by = PLOT, n = dplyr::n()) |>
  dplyr::filter(n >= 3) |>
  dplyr::select(PLOT) |>
  unlist() # there are only 20 of these...

fia_remeasure <- dplyr::filter(fia_subset, PLOT %in% plot_list)
FVS_standInitRem <- NULL
FVS_treeInitRem <- NULL

pb <- txtProgressBar(min = 1, max = nrow(fia_remeasure), initial = 1, style = 3)

for(i in 1:nrow(fia_remeasure)){
  plt <- get_FIA(fia_sql, fia_remeasure$PLT_CN[i])
  FVS_standInitRem <- rbind(FVS_standInitRem, plt[[1]])
  FVS_treeInitRem <- rbind(FVS_treeInitRem, plt[[2]])
  setTxtProgressBar(pb, i)
}
close(pb)

saveRDS(FVS_standInitRem, 
        file = file.path('..', 'data', 'fvs_ready', 'FVSstandInitRem.rds'))

saveRDS(FVS_treeInitRem, 
        file = file.path('..', 'data', 'fvs_ready', 'FVStreeInitRem.rds'))
