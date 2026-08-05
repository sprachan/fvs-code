# DESCRIPTION ==================================================================
#>
#> Purpose:
#> 
#> Outputs:
#> 
#> Notes:
#>
# ==============================================================================

# Dependencies ----
library(dplyr)
library(ggplot2)

# Spatially filtered FIA data
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

# most plots by far are 100% one-condition
fia_cond |>
  ggplot()+geom_histogram(aes(x = CONDPROP_UNADJ))

condprop_ecdf <- fia_cond |>
  group_by(CONDPROP_UNADJ) |>
  tally() |>
  mutate(contrib = cumsum(n),
         quant = contrib/sum(n))

