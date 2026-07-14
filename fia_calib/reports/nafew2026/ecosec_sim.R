library(here)
source(here('reports', 'nafew2026', 'run_globals.R'))

# Directory for .key, .tre, and .out files
dt <- paste0('lub_ecosec', format(Sys.Date(), '%b%d%Y'), '_', 
             format(Sys.time(), '%H%M'), collapse = '')
out_dir <- here('reports', 'nafew2026', 'fvs_runs', dt)
dir.create(out_dir)

carb_out <- vector('list', nrow(lub2018_stands))
sim_trees <- vector('list', nrow(lub2018_stands))

for(i in 1:nrow(lub2018_stands)){
  sim <- suppressWarnings(
    run_FVS(stand_info = lub2018_stands[i,], tree_list = lub2018_trees,
            out_dir = out_dir, fvs_bin = fvs_bin,
            proj_len = 100, 
            calibrate = FALSE,
            random_seed = 2025, 
            additionals = carb, 
            add_regen = TRUE,
            CYCLEAT = 2023,
            STDIDENT = lub2018_stands[i,]$STDIDENT,
            READCORD = ecosec_readcord)
  )
  sim_trees[[i]] <- sim$tree_list
  
  conn <- DBI::dbConnect(RSQLite::SQLite(), 
                         here('reports', 'nafew2026','FVSOut.db'))
  if('FVS_Carbon' %in% DBI::dbListTables(conn)){
    carb_out[[i]] <- tbl(conn, 'FVS_Carbon') |> collect()
  }
  DBI::dbDisconnect(conn)
  file.remove(here('reports', 'nafew2026', 'FVSOut.db'))
}

ecosec_carb <- bind_rows(carb_out)
rm(carb_out)
ecosec_trees <- bind_rows(sim_trees)
rm(sim_trees)

save(ecosec_carb, ecosec_trees, file = here('reports', 'nafew2026', 'ecosec_sim.rda'))