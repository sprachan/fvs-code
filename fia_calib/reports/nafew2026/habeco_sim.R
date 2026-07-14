library(here)
source(here('reports', 'nafew2026', 'run_globals.R'))

# Directory for .key, .tre, and .out files
dt <- paste0('lub_habeco', format(Sys.Date(), '%b%d%Y'), '_', 
             format(Sys.time(), '%H%M'), collapse = '')
out_dir <- here('reports', 'nafew2026', 'fvs_runs', dt)
dir.create(out_dir)

carb_out <- vector('list', nrow(lub2018_stands))
sim_trees <- vector('list', nrow(lub2018_stands))

for(i in 1:nrow(lub2018_stands)){
  no_hab_match <- any(is.na(lub2018_stands$FVS_HAB[i]),
                      !lub2018_stands$FVS_HAB %in% names(habeco_readcord))
  if(no_hab_match){
    readcord <- unname(habeco_readcord[['260']])
  }else{
    readcord <- unname(habeco_readcord[[as.character(lub2018_stands$FVS_HAB[i])]])
  }
  if(is.null(readcord)|any(is.na(readcord))) stop(i)
  sim <- suppressWarnings(
    run_FVS(stand_info = lub2018_stands[i,], tree_list = lub2018_trees,
            out_dir = out_dir, fvs_bin = fvs_bin,
            proj_len = 100, calibrate = FALSE,
            random_seed = 2025, 
            add_regen = TRUE,
            additionals = carb, 
            CYCLEAT = 2023,
            STDIDENT = lub2018_stands[i,]$STDIDENT,
            READCORD = readcord)
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

habeco_carb <- bind_rows(carb_out)
rm(carb_out)
habeco_trees <- bind_rows(sim_trees)
rm(sim_trees)

save(habeco_carb, habeco_trees, file = here('reports', 'nafew2026', 'habeco_sim.rda'))