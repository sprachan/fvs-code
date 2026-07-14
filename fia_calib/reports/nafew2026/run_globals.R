library(here)
library(rFVSIEtools)
library(dplyr)
library(readxl)

data_dir <- here('data', 'fvs_ready')

lub2018_stands <- read_xlsx(file.path(data_dir, 'FVS_Lubrecht_2018.xlsx'),
                            sheet = 1) |>
  rename(STAND_CN = STAND_ID) |>
  #set_stand_cols() |>
  mutate(STDORGCD = 0,
         FVS_HAB = as.numeric(PV_CODE),
         FOREST = 116,
         STDIDENT = paste0(gsub('_', '', STAND_CN), ' ', 
                           tolower(gsub('_', '', STAND_CN))))
lub2018_trees <- read_xlsx(file.path(data_dir, 'FVS_Lubrecht_2018.xlsx'),
                           sheet = 2) |>
  rename(STAND_CN = STAND_ID) |>
  mutate(CRRATIO = as.numeric(CRRATIO)) |>
  set_tree_cols(map_habcode = FALSE, stand_info = lub2018_stands)

lub2023_stands <- read_xlsx(file.path(data_dir, 'FVS_Lubrecht_2023.xlsx'),
                            sheet = 1)
lub2023_trees <- read_xlsx(file.path(data_dir, 'FVS_Lubrecht_2023.xlsx'),
                           sheet = 2)

lub2018_trees$TUID <- paste(lub2018_trees$STAND_CN, lub2018_trees$TAG_ID, 
                            sep = '_')
lub2023_trees$TUID <- paste(lub2023_trees$STAND_ID, lub2023_trees$TAG_ID,
                            sep = '_')

matched_TUIDs <- inner_join(lub2018_trees['TUID'], lub2023_trees['TUID']) 

lub2018_trees <- left_join(matched_TUIDs, lub2018_trees)
lub2023_trees <- left_join(matched_TUIDs, lub2023_trees)

habtype_readcord <- readRDS(here('data', 'sim_outputs', 'nafew_sfs',
                                 'habtype_READCORD.rds'))
habeco_readcord <- readRDS(here('data', 'sim_outputs', 'nafew_sfs',
                                'habeco_READCORD.rds'))
ecosec_readcord <- readRDS(here('data', 'sim_outputs', 'nafew_sfs',
                                'ecosec_READCORD.rds'))

fvs_bin <- 'C:/FVS/FVSSoftware/FVSbin'
carb <- paste0('\nFMIN\n',
               'CARBREPT',
               '\n',
               sprintf('CARBCALC  %10i%10i%10s%10s%10s', 0, 0, '', '', ''),
               '\n',
               'END\n')