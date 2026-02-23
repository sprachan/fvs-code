library(DBI)
library(RSQLite)
library('odbc')

dsn <- odbcListDataSources()$name[grep('.mdb', odbcListDataSources()$description)]

pgp_conn <- DBI::dbConnect(odbc(),
                           .connection_string = "Driver={Microsoft Access Driver (*.mdb, *.accdb)};dbq=./data/pgp_data_and_docs/PGP_SummaryDB.mdb")
dbListTables(pgp_conn)

exam_tbl <- dplyr::tbl(pgp_conn, 'PGP_Exam')
exam_tbl
colnames(exam_tbl)
exam_defs <- dplyr::tbl(pgp_conn, 'Definitions\ for\ PGP_Exam\ \ table') |>
  dplyr::collect()
View(exam_defs) # looks like exam_tbl has a bunch of FVS-ready variables

# gps locations dataframe. organized by cluster and SETTING_ID
load(file.path('data', 'pgp_data_and_docs', 'pgp_gps_locations_11feb2025.RData'))

# SETTING_ID, lon, lat only
load(file.path('data', 'pgp_data_and_docs', 'pgp_nongps_locations_11feb2025.RData'))

# gives us "master_db" and "master_exam_summary"
load(file.path('data', 'pgp_data_and_docs', 'pgp_database_attributes_25apr2023.RData'))

# I think master_db is this same guy with some columns removed and info cleaned up
dplyr::tbl(pgp_conn, 'PGP_MASTER_StandInfo') |>
  dplyr::collect()  |> View()

# master exam summary has same # of rows as exam_tbl but fewer columns.
#> master_exam_summary has a year and month columns not in exam_tbl,
#> COMMON_NAME (tree species common name)

dbDisconnect(pgp_conn)
