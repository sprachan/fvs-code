## File Tree
Directories marked with * are ignored by git.
```
fia_calib
|-- data
|   | All project data lives here. 
|   |
|   |-- raw_data
|   |   | Untouched, raw data lives here. This folder should be treated as read-
|   |   | only for scripts.
|   |   |
|   |   |-- fia
|   |   |   | Directly downloaded from FIA datamart.
|   |   |   |-- SQLite_FIADB_ID.db* 
|   |   |   |-- SQLite_FIADB_MT.db*
|   |   |-- lubrecht
|   |   |   | Shapefiles for boundary, tree lists, and FVS-formatted xlsx files.
|   |   |-- fvs_blank.xlsx
|   |   
|   |-- fvs_ready
|   |   | Files produced by 02_fia2fvs.R. These are data files ready to be input
|   |   | into FVS as stand or tree lists.
|   |   |
|   |   |-- firstpassFIA2FVS.RData
|   |   |-- FVS_Lubrecht_2018.xlsx
|   |   |-- FVS_Lubrecht_2023.xlsx
|   |   |-- FVSstandInitAll.rds
|   |   |-- FVStreeInitAll.rds
|   |   |-- lubrecht_FVS.xlsx
|   |
|   |-- fvs_runFiles
|   |   | .key, .OUT, .tre, and .trl files from running FVS. Organized into|
|   |   | sub-directories, with each directory containing files from a single 
|   |   | run.
|   |
|   |-- sim_outputs
|   |   | Tree lists, summary stats, and calibration stats from FVS runs as 
|   |   | RData.
|   
|-- reports*
|   | Quarto documents, pdfs, HTML files, and so on generated for class reports.
|   |-- fors538
|   |-- fors591
|   
|-- scripts
|   | R scripts that make up the core of analysis. Note that analysis for fors538 is also in the report PDF.
|   |-- lubrecht_scripts
|   |   |-- 01_lubrecht_prep
|   |   |-- 02_lubrecht_mults
|   |   |-- 03_lubrecht_project
|   |   |-- 04_lubrecht_analysis
|
|-- labnotes*
|   | Quarto documents that contain code , notes, descriptions of data and
|   | outputs, etc. "Lab Notebooks" to document analysis and the progress
|   | of development (supplementing Git commit log).
|   |-- labnotes_fall2025
|   |-- labnotes_spring2026
|
|-- fia_calib.RProj
|-- README.md
```
