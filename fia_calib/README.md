## Project Goals

The Forest Vegetation Simulator (FVS) is generally more accurate when it is calibrated to the stands that the user is projecting, but this requires growth data that are costly to obtain.

The US Forest Service's Forest Inventory and Analysis (FIA) National Forest Inventory provides a broad, publicly available dataset of tree growth.

This project examines how FIA data can be used to both to calibrate FVS and to understand why it needs calibration in the first place.

## File Tree
Directories marked with * are ignored by git.
```
fia_calib
|-- raw_data*
|   | Untouched, raw data lives here. This folder is treated as read-
|   | only for scripts.
|
|-- data
|   | All processed project data lives here. 
|   |   
|   |-- fvs_ready
|   |   | Data files ready to be input into FVS. Some larger files are ignored.
|   |
|   |-- fvs_runFiles*
|   |   | .key, .OUT, .tre, and .trl files from running FVS for diagnostics.
|   |
|   |-- sim_outputs*
|   |   | Tree lists, summary stats, and calibration stats from FVS runs as 
|   |   | RData.
|   
|-- reports*
|   | Quarto documents, pdfs, HTML files, and so on for course papers/presentations.
|   
|-- scripts
|   |-- bayes
|   |   | Scripts for Bayesian hierarchical modeling.
|   |   |-- 01a_prep_FIA.R -- get FIA data and prepare for FVS runs
|   |   |-- 01b_get_deficit.R -- get and process climatic water deficit data
|   |   |-- 02_uncalib_run.R -- run all FIA plots through FVS with calibration off.
|   |   |-- 03_lvl1.R, .stan -- simple Bayesian model for scale factors
|   |   |-- 04_lvl2_models.R, .stan -- 2-level hierarchical Bayesian model for 
|   |   |                               scale factors. 2 variations.
|   |   |-- 05_lvl3_models.R, .stan -- 3-level hierarchical Bayesian model for
|   |   |                               scale factors. 2 variations.
|   |   |-- 06_model_checks.R -- posterior predictive checks and visuals
|
|-- model_outputs*
|   | .RData/.RDS files of fitted models from Stan.
| 
|-- fia_calib.RProj
|-- README.md
```
