## Functions

## Setup

- To be sourced at the beginning of every script
- Set FVS and FIA file paths
- Load rFVS

## fia2fvs
*This could be made a function*

- Connect FIA DB
- Subset FIA data
- Use get_FIA to extract tree and stand tables for FVS input
- Assign plots and trees unique identifiers
- Write resulting files to RDS

## projectPlots
*This could be made a function*

- Load data
- Initialize FVS
- Write keyword and .tre files with write.FVSfiles
- Grow the stand
- Get tree lists
- combine projections with initial and map species
