
# 1. specify some paths (need to be full)
data_dir <- file.path(getwd(),"temp")            # datasets
base_dir <- file.path(getwd(),"R_FVS_terminal")  # R code 
binary_fvs_location <- "/media/affleck/SourisX/FVS/ForestVegetationSimulator/bin/FVScr"

in_data <- file.path(data_dir,"bhnf_FIA_FVS")

# 2. read kcp generating function for indiv stands
source(file.path(base_dir,"kcp_generating_function.R"))

# 3. load the Rdata version of the dataset to get the stand id's easily
load(paste0(in_data,".Rdata"),verbose=T)
fia_stands <- unique(FVS_Stand$STAND_ID)
fia_stands <- fia_stands[1:3]

# 4. set key file name
keyfilename <- file.path(data_dir,"test_db_output.key")

# 5. specify random seed for FVS
rand_seed <- 117

# 6. send output to sql file
outfile <- file.path("test_db_output.db")

# 7. loop over the stands you want to run
for (subject_stand in fia_stands){
  
  # 8. specify run name
  run_name <- paste0(subject_stand)

  # 9. build a kcp file for that stand
  kcp1 <- file.path(data_dir,paste0(run_name,".kcp"))
  fvs_stand_info(subject_stand,in_data,kcp1)
  
  # 10. append stand id to keyfile
  write("STDIDENT", file=keyfilename, append=T)  
  write(paste0(sprintf("%-26s",subject_stand)),file=keyfilename, append=T )
  write("MGMTID", file=keyfilename, append=T)
  write(paste0("R",sprintf("%03d",rand_seed)), file=keyfilename, append=T )
  
  # 11. specify output  to db file
  write("DataBase", file=keyfilename, append=T )
  write("DSNOut", file=keyfilename, append=T )
  write(file.path(data_dir,outfile), file=keyfilename, append=T )
  write("Summary", file=keyfilename, append=T )
  write(sprintf("COMPUTDB%10.0f%10.0f",0,0), file=keyfilename, append=T )
  write(sprintf("TREELIDB  %10.0f",2),file=keyfilename, append=T )
  write("END", file=keyfilename, append=T )
  
  # 12. add basic universal keywords
  write(sprintf("OPEN    %10.0f",41), file=keyfilename, append=T )
  write(kcp1,file=keyfilename, append=T )
  write(sprintf("ADDFILE %10.0f",41), file=keyfilename, append=T)
  write(sprintf("CLOSE   %10.0f",41), file=keyfilename, append=T )
  
  # 13. add random seed
  write(sprintf("RANNSEED %10.0f",rand_seed),file=keyfilename, append=T )
  
  # 14. some regen output
  write("ESTAB", file=keyfilename, append=T )
  write(sprintf("OUTPUT    %10.0f",0), file=keyfilename, append=T )
  write(sprintf("ESRANSD    %10.0f",rand_seed), file=keyfilename, append=T )
  write("END", file=keyfilename, append=T )
  
  # 15. shettle's tree cap stuff
  write(sprintf("OPEN    %10.0f",42), file=keyfilename, append=T )
  write(file.path(base_dir,"TREESZCP_BKNF.kcp"),file=keyfilename, append=T )
  write(sprintf("ADDFILE %10.0f",42), file=keyfilename, append=T)
  write(sprintf("CLOSE   %10.0f",42), file=keyfilename, append=T )
  
  # 16. shettle's volume stuff
  write(sprintf("OPEN    %10.0f",43), file=keyfilename, append=T )
  write(file.path(base_dir,"..","VOLUME_BKNF.kcp"),file=keyfilename, append=T )
  write(sprintf("ADDFILE %10.0f",43), file=keyfilename, append=T)
  write(sprintf("CLOSE   %10.0f",43), file=keyfilename, append=T )
  
  # 17. shettle's regen stuff
  write(sprintf("OPEN    %10.0f",44), file=keyfilename, append=T )
  write(file.path(base_dir,"..","Regen_SpeciesMethod_CR.kcp"),file=keyfilename, append=T )
  write(sprintf("ADDFILE %10.0f",44), file=keyfilename, append=T)
  write(sprintf("CLOSE   %10.0f",44), file=keyfilename, append=T )
  
  # 18. tell fvs to process stand
  write("Process", file=keyfilename, append=T )
} # end replicate loop
# 19. end keyword file with a STOP
write("STOP", file=keyfilename, append=T )

# run FVS
system2(binary_fvs_location, args=paste0("--keywordfile=",keyfilename),wait=TRUE)

# look at result file
library(RSQLite)
con <- dbConnect(SQLite(),file.path(data_dir,outfile))
dbListTables(con)
summary_table <- dbReadTable(con,"FVS_Summary")
case_table <- dbReadTable(con,"FVS_Cases")
tree_list <-dbReadTable(con,"FVS_TreeList")
dbDisconnect(con)

# clean up
# leaving only .out = full output text file for error checking
#              .key = full keyword file
#              .db = database file
junk <- list.files(path=data_dir,pattern="^.+\\.(kcp|txt)$",full.names = TRUE)
file.remove(junk)

