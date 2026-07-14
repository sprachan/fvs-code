# generates basic keyword component file 

fvs_stand_info <- function(stand_id,    # a particular stand
                           input_name,  # name of input file
                           file_out){   # name of keyword file
  # get the stand info
  load(file.path(paste0(input_name,".Rdata")))
  #some_trees <- FVS_Stand$num_live[FVS_Stand$STAND_ID==stand0]>0

  # load universals
  all_runs <- readLines(file.path(base_dir,"universal_keywords.kcp"))
  write(all_runs, file=file_out, append=T)
  write(sprintf("MODTYPE  %10.0f",3),file=file_out, append=T )
  write(sprintf("TREELIST  %10.0f",0),file=file_out, append=T )

  # # are there trees
  # if(!some_trees){
  #   write("NOTREES", file=file_out, append=T)
  #   write(sprintf("%-10s%10.0f","ESTAB",1), file=file_out, append=T )
  #   write(sprintf("%-10s%10.0f%10s%10.0f%10.0f%10.0f%10s%10.0f","NATURAL",1,"PP",150,100,7,"",0), 
  #         file=file_out, append=T )
  #   write(sprintf("%-10s%10.0f%10s%10.0f%10.0f%10.0f%10s%10.0f","NATURAL",1,"PP",300,100,2,"",0), 
  #         file=file_out, append=T )
  #   write("END", file=file_out, append=T )
  # }
 
  # diameter and height growth modifiers for PIPO, PIGL,POTR5
  corD <- data.frame(PIPO=1.4,PIGL=1.2,POTR5=1.2)
  write("ReadCorD", file=file_out, append=T)
  write(paste0(rep(sprintf("%10.3f",1),8),collapse=""), file=file_out, append=T )
  write(paste0(c(rep(sprintf("%10.3f",1),4),
                     sprintf("%10.3f",corD$PIPO),
                 rep(sprintf("%10.3f",1),3)),
               collapse=""), file=file_out, append=T )
  write(paste0(c(rep(sprintf("%10.3f",1),2),
                     sprintf("%10.3f",corD$PIGL),
                     sprintf("%10.3f",corD$POTR5),
                 rep(sprintf("%10.3f",1),4)),
               collapse=""), file=file_out, append=T )
  write(paste0(rep(sprintf("%10.3f",1),8),collapse=""), file=file_out, append=T )
  write(paste0(rep(sprintf("%10.3f",1),6),collapse=""), file=file_out, append=T )
  
  corH <- data.frame(PIPO=1.4,PIGL=1.2,POTR5=1.2)
  write("ReadCorR", file=file_out, append=T)
  write(paste0(rep(sprintf("%10.3f",1),8),collapse=""), file=file_out, append=T )
  write(paste0(c(rep(sprintf("%10.3f",1),4),
                     sprintf("%10.3f",corH$PIPO),
                 rep(sprintf("%10.3f",1),3)),
               collapse=""), file=file_out, append=T )
  write(paste0(c(rep(sprintf("%10.3f",1),2),
                     sprintf("%10.3f",corH$PIGL),
                     sprintf("%10.3f",corH$POTR5),
                 rep(sprintf("%10.3f",1),4)),
               collapse=""), file=file_out, append=T )
  write(paste0(rep(sprintf("%10.3f",1),8),collapse=""), file=file_out, append=T )
  write(paste0(rep(sprintf("%10.3f",1),6),collapse=""), file=file_out, append=T )

  # input data
  # start db commands
  write("DataBase", file=file_out, append=T )
  
  # specify input database
  write("DSNIN", file=file_out, append=T )
  write(file.path(paste0(input_name,".db")), 
        file=file_out, append=T )
  
  # start sql statement for reading stand info
  write("StandSQL", file=file_out, append=T )
  write("SELECT *", file=file_out, append=T )
  write("FROM FVS_STANDINIT", file=file_out, append=T )
  write("WHERE STAND_ID = '%StandID%'", file=file_out, append=T )
  write("EndSQL", file=file_out, append=T )
  
#  if(some_trees){
    # start sql statement for reading tree info
    write("TreeSQL", file=file_out, append=T )
    write("SELECT *", file=file_out, append=T )
    write("FROM FVS_TREEINIT", file=file_out, append=T )
    write("WHERE STAND_ID = '%StandID%'", file=file_out, append=T )
    write("EndSQL", file=file_out, append=T )
#  }
    
  # end db commands
  write("End", file=file_out, append=T )
  
}
