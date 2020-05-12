# Travel Time Distributions 

# Setup ------------------------------------------------------------------------
if(Sys.info()[["user"]] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE"
if(Sys.info()[["user"]] == "robmarty") project_file_path <- "~/Dropbox/World Bank/IEs/Ethiopia IE"

library(ggplot2)
library(matrixStats)

# Load Data --------------------------------------------------------------------
for(year in seq(from=1996,to=2016,by=2)){
  load(file.path(project_file_path, "Data","IntermediateData","Data Outputs for Creating 300m Grid", "travel_time_matrices",paste0("traveltime_",year,".Rda")))
  tt <- eval(parse(text=paste0("traveltime_",year)))
  tt <- subset(tt, select=-c(id))
  mat <- as.matrix(tt)/60/60
  
  png(file.path(project_file_path,"Figures","travel_time_histograms",paste0("traveltime_",year,".png")),height=980,width=980,res=150)
  hist(mat,
       freq=F,
       xlim=c(0,80),
       ylim=c(0,0.05),
       col="deepskyblue3",border="black",
       xlab="Travel Time (Hours)",
       ylab="Proportion",
       main=year)
  dev.off()
  
  rm(list=paste0("traveltime_",year))
  print(year)
}



