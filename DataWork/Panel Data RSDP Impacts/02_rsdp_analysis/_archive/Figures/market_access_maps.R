# Maps of Market Access

# Setup ------------------------------------------------------------------------
if(Sys.info()[["user"]] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE"
if(Sys.info()[["user"]] == "robmarty") project_file_path <- "~/Dropbox/World Bank/IEs/Ethiopia IE"

library(rgdal)
library(raster)
library(viridis)
library(dplyr)
library(rgeos)
library(ggplot2)

#### Load MA Data and Restrict, Replace Values of Raster 
load(file.path(project_file_path, "Data", "IntermediateData", "Data Outputs for Creating 300m Grid", "points.300m.blank.Rda"))
points.300m.blank <- cbind(points.300m.blank@data, points.300m.blank@coords)

load(file.path(project_file_path, "Data", "IntermediateData", "Data Outputs for Creating 300m Grid", "points.300m.distrd_all.Rda"))
for(year in seq(from=1996,to=2016,by=2)){
  points.300m.blank[["distance_distrd_all"]] <- log(points.300m.distrd_all[[paste0("distance_distrd_all_",year)]]+1)
  
  map <- ggplot() +
    geom_tile(data=points.300m.blank, aes(x=longitude,y=latitude,fill=distance_distrd_all)) +
    scale_fill_viridis(direction=-1) +
    coord_quickmap() +
    theme_void() +
    labs(title=year) +
    theme(legend.position="none") +
    theme(plot.title = element_text(hjust = 0.5, size=16))
  ggsave(map,filename=file.path(project_file_path, "Figures", "300m_variables_maps","distance_road", paste0("dist_road_",year,".png")),height=7, width=7)
  
  print(year)
}

