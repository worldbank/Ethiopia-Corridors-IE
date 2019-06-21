# Globcover Map of Ethiopia

# Setup ------------------------------------------------------------------------
if(Sys.info()[["user"]] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE"
if(Sys.info()[["user"]] == "robmarty") project_file_path <- "~/Dropbox/World Bank/IEs/Ethiopia IE"

library(sp)
library(rgdal)
library(rgeos)
library(raster)
library(ggplot2)
library(rasterVis)
library(dplyr)

# 3.1.1 / Table 3-3
# http://maps.elie.ucl.ac.be/CCI/viewer/download/ESACCI-LC-Ph2-PUGv2_2.0.pdf

# Load Data --------------------------------------------------------------------
eth_adm0 <- getData('GADM', country='ETH', level=0)
time <- 1
for(year in 1992:2015){
  globcover <- raster(file.path(project_file_path, "Data", "RawData", "esa_globcover", "scratch", "ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif"), time) %>% crop(eth_adm0)
  
  # Simplify Globcover Categories ------------------------------------------------
  globcover.df <- as(globcover, "SpatialPixelsDataFrame")
  globcover.df <- as.data.frame(globcover.df)
  colnames(globcover.df) <- c("value", "x", "y") 
  
  globcover.df$landclass <- NA
  globcover.df$landclass[globcover.df$value %in% c(200,201,202)] <- "Bare Areas"
  globcover.df$landclass[globcover.df$value %in% c(190)] <- "Urban"
  globcover.df$landclass[globcover.df$value %in% c(50,60,61,62,70,71,72,80,81,82,90,100,160,170)] <- "Forest"
  globcover.df$landclass[globcover.df$value %in% c(110,130)] <- "Grassland"
  globcover.df$landclass[globcover.df$value %in% c(120,121,122)] <- "Shrubland"
  globcover.df$landclass[globcover.df$value %in% c(140,150,151,152,153)] <- "Sparse Vegetation"
  globcover.df$landclass[globcover.df$value %in% c(180)] <- "Wetland"
  globcover.df$landclass[globcover.df$value %in% c(210)] <- "Water"
  globcover.df$landclass[globcover.df$value %in% c(10,11,12,20,30,40)] <- "Agriculture"
  
  globcover.df$landclass <- as.factor(globcover.df$landclass)
  
  ntl_map <- ggplot() +
    geom_tile(data=globcover.df, aes(x=x,y=y,fill=landclass)) +
    geom_polygon(data=eth_adm0, aes(x=long,y=lat,group=group),fill=NA,color="black",size=.5) +
    labs(colour="") +
    coord_equal() +
    theme_void() +
    theme(legend.key=element_blank(),
          legend.text = element_text(size=16),
          legend.title = element_text(size=13)) +
    labs(title=paste0("Land Cover: ",year),fill="Land Class") +
    scale_fill_manual(values=c("goldenrod2","bisque2","springgreen4","lawngreen","olivedrab","olivedrab4","red","blue","mediumturquoise")) +
    #scale_fill_gradient(name="Nighttime Lights", 
    #                    low = "black", high = "yellow",
    #                    breaks=c(quantile(viirs.df$value, 0.10),
    #                             quantile(viirs.df$value, 0.50),
    #                             quantile(viirs.df$value, 0.99999)),
    #                    labels=c("Minimum","","Maximum"))
    theme(
      plot.title = element_text(hjust = 0.5, size=16))
  ggsave(ntl_map, filename=file.path(project_file_path, "Figures", "globcover","country_level", paste0("eth_globcover_",year,".png")), height=9, width=7)
  time<-time+1
}