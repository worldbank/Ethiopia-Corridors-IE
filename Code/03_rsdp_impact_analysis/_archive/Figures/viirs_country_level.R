# Changes in NTL in Ethiopia 

# Setup ------------------------------------------------------------------------
if(Sys.info()[["user"]] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE"
if(Sys.info()[["user"]] == "robmarty") project_file_path <- "~/Dropbox/World Bank/IEs/Ethiopia IE"

library(raster)
library(ggplot2)
library(rgdal)
library(dplyr)

# Load Data --------------------------------------------------------------------
eth_adm0 <- getData('GADM', country='ETH', level=0)

cities <-rbind(c("Addis Ababa",9.03, 38.74),
               c("Adama",8.541389, 39.268889),
               c("Hawasa",7.05, 38.466667)) %>% 
  as.data.frame %>%
  dplyr::rename(city = V1) %>%
  dplyr::rename(lat = V2) %>%
  dplyr::rename(lon = V3)
cities$lat <- cities$lat %>% as.character %>% as.numeric
cities$lon <- cities$lon %>% as.character %>% as.numeric

for(year in 2013:2017){
  ntl <- raster(file.path(project_file_path, "Data", "RawData", "NTL Rasters Annual", "GEE", "Median", paste0("eth_viirs_",year,"_median.tif")))
  ntl[][ntl[] < 0.2] <- 0
  
  ntl[] <- log(ntl[]+1)
  ntl[] <- log(ntl[]+1)
  
  ntl.df <- as(ntl, "SpatialPixelsDataFrame")
  ntl.df <- as.data.frame(ntl.df)
  colnames(ntl.df) <- c("value", "x", "y") 
  
  ntl_map <- ggplot() +
    geom_polygon(data=eth_adm0, aes(x=long,y=lat,group=group),fill="black") +
    geom_tile(data=ntl.df[ntl.df$value > 0,], aes(x=x,y=y,fill=value)) +
    geom_text(data=cities, aes(x=lon,y=lat,label=city),color="white",nudge_x=c(.9,.6,.7),nudge_y=c(.35,-.2,-.2),size=3.5,fontface="bold",family="Times New Roman") +
    geom_text(data=data.frame(source="Source: VIIRS",lat= 3.7,lon=34.7), aes(x=lon, y=lat,label=source), color="black",family="Times New Roman",fontface="bold") +
    geom_point(data=cities, aes(x=lon,y=lat),color="white",size=.5) +
    labs(colour="") +
    coord_equal() +
    theme_void() +
    theme(legend.key=element_blank(),
          legend.text = element_text(size=11),
          legend.title = element_text(size=11)) +
    labs(title=year) +
    scale_fill_gradient2(name="Nighttime Lights", 
                         low = "yellow", mid="orange", high = "red",midpoint=1) +
    theme(legend.position="none",
          plot.title = element_text(hjust = 0.5))
  ntl_map
  ggsave(ntl_map, filename=file.path(project_file_path, "Figures", "viirs","country_level", paste0("viirs_",year,".png")), height=9, width=7)
}





















