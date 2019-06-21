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
library(broom)

# 3.1.1 / Table 3-3
# http://maps.elie.ucl.ac.be/CCI/viewer/download/ESACCI-LC-Ph2-PUGv2_2.0.pdf

addis <- data.frame(id=1,
                    lat=9.03, 
                    lon=38.74)
coordinates(addis) <- ~lon+lat
crs(addis) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
eth_adm0 <- gBuffer(addis, width=110/111.12, capStyle="SQUARE")
eth_adm0$id <- 1

setwd(file.path(project_file_path, "Data","RawData", "RoadNetworkPanelDataV3_1996_2016_Revised"))
#roads_1996 <- readOGR(dsn=".", layer="All_Network_1996") %>% spTransform("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#roads_1998 <- readOGR(dsn=".", layer="All_Network_1998") %>% spTransform("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#roads_2000 <- readOGR(dsn=".", layer="All_Network_2000") %>% spTransform("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#roads_2002 <- readOGR(dsn=".", layer="All_Network_2002") %>% spTransform("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#roads_2004 <- readOGR(dsn=".", layer="All_Network_2004") %>% spTransform("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#roads_2006 <- readOGR(dsn=".", layer="All_Network_2006") %>% spTransform("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#roads_2008 <- readOGR(dsn=".", layer="All_Network_2008") %>% spTransform("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#roads_2010 <- readOGR(dsn=".", layer="All_Network_2010") %>% spTransform("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#roads_2012 <- readOGR(dsn=".", layer="All_Network_2012") %>% spTransform("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#roads_2014 <- readOGR(dsn=".", layer="All_Network_2014") %>% spTransform("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
roads_2016 <- readOGR(dsn=".", layer="All_Network_2016") %>% spTransform("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
roads_2016 <- raster::intersect(roads_2016, eth_adm0)

roads_1996c <- roads_2016[roads_2016$Complete_G <= 1996 | roads_2016$Speed1996 > 0,]
roads_1997c <- roads_2016[roads_2016$Complete_G <= 1997 | roads_2016$Speed1998 > 0,]
roads_1998c <- roads_2016[roads_2016$Complete_G <= 1998 | roads_2016$Speed2000 > 0,]
roads_1999c <- roads_2016[roads_2016$Complete_G <= 1999 | roads_2016$Speed2000 > 0,]
roads_2000c <- roads_2016[roads_2016$Complete_G <= 2000 | roads_2016$Speed2002 > 0,]
roads_2001c <- roads_2016[roads_2016$Complete_G <= 2001 | roads_2016$Speed2002 > 0,]
roads_2002c <- roads_2016[roads_2016$Complete_G <= 2002 | roads_2016$Speed2004 > 0,]
roads_2003c <- roads_2016[roads_2016$Complete_G <= 2003 | roads_2016$Speed2004 > 0,]
roads_2004c <- roads_2016[roads_2016$Complete_G <= 2004 | roads_2016$Speed2006 > 0,]
roads_2005c <- roads_2016[roads_2016$Complete_G <= 2005 | roads_2016$Speed2006 > 0,]
roads_2006c <- roads_2016[roads_2016$Complete_G <= 2006 | roads_2016$Speed2008 > 0,]
roads_2007c <- roads_2016[roads_2016$Complete_G <= 2007 | roads_2016$Speed2008 > 0,]
roads_2008c <- roads_2016[roads_2016$Complete_G <= 2008 | roads_2016$Speed2010 > 0,]
roads_2009c <- roads_2016[roads_2016$Complete_G <= 2009 | roads_2016$Speed2010 > 0,]
roads_2010c <- roads_2016[roads_2016$Complete_G <= 2010 | roads_2016$Speed2012 > 0,]
roads_2011c <- roads_2016[roads_2016$Complete_G <= 2011 | roads_2016$Speed2012 > 0,]
roads_2012c <- roads_2016[roads_2016$Complete_G <= 2012 | roads_2016$Speed2014 > 0,]
roads_2013c <- roads_2016[roads_2016$Complete_G <= 2013 | roads_2016$Speed2014 > 0,]
roads_2014c <- roads_2016[roads_2016$Complete_G <= 2014 | roads_2016$Speed2016 > 0,]
roads_2015c <- roads_2016[roads_2016$Complete_G <= 2015 | roads_2016$Speed2016 > 0,]
roads_2016c <- roads_2016[roads_2016$Complete_G <= 2016 | roads_2016$Speed2016 > 0,]

roads_1997c$Speed1997 <- roads_1997c$Speed1998
roads_1999c$Speed1999 <- roads_1999c$Speed2000
roads_2001c$Speed2001 <- roads_2001c$Speed2002
roads_2003c$Speed2003 <- roads_2003c$Speed2004
roads_2005c$Speed2005 <- roads_2005c$Speed2006
roads_2007c$Speed2007 <- roads_2007c$Speed2008
roads_2009c$Speed2009 <- roads_2009c$Speed2010
roads_2011c$Speed2011 <- roads_2011c$Speed2012
roads_2013c$Speed2013 <- roads_2013c$Speed2014
roads_2015c$Speed2015 <- roads_2015c$Speed2016



# Load Data --------------------------------------------------------------------
#eth_adm0 <- getData('GADM', country='ETH', level=0)
time <- 5 # time = 5 = 1996
for(year in 1996:2015){

  globcover <- raster(file.path(project_file_path, "Data", "RawData", "esa_globcover", "scratch", "ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif"), time) %>% crop(eth_adm0)
  
  assign("roads", eval(parse(text=paste0("roads_",year,"c"))))
  #roads <- roads %>% crop(eth_adm0)
  
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
  
  globcover.df$landclass[globcover.df$landclass %in% c("Forest", "Grassland", "Shrubland", "Sparse Vegetation", "Bare Areas")] <- "Vegetation"
  globcover.df$landclass[globcover.df$landclass %in% c("Water", "Wetland")] <- "Water/Wetlands"
  
  globcover.df$landclass <- as.factor(globcover.df$landclass)
  
  globcover_map <- ggplot() +
    geom_tile(data=globcover.df, aes(x=x,y=y,fill=landclass)) +
    #geom_polygon(data=eth_adm0, aes(x=long,y=lat,group=group),fill=NA,color="black",size=.5) +
    labs(colour="") +
    coord_equal() +
    theme_void() +
    theme(legend.key=element_blank(),
          legend.text = element_text(size=16),
          legend.title = element_text(size=13)) +
    labs(title=paste0("Land Cover: ",year),fill="Land Class") +
    scale_fill_manual(values=c("lightgoldenrod1","red","chartreuse3","dodgerblue")) +
    theme(
      plot.title = element_text(hjust = 0.5, size=16))
  ggsave(globcover_map, filename=file.path(project_file_path, "Figures", "globcover","country_level_limitedcategories", paste0("eth_globcover_",year,".png")), height=9, width=7)
  time<-time+1
  
  
  roads$id <- row.names(roads)
  roads_tidy <- tidy(roads)
  roads_tidy <- merge(roads_tidy, roads, by="id")
  roads_tidy$speed <- roads_tidy[[names(roads_tidy)[grepl(paste0("Speed",year), names(roads_tidy))]]]
  roads_tidy$speed[roads_tidy$speed >= 70] <- 70
  
  p <- ggplot() +
    geom_polygon(data=eth_adm0, aes(x=long,y=lat,group=group),
                 color="black", fill="black") +
    geom_path(data=roads_tidy, aes(x=long,y=lat,group=group,color=speed),
              size=roads_tidy$speed^(1/3)/5, #
              lwd=1) +
    labs(title=year,
         color="Speed\nLimit\n(km/hr)") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5)) +
    coord_quickmap() + 
    scale_colour_distiller(palette="Spectral",limits=c(10,70),
                           labels=c("10","20","30","40","50","60",">70"))
  ggsave(p, filename = file.path(project_file_path,"Figures","roads_every_year","limited_area", paste0("roads_",year,".png")), height=9, width=7)
}
