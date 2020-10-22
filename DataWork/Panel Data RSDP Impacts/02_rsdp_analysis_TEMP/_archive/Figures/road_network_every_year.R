# Figure of Roads Every Year

# Setup ------------------------------------------------------------------------
if(Sys.info()[["user"]] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE"
if(Sys.info()[["user"]] == "robmarty") project_file_path <- "~/Dropbox/World Bank/IEs/Ethiopia IE"

library(rgdal)
library(raster)
library(dplyr)
library(ggplot2)
library(broom)
library(viridis)

# Load Data --------------------------------------------------------------------
setwd(file.path(project_file_path, "Data","RawData", "RoadNetworkPanelDataV3_1996_2016_Revised"))
roads_1996 <- readOGR(dsn=".", layer="All_Network_1996") %>% spTransform("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
roads_1998 <- readOGR(dsn=".", layer="All_Network_1998") %>% spTransform("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
roads_2000 <- readOGR(dsn=".", layer="All_Network_2000") %>% spTransform("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
roads_2002 <- readOGR(dsn=".", layer="All_Network_2002") %>% spTransform("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
roads_2004 <- readOGR(dsn=".", layer="All_Network_2004") %>% spTransform("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
roads_2006 <- readOGR(dsn=".", layer="All_Network_2006") %>% spTransform("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
roads_2008 <- readOGR(dsn=".", layer="All_Network_2008") %>% spTransform("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
roads_2010 <- readOGR(dsn=".", layer="All_Network_2010") %>% spTransform("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
roads_2012 <- readOGR(dsn=".", layer="All_Network_2012") %>% spTransform("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
roads_2014 <- readOGR(dsn=".", layer="All_Network_2014") %>% spTransform("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
roads_2016 <- readOGR(dsn=".", layer="All_Network_2016") %>% spTransform("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

eth_adm0 <- getData('GADM', country='ETH', level=0)



for(year in seq(1996,2016,2)){
  print(year)
  roads <- readOGR(dsn=".", layer=paste0("All_Network_",year)) %>% spTransform("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  roads$id <- row.names(roads)
  roads_tidy <- tidy(roads)
  roads_tidy <- merge(roads_tidy, roads, by="id")
  roads_tidy$speed <- roads_tidy[[names(roads_tidy)[grepl(paste0("Speed",year), names(roads_tidy))]]]
  roads_tidy$speed[roads_tidy$speed >= 70] <- 70

  p <- ggplot() +
    geom_polygon(data=eth_adm0, aes(x=long,y=lat,group=group),
                 color="black", fill="black") +
    geom_path(data=roads_tidy, aes(x=long,y=lat,group=group,color=speed),
              size=roads_tidy$speed^(1/3)/5-.3) +
    labs(title=year,
         color="Speed\nLimit\n(km/hr)") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5)) +
    coord_quickmap() + 
    scale_colour_distiller(palette="Spectral",limits=c(10,70),
                           labels=c("10","20","30","40","50","60",">70"))
  ggsave(p, filename = file.path(project_file_path,"Figures","roads_every_year",paste0("roads_",year,".png")))
}

RdYlGreen

roads_2016 <- readOGR(dsn=".", layer="All_Network_2016") %>% spTransform("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")



roads.addis.adama <- roads[roads$LINKNAME %in% c("Soge - Arjo",
                                                 "Galo-Anger-Bello-Bereda",
                                                 "Abel - Kutamori",
                                                 "Jirma Sebategna River - Kutamori",
                                                 "Main Road - Didiga",
                                                 "Tankara - Sayi Dalecha"),]  
roads <- 
