
library(cowplot)
library(grid)
library(maptools)
library(maps)
library(sp)
library(rgdal)
library(rgeos)
library(raster)
library(ggmap)
library(ggplot2)
library(dplyr)
#devtools::install_github("3wen/legendMap")
library(legendMap)


file.path <- "~/Dropbox/Ethiopia IE/"

# Load Data --------------------------------------------------------------------
# Roads
setwd(paste(file.path, "Data/RoadNetworkPanelDataV3_1996_2016_Revised/",sep=""))
roads <- readOGR(dsn=".", layer="All_Network_2016")
roads.addis.adama <- roads[roads$LINKNAME %in% c("Soge - Arjo",
                                                 "Galo-Anger-Bello-Bereda",
                                                 "Abel - Kutamori",
                                                 "Jirma Sebategna River - Kutamori",
                                                 "Main Road - Didiga",
                                                 "Tankara - Sayi Dalecha"),]  
roads <- spTransform(roads, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
roads.addis.adama <- spTransform(roads.addis.adama, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
roads.addis.adama.buff <- gBuffer(roads.addis.adama, width=5/111.12)
roads.addis.adama.buff.large <- gBuffer(roads.addis.adama, width=5/111.12)

if(F){
# Addis - Adama (Toll Road)
roads.addis.adama <- roads[roads$LINKNAME %in% c("Soge - Arjo",
                                                 "Galo-Anger-Bello-Bereda",
                                                 "Abel - Kutamori",
                                                 "Jirma Sebategna River - Kutamori",
                                                 "Main Road - Didiga",
                                                 "Tankara - Sayi Dalecha"),]  
roads.addis.adama <- roads[roads$LINKNAME %in% c("Soge - Arjo",
                                                 "Abel - Kutamori",
                                                 "Jirma Sebategna River - Kutamori",
                                                 "Main Road - Didiga",
                                                 "Tankara - Sayi Dalecha",
                                                 "Soge - Dedesa River"),]
roads.addis.adama <- roads[roads$LINKNAME %in% c("Dibate - Korka",
                                                 "Berber - Lega buna",
                                                 "Galisa - Gongo",
                                                 "Mora - Gipo",
                                                 "Berber - Doben - Gulzen"),]
roads.addis.adama <- roads[roads$LINKNAME %in% c("Simu to Chabe",
                                                 "Mega -Dillo",
                                                 "Delo- Badiya-Gobso",
                                                 "dillo-huboqi"),]
roads.addis.adama <- roads[roads$LINKNAME %in% c("Senbete  - Hadele ela",
                                                 "Hadese  ela - Dele ela",
                                                 "Hadele Ela - Enayto",
                                                 "Hadele ela - Ateyna Rawa",
                                                 "Awaro - Dula",
                                                 "Senbete  - Were Lencha Gangha",
                                                 "Lugo - Gengha"),]
roads.addis.adama <- roads[roads$LINKNAME %in% c("Merkiyo - Musly",
                                                 "Gagu - Melka'e"),]
roads.addis.adama <- roads[roads$LINKNAME %in% c("Jinka-Hana-Omo-Factory3"),]
}

# NTL
ntl.1 <- raster("~/Dropbox/Ethiopia IE/Data/NTL Rasters Annual/viirs2012.tif") %>% crop(roads.addis.adama.buff) #%>% mask(roads.addis.adama.buff.large)
ntl.2 <- raster("~/Dropbox/Ethiopia IE/Data/NTL Rasters Annual/viirs2017.tif") %>% crop(roads.addis.adama.buff) #%>% mask(roads.addis.adama.buff.large)

extent.graph <- extent(roads)
extent.graph@xmin <- 36.15
extent.graph@xmax <- 36.35
extent.graph@ymin <- 9.04
extent.graph@ymax <- 9.35



ntl.1 <- crop(ntl.1, extent.graph)
ntl.2 <- crop(ntl.2, extent.graph)
roads.addis.adama <- crop(roads.addis.adama, extent.graph)

roads.addis.adama.existing <- roads.addis.adama[roads.addis.adama$LINKNAME %in% "Galo-Anger-Bello-Bereda",]
roads.addis.adama.new <- roads.addis.adama[!roads.addis.adama$LINKNAME %in% "Galo-Anger-Bello-Bereda",]

ntl.1.df <- as(ntl.1, "SpatialPixelsDataFrame")
ntl.1.df <- as.data.frame(ntl.1.df)
colnames(ntl.1.df) <- c("value","x","y")
#ntl.1.df$value <- log(ntl.1.df$value+1)
ntl.1.df$value <- log(log(ntl.1.df$value+1)+1)

ntl.2.df <- as(ntl.2, "SpatialPixelsDataFrame")
ntl.2.df <- as.data.frame(ntl.2.df)
colnames(ntl.2.df) <- c("value","x","y")
#ntl.2.df$value <- log(ntl.2.df$value+1)
ntl.2.df$value <- log(log(ntl.2.df$value+1)+1)

ntl.plot.1 <- ggplot() + 
  geom_tile(data=ntl.1.df, aes(x=x,y=y,fill=value),alpha=0.7) +
  coord_equal() +
  theme_void() +
  geom_path(data=roads.addis.adama.existing, aes(x=long,y=lat,group=group,color="Existing"),alpha=0.6,size=1) +
  scale_fill_gradient(name="Nighttime\nLights", 
                      low = "black", high = "yellow",
                      breaks=c(0.1,0.25,0.5,0.75,1,1.25),
                      labels=c("Minimum","","","","","Maximum"),
                      limits=c(min(ntl.1.df$value,ntl.2.df$value), max(ntl.1.df$value,ntl.2.df$value))) +
  guides(colour=guide_legend(title="Roads")) +
  scale_color_manual(values=c("paleturquoise3")) +
  labs(title="2012") +
  theme(legend.key=element_blank(),
        plot.title = element_text(hjust=.5, size=25)) 

ntl.plot.2 <- ggplot() + 
  geom_tile(data=ntl.2.df, aes(x=x,y=y,fill=value),alpha=0.7) +
  coord_equal() +
  theme_void() +
  geom_path(data=roads.addis.adama.existing, aes(x=long,y=lat,group=group,color="Existing"),alpha=0.6,size=1) +
  geom_path(data=roads.addis.adama.new, aes(x=long,y=lat,group=group,color="New"),alpha=0.6,size=1) +
  scale_fill_gradient(name="Nighttime\nLights", 
                      low = "black", high = "yellow",
                      breaks=c(0.1,0.25,0.5,0.75,1,1.15),
                      labels=c("Minimum","","","","","Maximum"),
                      limits=c(min(ntl.1.df$value,ntl.2.df$value), max(ntl.1.df$value,ntl.2.df$value))) +
  guides(colour=guide_legend(title="Roads")) +
  scale_color_manual(values=c("paleturquoise3","tomato")) +
  labs(title="2017") + scale_bar(lon = 36.24, lat = 9.045, 
          distance_lon = 5, distance_lat = 1, distance_legend = 2, 
          dist_unit = "km", orientation = FALSE, legend_colour="white") +
  theme(legend.key=element_blank(),
        plot.title = element_text(hjust=.5, size=25),
        legend.text=element_text(size=13),
        legend.title=element_text(size=13)) 

if(F){
ethiopia <- getData('GADM', country='ETH', level=0)
extent.graph.buf <- extent.graph
extent.graph.buf <- extent.graph.buf+.12
extent.graph.sdf <- as(extent.graph.buf, 'SpatialPolygons') 

eth.road.area <- ggplot() + 
  geom_polygon(data=ethiopia, aes(x=long,y=lat,group=group),fill=NA,color="black") +
  geom_path(data=roads, aes(x=long,y=lat,group=group)) +
  geom_polygon(data=extent.graph.sdf, aes(x=long,y=lat,group=group),fill=NA,color="red") +
  coord_equal() +
  theme(legend.key=element_blank()) +
  theme_void() 
}

ntl.plot.2.legend <- get_legend(ntl.plot.2)
ntl.plot.1 <- ntl.plot.1 + theme(legend.position = "none")
ntl.plot.2 <- ntl.plot.2 + theme(legend.position = "none")

p <- plot_grid(ntl.plot.1, ntl.plot.2 ,ntl.plot.2.legend, ncol=3)
title <- ggdraw() + draw_label("Across Ethiopia, over 22,000km of road has been constructed since 1997. A road segment constructed in 2013-14 is shown, located on the western edge of the Oromia region.                                                                 \n", size=10)
plot_grid(p,title, ncol=1, rel_heights=c(1, 0.1)) # rel_heights values control title margins

ggsave(filename="~/Desktop/ntl_increase_road.png", dpi=400, width=14, height=6) 



ggplot() + 
  geom_tile(data=ntl.2.df, aes(x=x,y=y,fill=value),alpha=0.7) +
  geom_path(data=roads.addis.adama.existing, aes(x=long,y=lat,group=group,color="Existing"),alpha=0.6) +
  geom_path(data=roads.addis.adama.new, aes(x=long,y=lat,group=group,color="New"),alpha=0.6) 
  












iris1 <- ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  geom_boxplot() + theme_bw()

iris2 <- ggplot(iris, aes(x = Sepal.Length, fill = Species)) +
  geom_density(alpha = 0.7) + theme_bw() +
  theme(legend.position = c(0.8, 0.8))

plot_grid(iris1, iris2, labels = "AUTO")

# Map --------------------------------------------------------------------------
google.map.expressway <- get_map(location=c(lon=mean(roads.addis.adama@bbox[1,]), 
                                            lat=mean(roads.addis.adama@bbox[2,])), 
                                 zoom = 11,
                                 maptype="watercolor")

ggmap(google.map.expressway) + 
  geom_tile(data=ntl.21.df, aes(x=x,y=y,fill=value),alpha=0.7) +
  geom_path(data=roads.addis.adama, aes(x=long,y=lat,group=group,
                                        color="Addis-Adama \nExpressway \n(Constructed 2014)"),
            alpha=0.6) +
  coord_equal() +
  theme_nothing(legend = TRUE) +
  theme(legend.key=element_blank()) +
  scale_fill_gradient(name="Increase \nin NTL", 
                      low = "red3", high = "yellow",
                      breaks=c(1,2,3,4),
                      labels=c("Minimum","","","Maximum")) +
  labs(colour="") +
  scale_color_manual(values=c("black")) +
  ggtitle("Increase in Nighttime Lights Along Addis-Adama Expressway (2012 to 2016)") +
  theme(plot.title = element_text(hjust = -2.3,face = "bold"))
ggsave(filename="~/Desktop/ntl_increase_addis_adama.png", dpi=400)





