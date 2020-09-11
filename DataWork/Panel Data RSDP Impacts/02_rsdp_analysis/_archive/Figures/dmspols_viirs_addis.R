# Figures of DMSPOLS and VIIRS Along Addis-Adama Expressway

# Setup ------------------------------------------------------------------------
if(Sys.info()[["user"]] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE"
if(Sys.info()[["user"]] == "robmarty") project_file_path <- "~/Dropbox/World Bank/IEs/Ethiopia IE"

library(ggpubr)

# Load Data --------------------------------------------------------------------
addis <- c("Addis-Adama\nExpressway", 9.002009, 38.761228) %>% t %>% as.data.frame
addis$V1 <- addis$V1 %>% as.character
addis$V2 <- addis$V2 %>% as.character %>% as.numeric
addis$V3 <- addis$V3 %>% as.character %>% as.numeric
addis_buff <- addis
coordinates(addis_buff) <- ~V3+V2
crs(addis_buff) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
addis_buff <- gBuffer(addis_buff, width=40/111.12, capStyle="SQUARE")

year <- 2013
dmspols <- raster(file.path(project_file_path, "Data", "RawData", "Nighttime Lights", "DMSP_OLS","Individual Files", paste0("eth_dmspols_",year,".tif"))) %>% crop(addis_buff) %>% mask(addis_buff)

year <- 2016
viirs <- raster(file.path(project_file_path, "Data", "RawData", "NTL Rasters Annual", "GEE", "Median", paste0("eth_viirs_",year,"_median.tif"))) %>% crop(addis_buff) %>% mask(addis_buff)

setwd(file.path(project_file_path,"Data","RawData", "RoadNetworkPanelDataV3_1996_2016_Revised"))
road.2016 <- readOGR(dsn=".", "All_Network_2016") %>% spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
trunk_roads <- road.2016[road.2016$Classifi_m %in% c("Trunk","Link"),] 
trunk_roads <- trunk_roads %>% crop(addis_buff)

# DMSPOLS Plot -----------------------------------------------------------------
#dmspols[][dmspols[] < 3] <- 0
dmspols[] <- log(dmspols[])

dmspols.df <- as(dmspols, "SpatialPixelsDataFrame")
dmspols.df <- as.data.frame(dmspols.df)
colnames(dmspols.df) <- c("value", "x", "y") 

dmspols_map <- ggplot() +
  geom_polygon(data=addis_buff, aes(x=long,y=lat,group=group),fill="steelblue4") +
  geom_tile(data=dmspols.df[dmspols.df$value > 0,], aes(x=x,y=y,fill=value)) +
  geom_path(data=trunk_roads, aes(x=long,y=lat,group=group),color="lawngreen",size=0.75) +
  geom_path(data=trunk_roads, aes(x=long,y=lat,group=group),color="limegreen",size=0.5) +
  labs(colour="") +
  coord_equal() +
  theme_void() +
  theme(legend.key=element_blank(),
        legend.text = element_text(size=11),
        legend.title = element_text(size=11)) +
  labs(title="") +
  #scale_fill_gradient(name="Nighttime Lights", 
  #                    low = "black", high = "yellow",
  #                    breaks=c(quantile(viirs.df$value, 0.10),
  #                             quantile(viirs.df$value, 0.50),
  #                             quantile(viirs.df$value, 0.99999)),
  #                    labels=c("Minimum","","Maximum")) +
  scale_fill_gradient2(name="Nighttime Lights", 
                       low = "steelblue4", mid="wheat1", high = "red",midpoint=3) +
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5, size=18,face="bold"))
dmspols_map

# VIIRS Plot -----------------------------------------------------------------
viirs <- raster(file.path(project_file_path, "Data", "RawData", "NTL Rasters Annual", "GEE", "Median", paste0("eth_viirs_",year,"_median.tif"))) %>% crop(addis_buff) %>% mask(addis_buff)

viirs[][viirs[] < .5] <- 0
viirs[][viirs[] > 20] <- 20
#viirs[] <- log(viirs[],base=5)

viirs.df <- as(viirs, "SpatialPixelsDataFrame")
viirs.df <- as.data.frame(viirs.df)
colnames(viirs.df) <- c("value", "x", "y") 

viirs_map <- ggplot() +
  geom_polygon(data=addis_buff, aes(x=long,y=lat,group=group),fill="steelblue4") +
  geom_tile(data=viirs.df[viirs.df$value > 0,], aes(x=x,y=y,fill=value)) +
  geom_path(data=trunk_roads, aes(x=long,y=lat,group=group),color="lawngreen",size=0.75) +
  geom_path(data=trunk_roads, aes(x=long,y=lat,group=group),color="limegreen",size=0.5) +
  labs(colour="") +
  coord_equal() +
  theme_void() +
  theme(legend.key=element_blank(),
        legend.text = element_text(size=11),
        legend.title = element_text(size=11)) +
  labs(title="") +
  #scale_fill_gradient(name="Nighttime Lights", 
  #                    low = "black", high = "yellow",
  #                    breaks=c(quantile(viirs.df$value, 0.10),
  #                             quantile(viirs.df$value, 0.50),
  #                             quantile(viirs.df$value, 0.99999)),
  #                    labels=c("Minimum","","Maximum")) +
  scale_fill_gradient2(name="Nighttime Lights", 
                       low = "steelblue4", mid="wheat1", high = "red",midpoint=9.5) +
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5, size=18,face="bold"))

map <- ggarrange(dmspols_map, viirs_map)
ggsave(map, filename=file.path(project_file_path, "Figures", "addis_nighttimelights.png"), height=9, width=7,dpi=400)

