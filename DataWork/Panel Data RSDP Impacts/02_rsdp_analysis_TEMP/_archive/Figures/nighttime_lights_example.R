# Figures of DMSPOLS and VIIRS Along Addis-Adama Expressway

# Load Data --------------------------------------------------------------------
#### Create Shapefile of Addis Adama Expressway
addis <- c("Addis-Adama\nExpressway", 30.496170, 71.040466) %>% t %>% as.data.frame
addis$V1 <- addis$V1 %>% as.character
addis$V2 <- addis$V2 %>% as.character %>% as.numeric
addis$V3 <- addis$V3 %>% as.character %>% as.numeric
addis_buff <- addis
coordinates(addis_buff) <- ~V3+V2
crs(addis_buff) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
addis_buff <- gBuffer(addis_buff, width=180/111.12, capStyle="SQUARE")

#### Load DMSP-OLS
#year <- 2013
#dmspols <- raster(file.path(project_file_path, "Data", "RawData", "Nighttime Lights", "DMSP_OLS","Individual Files", paste0("eth_dmspols_",year,".tif"))) %>% crop(addis_buff) %>% mask(addis_buff)

#### Load VIIRS
year <- 2018
#viirs <- raster(file.path(project_file_path, "Data", "RawData", "Nighttime Lights", "VIIRS - 400m extract", paste0("eth_viirs_mean_",year,".tif"))) %>% crop(addis_buff) %>% mask(addis_buff)
viirs <- raster(file.path(paste0("~/Desktop/pak_viirs_mean_",year,".tif"))) %>% crop(addis_buff) %>% mask(addis_buff)

#### Load Road Data
#setwd(file.path(project_file_path,"Data","RawData", "RoadNetworkPanelDataV3_1996_2016_Revised"))
#road.2016 <- readOGR(dsn=".", "All_Network_2016") %>% spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
#trunk_roads <- road.2016[road.2016$Classifi_m %in% c("Trunk","Link","Main Access","Feeder"),] 
#trunk_roads <- trunk_roads %>% crop(addis_buff)

# VIIRS Plot -----------------------------------------------------------------
viirs.df <- as(viirs, "SpatialPixelsDataFrame")
viirs.df <- as.data.frame(viirs.df)
colnames(viirs.df) <- c("value", "x", "y") 

#viirs.df$value_adj <- sqrt(sqrt(viirs.df$value))
viirs.df$value_adj <- viirs.df$value %>% sqrt %>% sqrt
#viirs.df$value_adj[viirs.df$value_adj < .8] <- 0
p <- ggplot() +
  geom_polygon(data=addis_buff, aes(x=long,y=lat,group=group),fill="blue4") +
  geom_tile(data=viirs.df[viirs.df$value_adj > .8,], aes(x=x,y=y,fill=value_adj)) +
  #geom_path(data=trunk_roads, aes(x=long,y=lat,group=group),color="lightsteelblue1",size=.1,alpha=1) +
  labs(colour="") +
  coord_equal() +
  theme_void() +
  labs(title="") +
  scale_fill_gradient2(name="Nighttime Lights", 
                       low = "blue4", mid="orange", high = "white",midpoint=1.9,
                       limits = c(0.6258,3.2388)) +
  theme(legend.position = "none")
ggsave(p, filename=file.path(paste0("~/Desktop/pakistan_ntl_",year,".png")), height=6, width=6)

