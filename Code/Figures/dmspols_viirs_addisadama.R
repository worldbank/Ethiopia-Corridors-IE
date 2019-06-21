# Figures of DMSPOLS and VIIRS Along Addis-Adama Expressway

# Setup ------------------------------------------------------------------------
if(Sys.info()[["user"]] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE"
if(Sys.info()[["user"]] == "robmarty") project_file_path <- "~/Dropbox/World Bank/IEs/Ethiopia IE"

# Load Data --------------------------------------------------------------------
#### Expressway
setwd(file.path(project_file_path,"Data","RawData", "RoadNetworkPanelDataV3_1996_2016_Revised"))
road.2016 <- readOGR(dsn=".", "All_Network_2016") %>% spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
expressway <- road.2016[road.2016$LINKNAME %in% "Addis - Adama (Toll Road)",] 
expressway_buff <- gBuffer(expressway, width=30/111.12, byid = T)

year <- 2013
dmspols <- raster(file.path(project_file_path, "Data", "RawData", "DMSP_OLS", "GEE", paste0("eth_dmspols_",year,".tif"))) %>% crop(expressway_buff) %>% mask(expressway_buff)
viirs <- raster(file.path(project_file_path, "Data", "RawData", "NTL Rasters Annual", "GEE", "Median", paste0("eth_viirs_",year,"_median.tif"))) %>% crop(expressway_buff) %>% mask(expressway_buff)

addis <- c("Addis-Adama\nExpressway", 8.770135, 38.974139) %>% t %>% as.data.frame
addis$V1 <- addis$V1 %>% as.character
addis$V2 <- addis$V2 %>% as.character %>% as.numeric
addis$V3 <- addis$V3 %>% as.character %>% as.numeric

# Along Addis Adama ============================================================

# DMSPOLS Plot -----------------------------------------------------------------
#dmspols[][dmspols[] < 3] <- 0
dmspols[] <- log(dmspols[])

dmspols.df <- as(dmspols, "SpatialPixelsDataFrame")
dmspols.df <- as.data.frame(dmspols.df)
colnames(dmspols.df) <- c("value", "x", "y") 

dmspols_map <- ggplot() +
  geom_polygon(data=expressway_buff, aes(x=long,y=lat,group=group),fill="black") +
  geom_tile(data=dmspols.df[dmspols.df$value > 0,], aes(x=x,y=y,fill=value)) +
  geom_path(data=expressway, aes(x=long,y=lat,group=group),color="green",size=1.5) +
  labs(colour="") +
  coord_equal() +
  theme_void() +
  theme(legend.key=element_blank(),
        legend.text = element_text(size=11),
        legend.title = element_text(size=11)) +
  labs(title="DMSP-OLS") +
  #scale_fill_gradient(name="Nighttime Lights", 
  #                    low = "black", high = "yellow",
  #                    breaks=c(quantile(viirs.df$value, 0.10),
  #                             quantile(viirs.df$value, 0.50),
  #                             quantile(viirs.df$value, 0.99999)),
  #                    labels=c("Minimum","","Maximum")) +
  scale_fill_gradient2(name="Nighttime Lights", 
                       low = "yellow", mid="orange", high = "red",midpoint=3) +
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5, size=18,face="bold"))
dmspols_map
ggsave(dmspols_map, filename=file.path(project_file_path, "Figures", "dmspols","addis_adama", paste0("dmspols_",year,".png")), height=9, width=7)

# VIIRS Plot -----------------------------------------------------------------
viirs[][viirs[] < .2] <- 0
viirs[] <- log(viirs[])

viirs.df <- as(viirs, "SpatialPixelsDataFrame")
viirs.df <- as.data.frame(viirs.df)
colnames(viirs.df) <- c("value", "x", "y") 

viirs_map <- ggplot() +
  geom_polygon(data=expressway_buff, aes(x=long,y=lat,group=group),fill="black") +
  geom_tile(data=viirs.df[viirs.df$value > 0,], aes(x=x,y=y,fill=value)) +
  geom_path(data=expressway, aes(x=long,y=lat,group=group),color="green",size=1.5) +
  labs(colour="") +
  coord_equal() +
  theme_void() +
  theme(legend.key=element_blank(),
        legend.text = element_text(size=11),
        legend.title = element_text(size=11)) +
  labs(title="VIIRS") +
  #scale_fill_gradient(name="Nighttime Lights", 
  #                    low = "black", high = "yellow",
  #                    breaks=c(quantile(viirs.df$value, 0.10),
  #                             quantile(viirs.df$value, 0.50),
  #                             quantile(viirs.df$value, 0.99999)),
  #                    labels=c("Minimum","","Maximum")) +
  scale_fill_gradient2(name="Nighttime Lights", 
                       low = "yellow", mid="orange", high = "red",midpoint=3) +
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5, size=18,face="bold"))
viirs_map
ggsave(viirs_map, filename=file.path(project_file_path, "Figures", "viirs","addis_adama", paste0("viirs_",year,".png")), height=9, width=7)

