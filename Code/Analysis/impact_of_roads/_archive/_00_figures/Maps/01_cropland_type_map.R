
# Load Data --------------------------------------------------------------------
#### ADM
eth_adm0 <- getData('GADM', country='ETH', level=0)

#### Cropland
year <- 2010
cropland <- raster(file.path(rawdata_file_path, "esa_globcover", "scratch", "ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif"), band=(year-1991)) %>% crop(eth_adm0) %>% mask(eth_adm0)
cropland[][!(cropland[] %in% c(10,20,30))] <- NA

# Raster to DF -----------------------------------------------------------------
cropland_spdf <- as(cropland, "SpatialPixelsDataFrame")
cropland_spdf <- as.data.frame(cropland_spdf)
colnames(cropland_spdf) <- c("value", "x", "y")
cropland_spdf$value <- as.character(cropland_spdf$value)

cropland_spdf$value[cropland_spdf$value %in% "10"] <- "Cropland, Rainfed"
cropland_spdf$value[cropland_spdf$value %in% "20"] <- "Cropland, Irrigated"
cropland_spdf$value[cropland_spdf$value %in% "30"] <- "Mosaic Cropland"

cropland_spdf$value <- as.factor(cropland_spdf$value)

# Figures ----------------------------------------------------------------------
cropland_map <- ggplot() +  
  geom_polygon(data=eth_adm0, aes(x=long, y=lat, group=group), fill="gray50") +
  geom_tile(data=cropland_spdf, aes(x=x, y=y, fill=value)) +
  scale_fill_manual(values = c("gold", "green", "blue")) +
  labs(fill="", title = "Cropland Types") +
  theme_void() +
  theme(plot.title = element_text(hjust=0.5, face="bold")) +
  coord_quickmap() 
ggsave(cropland_map, filename=file.path(figures_file_path, "cropland_types_map.png"), height=5, width=5)
