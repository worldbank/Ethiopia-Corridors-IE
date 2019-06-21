
# Load Data --------------------------------------------------------------------
#### ADM
eth_adm0 <- getData('GADM', country='ETH', level=0)

#### DMSPOLS
dmspols_1996 <- raster(file.path(rawdata_file_path, "Nighttime Lights", "DMSP_OLS_INTERCALIBRATED_ZHANG2016", "F121996.tif")) %>% crop(eth_adm0) %>% mask(eth_adm0)
dmspols_2012 <- raster(file.path(rawdata_file_path, "Nighttime Lights", "DMSP_OLS_INTERCALIBRATED_ZHANG2016", "F182012.tif")) %>% crop(eth_adm0) %>% mask(eth_adm0)

dmspols_1996[] <- dmspols_1996[] / 1000
dmspols_2012[] <- dmspols_2012[] / 1000

dmspols_1996[][dmspols_1996[] %in% 0] <- NA
dmspols_2012[][dmspols_2012[] %in% 0] <- NA

# Raster to DF -----------------------------------------------------------------
dmspols_2012_spdf <- as(dmspols_2012, "SpatialPixelsDataFrame")
dmspols_2012_spdf <- as.data.frame(dmspols_2012_spdf)
colnames(dmspols_2012_spdf) <- c("value", "x", "y")

dmspols_1996_spdf <- as(dmspols_1996, "SpatialPixelsDataFrame")
dmspols_1996_spdf <- as.data.frame(dmspols_1996_spdf)
colnames(dmspols_1996_spdf) <- c("value", "x", "y")

# Figures ----------------------------------------------------------------------
dmspols_1996_map <- ggplot() +  
  geom_polygon(data=eth_adm0, aes(x=long, y=lat, group=group), fill="black") +
  geom_tile(data=dmspols_1996_spdf, aes(x=x, y=y, fill=value)) +
  scale_fill_gradient2(low="white", mid="yellow", high="red") +
  labs(title = "1996") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(hjust=0.5, face="bold")) +
  coord_quickmap() 
ggsave(dmspols_1996_map, filename=file.path(figures_file_path, "dmspols_1996_map.png"), height=5, width=5)

dmspols_2012_map <- ggplot() +  
  geom_polygon(data=eth_adm0, aes(x=long, y=lat, group=group), fill="black") +
  geom_tile(data=dmspols_2012_spdf, aes(x=x, y=y, fill=value)) +
  scale_fill_gradient2(low="white", mid="yellow", high="red") +
  labs(title = "2012") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(hjust=0.5, face="bold")) +
  coord_quickmap() 
ggsave(dmspols_2012_map, filename=file.path(figures_file_path, "dmspols_2012_map.png"), height=5, width=5)



