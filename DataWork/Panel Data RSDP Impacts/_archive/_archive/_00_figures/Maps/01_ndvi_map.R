
# Load Data --------------------------------------------------------------------
#### ADM
eth_adm0 <- getData('GADM', country='ETH', level=0)

#### NDVI and Cropland
year <- 2010

ndvi <- raster(file.path(rawdata_file_path, "NDVI", "MODIS Annual 1km", paste0("eth_ndvi_modis_1km_",year,".tif")))  
ndvi[] <- ndvi[] / 10000

cropland_area <- raster(file.path(rawdata_file_path, "esa_globcover", "scratch", "ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif"), band=(year-1991)) %>% crop(extent(ndvi))
cropland_area[] <- as.numeric(cropland_area[] %in% c(10,20,30))
ndvi_resample <- resample(ndvi, cropland_area)

ndvi_cropland <- overlay(ndvi_resample, cropland_area, fun=function(x,y){return(x*y)} )
ndvi_cropland[][ndvi_cropland[] %in% 0] <- NA

# Raster to DF -----------------------------------------------------------------
ndvi_spdf <- as(ndvi, "SpatialPixelsDataFrame")
ndvi_spdf <- as.data.frame(ndvi_spdf)
colnames(ndvi_spdf) <- c("value", "x", "y")

ndvi_cropland_spdf <- as(ndvi_cropland, "SpatialPixelsDataFrame")
ndvi_cropland_spdf <- as.data.frame(ndvi_cropland_spdf)
colnames(ndvi_cropland_spdf) <- c("value", "x", "y")

# Figures ----------------------------------------------------------------------
ndvi_map <- ggplot() +  
  geom_tile(data=ndvi_spdf, aes(x=x, y=y, fill=value)) +
  scale_fill_gradientn(colours = rev(terrain.colors(10))) +
  labs(title = "NDVI") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(hjust=0.5, face="bold")) +
  coord_quickmap() 
ggsave(ndvi_map, filename=file.path(figures_file_path, "ndvi_map.png"), height=5, width=5)

ndvi_cropland_map <- ggplot() +  
  geom_polygon(data=eth_adm0, aes(x=long, y=lat, group=group), fill="gray50", color=NA) +
  geom_tile(data=ndvi_cropland_spdf, aes(x=x, y=y, fill=value)) +
  scale_fill_gradientn(colours = rev(terrain.colors(10))) +
  labs(title = "NDVI in\nCropland Areas") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(hjust=0.5, face="bold")) +
  coord_quickmap() 
ggsave(ndvi_cropland_map, filename=file.path(figures_file_path, "ndvi_cropland_map.png"), height=5, width=5)
