# NDVI GIF
# All Areas and cropland areas

# Load Data --------------------------------------------------------------------
eth_adm <- readRDS(file.path(data_file_path, "GADM", "RawData", "gadm36_ETH_0_sp.rds"))

#### NDVI and Cropland
year <- 2010

# Loop Through Years -----------------------------------------------------------
time <- 5 # time = 5 = 1996
for(year in rev(1996:2016)){
  print(year)
  
  # Load Data ------------------------------------------------------------------
  ## NDVI
  if(year %in% 1990:1998){
    ndvi <- raster(file.path(data_file_path, "NDVI", "RawData", "Landsat_1km", paste0("eth_1km_ls5_ndvi_annual_",year,".tif")))  
  } else{
    ndvi <- raster(file.path(data_file_path, "NDVI", "RawData", "Landsat_1km", paste0("eth_1km_ls7_ndvi_annual_",year,".tif")))  
  }
  
  ## Load Globcover Band
  if(year %in% 1996:2015){
    globcover <- raster(file.path(data_file_path, "Globcover", "RawData", "1992_2015_data", "ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif"), time) 
  } else{
    globcover <- raster(file.path(data_file_path, "Globcover", "RawData", "2016_2018_data", paste0("C3S-LC-L4-LCCS-Map-300m-P1Y-",year,"-v2.1.1.tif"))) 
  }
  
  # Prep Data ------------------------------------------------------------------
  ## Crop/Mask
  globcover <- globcover %>%
    crop(eth_adm) %>%
    crop(extent(ndvi)) %>%
    mask(eth_adm)
  
  ndvi <- ndvi %>%
    crop(eth_adm) %>%
    mask(eth_adm)
  
  ## NDVI Cropland
  cropland_area <- globcover
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
    labs(title = year) +
    theme_void() +
    theme(plot.background = element_rect(fill = "black",
                                         color = "black"),
          legend.position = "none",
          plot.title = element_text(hjust=0.5, face="bold", color = "white")) +
    coord_quickmap() 
  ggsave(ndvi_map, filename=file.path(data_file_path, "NDVI", "Outputs", "figures",
                                      "country_gif", "pngs",
                                      paste0("ndvi_",year,".png")), height=5, width=5)
  
  ndvi_cropland_map <- ggplot() +  
    geom_polygon(data = eth_adm,
                 aes(x = long, y = lat, group = group),
                 color = "white", fill = NA,
                 size = .15) +
    geom_polygon(data=eth_adm, aes(x=long, y=lat, group=group), fill="black", color=NA) +
    geom_tile(data=ndvi_cropland_spdf, aes(x=x, y=y, fill=value)) +
    scale_fill_gradientn(colours = rev(terrain.colors(10))) +
    labs(title = year) +
    theme_void() +
    theme(plot.background = element_rect(fill = "black",
                                         color = "black"),
          legend.position = "none",
          plot.title = element_text(hjust=0.5, face="bold", color = "white")) +
    coord_quickmap() 
  ggsave(ndvi_cropland_map, filename=file.path(data_file_path, "NDVI", "Outputs", "figures",
                                               "country_cropland_gif", "pngs",
                                               paste0("ndvi_cropland_",year,".png")), height=5, width=5)
  
  
}





