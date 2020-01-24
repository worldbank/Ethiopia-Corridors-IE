# Exact NDVI

# Extract average NDVI in polygons -- use all areas and areas just defined
# as cropland.

# Load Data --------------------------------------------------------------------
polygons <- readRDS(file.path(finaldata_file_path, DATASET_TYPE,"individual_datasets", "polygons.Rds"))

# Add Data ---------------------------------------------------------------------
extract_ndvi_to_polygons <- function(year, polygons){
  print(year)
  
  ndvi <- raster(file.path(rawdata_file_path, "NDVI", "MODIS Annual 1km", paste0("eth_ndvi_modis_1km_",year,".tif")))  
  ndvi[] <- ndvi[] / 10000
  
  # Only have cropland data until 2015. If area is not cropland, make the NDVI value NA.
  if(year %in% 2000:2015){
    cropland_area <- raster(file.path(rawdata_file_path, "esa_globcover", "scratch", "ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif"), band=(year-1991)) %>% crop(extent(ndvi))
    cropland_area[] <- as.numeric(cropland_area[] %in% c(10,20,30))
    ndvi_resample <- resample(ndvi, cropland_area)
    
    ndvi_cropland <- overlay(ndvi_resample, cropland_area, fun=function(x,y){return(x*y)} )
    ndvi_cropland[][ndvi_cropland[] %in% 0] <- NA
    
    polygons$ndvi_cropland <- velox(ndvi_cropland)$extract(sp=polygons, fun=function(x){mean(x, na.rm=T)}) %>% as.numeric
  } 
  
  polygons$ndvi <- velox(ndvi)$extract(sp=polygons, fun=function(x){mean(x, na.rm=T)})
  polygons$year <- year
  
  polygons$geometry <- NULL
  
  return(polygons)
}

polygons_ndvi <- lapply(2000:2018, extract_ndvi_to_polygons, polygons) %>% bind_rows

# Export -----------------------------------------------------------------------
saveRDS(polygons_ndvi, file.path(finaldata_file_path, DATASET_TYPE, "points_ndvi.Rds"))

