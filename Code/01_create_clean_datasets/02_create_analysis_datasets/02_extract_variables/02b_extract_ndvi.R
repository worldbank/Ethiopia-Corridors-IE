# Exact NDVI

# Extract average NDVI in polygons -- use all areas and areas just defined
# as cropland.

# NOTE: This script may take about an hour to run.

# Load Polygon Data ------------------------------------------------------------
polygons <- readRDS(file.path(finaldata_file_path, DATASET_TYPE,"individual_datasets", "polygons_all.Rds"))

# Determine Constant Cropland Areas --------------------------------------------
# Start with cropland in baseline year calling it cropland_constant. Then, loop 
# through all years of globcover. Here, we update cropland_constant, removing
# areas not cropland in the current year.

cropland_constant <- raster(file.path(rawdata_file_path, "esa_globcover", "scratch", "ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif"), band=(1992-1991)) %>% crop(extent(polygons))
cropland_constant[] <- as.numeric(cropland_constant[] %in% c(10,20,30))

for(year in 1993:2015){
  print(year)
  cropland_yyyy <- raster(file.path(rawdata_file_path, "esa_globcover", "scratch", "ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif"), band=(year-1991)) %>% crop(extent(polygons))
  cropland_constant[] <- as.numeric((cropland_constant[] %in% 1) & (cropland_yyyy[] %in% c(10,20,30))) 
}

cropland_constant[][cropland_constant[] %in% 0] <- NA


#### Attemps to polygonize/mask ndvi (instead of resampling)
#cropland_constant_polygon <- spex::qm_rasterToPolygons(cropland_constant, na.rm = TRUE)
#a <- st_union(cropland_constant_polygon[1:10000,])

#ndvi_masked <- mask(ndvi, cropland_constant_polygon)

# Add Data ---------------------------------------------------------------------
# Extract two things:
  # (1) Average NDVI within each polygon
  # (2) Average NDVI within each polygon, only considering cropland areas. So
  #     if a polygon only is partially covered by cropland, we only consider
  #     that area.

extract_ndvi_to_polygons <- function(year, polygons){
  print(year)
  
  #### Load NDVI
  if(year <= 1998) ndvi <- raster(file.path(rawdata_file_path, "NDVI", "Landsat", paste0("eth_ls5_ndvi_annual_",year,".tif")))  
  if(year > 1998) ndvi <- raster(file.path(rawdata_file_path, "NDVI", "Landsat", paste0("eth_ls7_ndvi_annual_",year,".tif")))  

  #### NDVI in Cropland Areas
  # If not cropland, make NA
  
  ndvi_resample <- resample(ndvi, cropland_constant)
  ndvi_cropland <- overlay(ndvi_resample, cropland_constant, fun=function(x,y){return(x*y)} )

  polygons$ndvi <- velox(ndvi)$extract(sp=polygons, fun=function(x){mean(x, na.rm=T)})
  polygons$ndvi_cropland <- velox(ndvi_cropland)$extract(sp=polygons, fun=function(x){mean(x, na.rm=T)})
  
  
  polygons$year <- year
  
  polygons$geometry <- NULL
  
  return(polygons)
}

polygons_ndvi <- lapply(1992:2018, extract_ndvi_to_polygons, polygons) %>% bind_rows

# Export -----------------------------------------------------------------------
saveRDS(polygons_ndvi, file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", "points_ndvi.Rds"))

