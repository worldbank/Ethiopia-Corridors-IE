# Extract GADM to Points

# Load Data --------------------------------------------------------------------
points <- readRDS(file.path(finaldata_file_path, DATASET_TYPE,"individual_datasets", "points.Rds"))

if(grepl("grid", DATASET_TYPE)){
  coordinates(points) <- ~long+lat
  crs(points) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
}

# Add Data ---------------------------------------------------------------------
extract_viirs_to_points <- function(year, points){
  print(year)
  viirs_median <- raster(file.path(rawdata_file_path, "Nighttime Lights", "VIIRS", "Annual", "median", paste0("eth_viirs_median_",year,".tif")))
  viirs_mean <- raster(file.path(rawdata_file_path, "Nighttime Lights", "VIIRS", "Annual", "mean", paste0("eth_viirs_mean_",year,".tif")))
  viirs_max <- raster(file.path(rawdata_file_path, "Nighttime Lights", "VIIRS", "Annual", "max", paste0("eth_viirs_max_",year,".tif")))
  
  if(grepl("grid", DATASET_TYPE)){
    points$viirs_median <- raster::extract(viirs_median, points)
    points$viirs_mean <- raster::extract(viirs_mean, points)
    points$viirs_max <- raster::extract(viirs_max, points)
    
  } else{
    points$viirs_median <- velox(viirs_median)$extract(sp=points, fun=function(x){median(x, na.rm=T)}) %>% as.numeric
    points$viirs_mean <- velox(viirs_mean)$extract(sp=points, fun=function(x){mean(x, na.rm=T)}) %>% as.numeric
    points$viirs_max <- velox(viirs_max)$extract(sp=points, fun=function(x){max(x, na.rm=T)}) %>% as.numeric
  }
  
  points$year <- year
  return(points@data)
}

points_all <- lapply(2012:2019, extract_viirs_to_points, points) %>% bind_rows

# Export -----------------------------------------------------------------------
saveRDS(points_all, file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", "points_viirs.Rds"))

