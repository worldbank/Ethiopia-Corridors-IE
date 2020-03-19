# Extract GADM to Points

# Load Data --------------------------------------------------------------------
points <- readRDS(file.path(finaldata_file_path, DATASET_TYPE,"individual_datasets", "points.Rds"))

if(grepl("grid", DATASET_TYPE)){
  coordinates(points) <- ~long+lat
  crs(points) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
}

# Add Data ---------------------------------------------------------------------
extract_precip_to_points <- function(year, points){
  print(year)
  temp_avg <- raster(file.path(rawdata_file_path, "temperature", "FEWS_NET_FLDAS", paste0("eth_temp_",year,"_avg_FLDAS_10km.tif")))
  temp_min <- raster(file.path(rawdata_file_path, "temperature", "FEWS_NET_FLDAS", paste0("eth_temp_",year,"_min_FLDAS_10km.tif")))
  temp_max <- raster(file.path(rawdata_file_path, "temperature", "FEWS_NET_FLDAS", paste0("eth_temp_",year,"_max_FLDAS_10km.tif")))
  
  if(grepl("grid", DATASET_TYPE)){
    points$temp_avg <- extract(temp_avg, points)
    points$temp_min <- extract(temp_min, points)
    points$temp_max <- extract(temp_max, points)
    
  } else{
    points$temp_avg <- velox(temp_avg)$extract(sp=points, fun=function(x){mean(x, na.rm=T)}) %>% as.numeric
    points$temp_min <- velox(temp_min)$extract(sp=points, fun=function(x){mean(x, na.rm=T)}) %>% as.numeric
    points$temp_max <- velox(temp_max)$extract(sp=points, fun=function(x){mean(x, na.rm=T)}) %>% as.numeric
  }
  
  points$year <- year
  return(points@data)
}

points_all <- lapply(1992:2018, extract_precip_to_points, points) %>% bind_rows

# Export -----------------------------------------------------------------------
saveRDS(points_all, file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", "points_temperature.Rds"))

