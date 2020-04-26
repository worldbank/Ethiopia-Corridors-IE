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
  precip <- raster(file.path(rawdata_file_path, "precipitation", "NOAA_PERSIAN", paste0("eth_precip_",year,"_avg_NOAA_PERSIANN-CDR_25km.tif")))
  
  if(grepl("grid", DATASET_TYPE)){
    points$precipitation <- raster::extract(precip, points, fun = mean, na.rm=T)
  } else{
    points$precipitation <- velox(precip)$extract(sp=points, fun=function(x){mean(x, na.rm=T)}) %>% as.numeric
    
    #### If NA, use centroid
    points_NA <- points[is.na(points$precipitation),] 
    points_NA <- coordinates(points_NA) %>%
      as.data.frame() %>%
      dplyr::rename(long = V1,
                    lat = V2)
    coordinates(points_NA) <- ~long+lat
    crs(points_NA) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    points_NA$precipitation <- raster::extract(precip, points_NA, fun = mean)
    
    points$precipitation[is.na(points$precipitation)] <- points_NA$precipitation
  }
  
  points$year <- year
  return(points@data)
}

points_all <- lapply(1992:2018, extract_precip_to_points, points) %>% bind_rows

# Export -----------------------------------------------------------------------
saveRDS(points_all, file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", "points_precipitation.Rds"))


