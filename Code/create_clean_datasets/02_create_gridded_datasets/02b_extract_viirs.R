# Extract GADM to Points

# Load Data --------------------------------------------------------------------
points <- readRDS(file.path(finaldata_file_path, DATASET_TYPE,"individual_datasets", "points.Rds"))
coordinates(points) <- ~long+lat
crs(points) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

viirs_2012 <- raster(file.path(rawdata_file_path, "Nighttime Lights", "VIIRS", "Annual", "Median", "eth_viirs_2012_median.tif"))
viirs_2013 <- raster(file.path(rawdata_file_path, "Nighttime Lights", "VIIRS", "Annual", "Median", "eth_viirs_2013_median.tif"))
viirs_2014 <- raster(file.path(rawdata_file_path, "Nighttime Lights", "VIIRS", "Annual", "Median", "eth_viirs_2014_median.tif"))
viirs_2015 <- raster(file.path(rawdata_file_path, "Nighttime Lights", "VIIRS", "Annual", "Median", "eth_viirs_2015_median.tif"))
viirs_2016 <- raster(file.path(rawdata_file_path, "Nighttime Lights", "VIIRS", "Annual", "Median", "eth_viirs_2016_median.tif"))
viirs_2017 <- raster(file.path(rawdata_file_path, "Nighttime Lights", "VIIRS", "Annual", "Median", "eth_viirs_2017_median.tif"))
viirs_2018 <- raster(file.path(rawdata_file_path, "Nighttime Lights", "VIIRS", "Annual", "Median", "eth_viirs_2018_median.tif"))

# Add Data ---------------------------------------------------------------------
extract_viirs_to_points <- function(year, points){
  print(year)
  viirs <- raster(file.path(rawdata_file_path, "Nighttime Lights", "VIIRS", "Annual", "Median", paste0("eth_viirs_",year,"_median.tif")))
  points$viirs <- extract(viirs, points)
  points$year <- year
  return(points@data)
}

points_all <- lapply(2012:2018, extract_viirs_to_points, points) %>% bind_rows

# Export -----------------------------------------------------------------------
saveRDS(points_all, file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", "points_viirs.Rds"))

