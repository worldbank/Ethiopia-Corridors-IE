# Extract GADM to Points

# Load Data --------------------------------------------------------------------
points <- readRDS(file.path(finaldata_file_path, DATASET_TYPE,"individual_datasets", "points.Rds"))
points <- spTransform(points, CRS(UTM_ETH))

points$area_polygon <- gArea(points, byid=T) %>% as.vector()
points$area_polygon <- points$area_polygon / 1000^2

# Export -----------------------------------------------------------------------
saveRDS(points@data, file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", "points_area.Rds"))
