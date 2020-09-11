# Distance to Roads by Speed Limit and Year

# Calculate distance of each point to the closest road for each speed limit in 
# each year.

# Load and Prep Data -----------------------------------------------------------
# Load data and reporject to Ethiopia UTM. UTM better for distance calculations 
# than WGS84.

#### Load points
points <- readRDS(file.path(finaldata_file_path, DATASET_TYPE,"individual_datasets", "points.Rds"))
if(grepl("grid", DATASET_TYPE)){
  coordinates(points) <- ~long+lat
  crs(points) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
}
points <- points %>% spTransform(CRS(UTM_ETH))

#### Load roads
roads_sdf <- readRDS(file.path(project_file_path, "Data", "FinalData", "roads", "RoadNetworkPanelData_1996_2016.Rds"))

## Restrict to improved
roads_sdf <- roads_sdf[roads_sdf$Speed2016 > roads_sdf$Speed1996,]

## Check
roads_sdf$id <- 1 # useful to have a variable the same for all obs when aggreagting roads later
roads_sdf <- roads_sdf %>% spTransform(CRS(UTM_ETH))

# Calculate Distance -----------------------------------------------------------
roads_sdf <- roads_sdf %>% st_as_sf() %>% st_combine() %>% as("Spatial")
roads_sdf$id <- 1
#roads_sdf <- raster::aggregate(roads_sdf, by="id")

points$distance_anyimproved_ever <- gDistance_chunks(points, roads_sdf, CHUNK_SIZE_DIST_ROADS)

# Export -----------------------------------------------------------------------
saveRDS(points@data, file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", "points_distance_anyroad_improved_ever.Rds"))

