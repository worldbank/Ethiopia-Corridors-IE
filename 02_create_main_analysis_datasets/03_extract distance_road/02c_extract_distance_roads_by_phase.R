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
roads_sdf$id <- 1 # useful to have a variable the same for all obs when aggreagting roads later
roads_sdf <- roads_sdf %>% spTransform(CRS(UTM_ETH))

# Separate into Phases ---------------------------------------------------------
#roads_subset <- roads_subset
#roads_subset$id <- 1

#roads_sdf_p1 <- roads_sdf[roads_sdf$rsdp_phase %in% 1,] %>% raster::aggregate(by="id")
#roads_sdf_p2 <- roads_sdf[roads_sdf$rsdp_phase %in% 2,] %>% raster::aggregate(by="id")
#roads_sdf_p3 <- roads_sdf[roads_sdf$rsdp_phase %in% 3,] %>% raster::aggregate(by="id")
#roads_sdf_p4 <- roads_sdf[roads_sdf$rsdp_phase %in% 4,] %>% raster::aggregate(by="id")

roads_sdf_p1 <- roads_sdf[roads_sdf$rsdp_phase %in% 1,] %>% st_as_sf() %>% st_combine() %>% as("Spatial")
roads_sdf_p2 <- roads_sdf[roads_sdf$rsdp_phase %in% 2,] %>% st_as_sf() %>% st_combine() %>% as("Spatial")
roads_sdf_p3 <- roads_sdf[roads_sdf$rsdp_phase %in% 3,] %>% st_as_sf() %>% st_combine() %>% as("Spatial")
roads_sdf_p4 <- roads_sdf[roads_sdf$rsdp_phase %in% 4,] %>% st_as_sf() %>% st_combine() %>% as("Spatial")

roads_sdf_p1$id <- 1
roads_sdf_p2$id <- 1
roads_sdf_p3$id <- 1
roads_sdf_p4$id <- 1

# Distance to Phases -----------------------------------------------------------
points$distance_rsdp_phase1 <- gDistance_chunks(points, roads_sdf_p1, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)
points$distance_rsdp_phase2 <- gDistance_chunks(points, roads_sdf_p2, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)
points$distance_rsdp_phase3 <- gDistance_chunks(points, roads_sdf_p3, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)
points$distance_rsdp_phase4 <- gDistance_chunks(points, roads_sdf_p4, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)

# Export -----------------------------------------------------------------------
saveRDS(points@data, file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", "points_distance_rsdp_phases.Rds"))

