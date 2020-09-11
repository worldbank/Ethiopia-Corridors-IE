# Distance to road, from separate shapefiles. Determine distance by speed
# and by surface type.

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

# Calculate Distance -----------------------------------------------------------
determine_distance_to_points <- function(year, points){
  
  print(paste(year, "--------------------------------------------------------"))
  
  # Load Roads
  roads_yyyy <- readOGR(dsn = file.path(project_file_path, "Data", "RawData", "RoadNetworkPanelDataV3_1996_2016_Revised"),
                       layer = paste0("All_Network_", year))
  roads_yyyy <- roads_yyyy %>% spTransform(CRS(UTM_ETH))
  if("Speed2006a" %in% names(roads_yyyy)) roads_yyyy$Speed2006 <- roads_yyyy$Speed2006a
    
  # Loop through speeds. Subset road based on that speed. Add that speed to the
  # points dataframe
  for(speed in sort(unique(roads_yyyy[[paste0("Speed", year)]]))){
    print(paste(speed, year, "-----------------------------------------------"))
    
    roads_subset <- roads_yyyy[roads_yyyy[[paste0("Speed", year)]] %in% speed,] #%>% raster::aggregate(by="id")
    roads_subset <- roads_subset %>% st_as_sf() %>% st_combine() %>% as("Spatial")
    roads_subset$id <- 1
    
    points[[paste0("distance_rdsep_speed_", speed)]] <- gDistance_chunks(points, roads_subset, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS) 
  }
  
  # Loop through road types
  for(surface_type in sort(unique(roads_yyyy$SURFACETYP))){
    print(paste(surface_type, year, "-----------------------------------------------"))
    
    roads_subset <- roads_yyyy[roads_yyyy$SURFACETYP %in% surface_type,] #%>% raster::aggregate(by="id")
    roads_subset <- roads_subset %>% st_as_sf() %>% st_combine() %>% as("Spatial")
    roads_subset$id <- 1
    
    points[[paste0("distance_rdsep_surftype_", surface_type)]] <- gDistance_chunks(points, roads_subset, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS) 
  }
  
  points$year <- year
  
  rm(roads)
  return(points@data)
}

points_all <- lapply(seq(from=1996, to=2016, by=2), determine_distance_to_points, points) %>% bind_rows

# Export -----------------------------------------------------------------------
saveRDS(points_all, file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", "points_distance_rdsep_speed_surface.Rds"))

