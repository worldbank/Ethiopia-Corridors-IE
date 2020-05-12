# Calculate Distance from Road to Points (Road by Speed Limit)

for(i in 1:10) gc()
# Load Points ------------------------------------------------------------------
points <- readRDS(file.path(outputs_for_grid, TYPE, "points.Rds"))
coordinates(points) <- ~long+lat
crs(points) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

points <- points %>% spTransform(CRS(UTM_ETH))

# Load Roads -------------------------------------------------------------------
#### Load
setwd(file.path(rawdata_file_path, "RoadNetworkPanelDataV3_1996_2016_Revised"))
for(year in seq(from=1996, to=2016, by=2)){
  assign(paste0("roads_", year),
        readOGR(dsn=".", layer=paste0("All_Network_",year)) %>% spTransform(CRS(UTM_ETH)))
}

# Correct speed variable for 2006 dataset
roads_2006$Speed2006 <- roads_2006$Speed2006a

#### Add ID of 1. Use when aggregating
roads_1996$id <- 1
roads_1998$id <- 1
roads_2000$id <- 1
roads_2002$id <- 1
roads_2004$id <- 1
roads_2006$id <- 1
roads_2008$id <- 1
roads_2010$id <- 1
roads_2012$id <- 1
roads_2014$id <- 1
roads_2016$id <- 1

# Calculate Distance -----------------------------------------------------------
determine_distance_to_points <- function(year, points){
  
  print(year)
  
  # Grab roads for relevant year
  roads <- eval(parse(text=paste0("roads_", year)))
  
  # Loop through speeds. Subset road based on that speed. Add that speed to the
  # points dataframe
  for(speed in sort(unique(roads[[paste0("Speed", year)]]))){
    roads_subset <- roads[roads[[paste0("Speed", year)]] == speed,] %>% raster::aggregate(by="id")
    points[[paste0("distance_road_speed_", speed)]] <- gDistance_chunks(points, roads_subset, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS) 
  }
  
  points$year <- year
  
  rm(roads)
  return(points@data)
}

points_all <- lapply(seq(1996, 2016, 2), determine_distance_to_points, points) %>% bind_rows

# Create odd years dataset, but don't include 2017
points_all_oddyears <- points_all
points_all_oddyears$year <- points_all_oddyears$year + 1
points_all_oddyears <- points_all_oddyears[points_all_oddyears$year != 2017,]

points_evenodd <- bind_rows(points_all, points_all_oddyears)

# Export -----------------------------------------------------------------------
saveRDS(points_evenodd, file.path(outputs_for_grid, TYPE, "points_distance_roads_byspeed.Rds"))

