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
roads_2016 <- readOGR(dsn=".", layer=paste0("All_Network_","2016")) %>% spTransform(CRS(UTM_ETH))

year <- 2015
for(year in c(1997,1999,2001,2003,2005,2007,2009,2011,2013,2015)){
  roads_2016[[paste0("Speed",year)]] <- roads_2016[[paste0("Speed",year-1)]]
  roads_2016[[paste0("Speed",year)]][roads_2016$Complete_G == year] <- roads_2016[[paste0("Speed",year+1)]][roads_2016$Complete_G == year]
}

roads_2016$id <- 1

# Calculate Distance -----------------------------------------------------------
determine_distance_to_points <- function(year, points){
  
  print("* -------------------------")
  print(year)
  
  # Grab roads for relevant year
  roads <- roads_2016[roads_2016[[paste0("Speed",year)]] > 0,]
  
  # Loop through speeds. Subset road based on that speed. Add that speed to the
  # points dataframe
  for(speed in sort(unique(roads[[paste0("Speed", year)]]))){
    print("* -------------------------")
    print(speed)
    
    roads_subset <- roads[roads[[paste0("Speed", year)]] == speed,] %>% raster::aggregate(by="id")
    points[[paste0("distance_road_speed_", speed)]] <- gDistance_chunks(points, roads_subset, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS) 
  }
  
  points$year <- year
  
  rm(roads)
  return(points@data)
}

points_all <- lapply(1996:2016, determine_distance_to_points, points) %>% bind_rows

# Export -----------------------------------------------------------------------
saveRDS(points_all, file.path(outputs_for_grid, TYPE, "points_distance_roads_byspeed_2016file.Rds"))

