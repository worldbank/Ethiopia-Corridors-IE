# Extract GADM to Points

# Load Data --------------------------------------------------------------------
#### Grid points
points <- readRDS(file.path(finaldata_file_path, DATASET_TYPE,"individual_datasets", "points.Rds"))
if(grepl("grid", DATASET_TYPE)){
  coordinates(points) <- ~long+lat
  crs(points) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
}
points <- points %>% spTransform(CRS(UTM_ETH))

#### Cities
city_data <- read.csv(file.path(finaldata_file_path, "city_population", "city_pop_geocoded.csv"))
coordinates(city_data) <- ~lon+lat
crs(city_data) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
city_data <- city_data %>% spTransform(CRS(UTM_ETH))
city_data$id <- 1

# Aggregating only accepts SpatialPolyons, so buffer by small amount
city_data <- city_data %>% gBuffer(width=.1, byid=T)


# Distance to Cities -----------------------------------------------------------
#### Specific Cities
city_data_addisababa <- city_data[city_data$name %in% "Addis Ababa",]

#### Population Groups
## Three Groups
pop_group_list <- city_data$pop_1994 %>% quantile(probs = c(0.3333, 0.6666)) %>% as.numeric()
city_data$popsize_3groups <- 1
for(i in 1:length(pop_group_list)) city_data$popsize_3groups[city_data$pop_1994 >= pop_group_list[i]] <- (i+1)

city_data_popsize_3groups_g1 <- city_data[city_data$popsize_3groups %in% 1,] %>% raster::aggregate(by="id")
city_data_popsize_3groups_g2 <- city_data[city_data$popsize_3groups %in% 2,] %>% raster::aggregate(by="id")
city_data_popsize_3groups_g3 <- city_data[city_data$popsize_3groups %in% 3,] %>% raster::aggregate(by="id")

#### All Cities
city_data_all <- city_data %>% raster::aggregate(by="id")

# Calculate Distance -----------------------------------------------------------
points$distance_city_addisababa <- gDistance_chunks(points, city_data_addisababa, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)

points$distance_city_popsize_3groups_g1 <- gDistance_chunks(points, city_data_popsize_3groups_g1, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)
points$distance_city_popsize_3groups_g2 <- gDistance_chunks(points, city_data_popsize_3groups_g2, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)
points$distance_city_popsize_3groups_g3 <- gDistance_chunks(points, city_data_popsize_3groups_g3, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)

points$distance_city_all <- gDistance_chunks(points, city_data_all, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)

# Export -----------------------------------------------------------------------
saveRDS(points@data, file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", "points_distance_cities.Rds"))




