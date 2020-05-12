# Travel Time

source("~/Documents/Github/Ethiopia-Corridors-IE/Code/_ethiopia_ie_master.R")

RESOLUTION_KM <- 3
WALKING_SPEED <- 5

# Load Data --------------------------------------------------------------------
roads <- readRDS(file.path(project_file_path, "Data", "FinalData", "roads", "RoadNetworkPanelData_1996_2016.Rds"))
woreda_wgs84 <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", "points_all.Rds"))
gpw <- raster(file.path(rawdata_file_path, "gpw-v4-population-density-2000", "gpw-v4-population-density_2000.tif"))
gpw <- gpw %>% crop(woreda_wgs84)

# Location with largest population with woreda ---------------------------------
woreda_points <- lapply(1:nrow(woreda_wgs84), function(i){
  
  print(i)
  
  gpw_i <- gpw %>% 
    crop(woreda_wgs84[i,]) %>%
    mask(woreda_wgs84[i,])
  
  df <- gpw_i %>% coordinates() %>% as.data.frame()
  df$pop <- gpw_i[]
  
  loc_df <- df[which.max(df$pop),] %>%
    dplyr::select(x,y)
  
  if(nrow(loc_df) %in% 0){
    loc_df <- coordinates(woreda_wgs84[i,]) %>%
      as.data.frame() %>%
      dplyr::rename(x= V1,
                    y= V2)
  }
  
  
  return(loc_df)
  
}) %>% bind_rows()

woreda_points$uid <- woreda_wgs84$uid
coordinates(woreda_points) <- ~x+y
crs(woreda_points) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Reproject to Ethiopia Projection ---------------------------------------------
# Reproject to UTM. Better for distance calculations (eg, for setting grid cell size)
roads <- spTransform(roads, UTM_ETH)
woreda_points <- spTransform(woreda_points, UTM_ETH)
woreda <- spTransform(woreda_wgs84, UTM_ETH)

# Crete Raster BaseLayer -------------------------------------------------------
r <- raster(xmn=woreda@bbox[1,1], 
            xmx=woreda@bbox[1,2], 
            ymn=woreda@bbox[2,1], 
            ymx=woreda@bbox[2,2], 
            crs=UTM_ETH, 
            resolution = RESOLUTION_KM*1000)

# Function for Travel Times ----------------------------------------------------
calc_travel_time <- function(year, roads, woreda_points){
  
  print(paste(year, "--------------------------------------------------------"))
  
  #### Create speed variable for year
  speed_var <- paste0("Speed", year)
  roads$SpeedYYYY <- roads[[speed_var]]
  roads$SpeedYYYY[roads$SpeedYYYY %in% 0] <- WALKING_SPEED
  
  #### Sory by Speed
  # If multiple polylines interesect with a cell, velox uses the last polygon from
  # the spatial polygons dataframe. Consequently, we sort by speeds from slowest to
  # fastest so that velox uses the fastest speed.
  roads <- roads[order(roads$SpeedYYYY),] 
  
  #### Rasterize
  roads_r <- r
  roads_r[] <- 0
  roads_r_vx <- velox(roads_r)
  roads_r_vx$rasterize(roads, field="SpeedYYYY", background=WALKING_SPEED) # background should be walking speed (5km/hr); https://en.wikipedia.org/wiki/Preferred_walking_speed
  roads_r <- roads_r_vx$as.RasterLayer()
  
  #### Make Transition Layer
  # Roads is currently speed; calculate how long it takes to move across cell
  #roads_r[] <- 1/roads_r[]
  
  cost_t <- transition(roads_r, function(x) sum(x), directions=8)
  cost_t <- geoCorrection(cost_t, type="c")
  
  #### Calculate Travel Time for Each Location
  tt_df <- lapply(1:nrow(woreda_points), function(i){
    if((i %% 10) %in% 0) print(i)
    
    tt <- costDistance(cost_t,
                       woreda_points[i,],
                       woreda_points) %>% as.numeric()
    
    tt <- tt * RESOLUTION_KM # to get more accurate travel time???? TODO
    
    #tt <- costDistance(cost_t,
    #                   woreda_points[1,],
    #                   woreda_points[50,]) %>% as.numeric()
    
    #tt1 <- shortestPath(cost_t,
    #                   woreda_points[1,],
    #                   woreda_points[50,],
    #             output = "SpatialLines")
    
    #plot(tt1)
    #plot(roads_r,add=T)
    #plot(tt1,add=T)
    
    df_out <- data.frame(dest_uid = woreda_points$uid,
                         travel_time = tt)
    
    df_out$orig_uid <- woreda_points$uid[i]
    return(df_out)
  }) %>% bind_rows
  
  tt_df$year <- year
  
  return(tt_df)
}

location_traveltimes <- lapply(1996:2016, calc_travel_time, roads, woreda_points) %>% 
  bind_rows() %>%
  as.data.table()

# Calculate Linear Distance ----------------------------------------------------
distance_df <- lapply(1:nrow(woreda_points), function(i){
  if((i %% 100) %in% 0) print(i)
  
  distance <- gDistance(woreda_points[i,],
                        woreda_points,
                        byid=T) %>% 
    as.vector()
  
  df_out <- data.frame(dest_uid = woreda_points$uid,
                       distance = distance)
  
  df_out$orig_uid <- woreda_points$uid[i]
  return(df_out)
}) %>% 
  bind_rows %>%
  as.data.table()

location_traveltimes <- merge(location_traveltimes, distance_df, by=c("orig_uid",
                                                                      "dest_uid"))

# Export -----------------------------------------------------------------------
saveRDS(location_traveltimes, file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", "woreda_traveltimes_distances.Rds"))







