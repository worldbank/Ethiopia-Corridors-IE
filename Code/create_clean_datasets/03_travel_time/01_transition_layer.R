# Travel Time

# Load Data --------------------------------------------------------------------
roads <- readRDS(file.path(project_file_path, "Data", "FinalData", "roads", "RoadNetworkPanelData_1996_2016.Rds"))
ethiopia_adm3_wgs84 <- readRDS(file.path(project_file_path, "Data", "RawData", "GADM", "gadm36_ETH_3_sp.rds"))

# Reproject to Ethiopia Projection ---------------------------------------------
# Reproject to UTM. Better for distance calculations (eg, for setting grid cell size)
roads <- spTransform(roads, UTM_ETH)
ethiopia_adm3 <- spTransform(ethiopia_adm3_wgs84, UTM_ETH)

# Point Locations in Woreda ----------------------------------------------------
coords_df <- coordinates(ethiopia_adm3) %>%
  as.data.frame() %>%
  dplyr::rename(long = V1,
                lat = V2) 
ethiopia_adm3_points <- bind_cols(coords_df, ethiopia_adm3@data)
coordinates(ethiopia_adm3_points) <- ~long+lat
crs(ethiopia_adm3_points) <- CRS(UTM_ETH)

# Crete Raster BaseLayer -------------------------------------------------------
r <- raster(xmn=ethiopia_adm3@bbox[1,1], 
           xmx=ethiopia_adm3@bbox[1,2], 
           ymn=ethiopia_adm3@bbox[2,1], 
           ymx=ethiopia_adm3@bbox[2,2], 
       crs=UTM_ETH, 
       resolution = 50*1000)

# Function for Travel Times ----------------------------------------------------
calc_travel_time <- function(year, roads, ethiopia_adm3_points){
  
  print(paste(year, "--------------------------------------------------------"))
  
  #### Create speed variable for year
  speed_var <- paste0("Speed", year)
  roads$SpeedYYYY <- roads[[speed_var]]
  
  #### Sory by Speed
  # If multiple polylines interesect with a cell, velox uses the last polygon from
  # the spatial polygons dataframe. Consequently, we sort by speeds from slowest to
  # fastest so that velox uses the fastest speed.
  roads <- roads[order(roads$SpeedYYYY),] 
  
  #### Rasterize
  roads_r <- r
  roads_r[] <- 0
  roads_r_vx <- velox(roads_r)
  roads_r_vx$rasterize(roads, field="Speed1996", background=4) # background should be walking speed
  roads_r <- roads_r_vx$as.RasterLayer()
  
  #### Make Transition Layer
  # Roads is currently speed; calculate how long it takes to move across cell
  roads_r[] <- 1/roads_r[]
  
  cost_t <- transition(roads_r, function(x) sum(x), directions=8)
  cost_t <- geoCorrection(cost_t)
  
  #### Calculate Travel Time for Each Location
  tt_df <- lapply(1:nrow(ethiopia_adm3_points), function(i){
    #if((i %% 100) %in% 0) print(i)
    
    tt <- costDistance(cost_t,
                       ethiopia_adm3_points[i,],
                       ethiopia_adm3_points)
    
    df_out <- data.frame(origin_NAME_3 = ethiopia_adm3_points$NAME_3,
                         origin_GID_3 = ethiopia_adm3_points$GID_3,
                         travel_time = tt %>% as.numeric())
    
    df_out$dest_GID_3 <- ethiopia_adm3_points$GID_3[i]
    df_out$dest_NAME_3 <- ethiopia_adm3_points$NAME_3[i]
    return(df_out)
  }) %>% bind_rows
  
  tt_df$year <- year
  
  return(tt_df)
}

location_traveltimes <- lapply(1996:1998, calc_travel_time, roads, ethiopia_adm3_points) %>% 
  bind_rows() %>%
  as.data.table()

# Calculate Linear Distance ----------------------------------------------------
distance_df <- lapply(1:nrow(ethiopia_adm3_points), function(i){
  if((i %% 100) %in% 0) print(i)
  
  distance <- gDistance(ethiopia_adm3_points[i,],
                  ethiopia_adm3_points,
                  byid=T) %>% 
    as.vector()
  
  df_out <- data.frame(origin_NAME_3 = ethiopia_adm3_points$NAME_3,
                       origin_GID_3 = ethiopia_adm3_points$GID_3,
                       distance = distance)
  
  df_out$dest_GID_3 <- ethiopia_adm3_points$GID_3[i]
  df_out$dest_NAME_3 <- ethiopia_adm3_points$NAME_3[i]
  return(df_out)
}) %>% 
  bind_rows %>%
  as.data.table()

location_traveltimes <- merge(location_traveltimes, distance_df, by=c("origin_NAME_3",
                                                                      "origin_GID_3",
                                                                      "dest_GID_3",
                                                                      "dest_NAME_3"))

# Extract Nighttime Lights -----------------------------------------------------
ntl_df <- lapply(1996:2012, function(year){
  print(year)
  dmspols_i <- raster(file.path(rawdata_file_path, "Nighttime Lights", "DMSP_OLS","Stacked", "eth_dmspols_allyears.tif"),(year - 1991)) 
  
  dmspols_i_velox <- velox(dmspols_i)
  dmspols_mean <- dmspols_i_velox$extract(sp=ethiopia_adm3_wgs84, fun=function(x){mean(x, na.rm=T)}) %>% as.numeric
  dmspols_median <- dmspols_i_velox$extract(sp=ethiopia_adm3_wgs84, fun=function(x){median(x, na.rm=T)}) %>% as.numeric
  dmspols_max <- dmspols_i_velox$extract(sp=ethiopia_adm3_wgs84, fun=function(x){max(x, na.rm=T)}) %>% as.numeric
  dmspols_sum <- dmspols_i_velox$extract(sp=ethiopia_adm3_wgs84, fun=function(x){sum(x, na.rm=T)}) %>% as.numeric
  dmspols_Npositive <- dmspols_i_velox$extract(sp=ethiopia_adm3_wgs84, fun=function(x){sum(x>1, na.rm=T)}) %>% as.numeric
  
  df_out <- data.frame(dmspols_mean = dmspols_mean,
                       dmspols_median = dmspols_median,
                       dmspols_max = dmspols_max,
                       dmspols_sum = dmspols_sum,
                       dmspols_Npositive = dmspols_Npositive,
                       NAME_3 = ethiopia_adm3_wgs84$NAME_3,
                       GID_3 = ethiopia_adm3_wgs84$GID_3)
  
  return(df_out)
})





# Extract Nighttime Lights -----------------------------------------------------




# Export -----------------------------------------------------------------------
saveRDS()



location_traveltimes_1 <- location_traveltimes[location_traveltimes$year %in% 1996,]

plot(location_traveltimes_1$travel_time, location_traveltimes_1$distance)




