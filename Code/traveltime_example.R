# Travel Time Example Figures

# Figures to Illustrate Travel Time Calculation

RESOLUTION_KM <- 3
DATASET_TYPE <- "woreda_panel_hdx_csa"
WALKING_SPEED <- 5
# Load Data --------------------------------------------------------------------
roads <- readRDS(file.path(project_file_path, "Data", "FinalData", "roads", "RoadNetworkPanelData_1996_2016.Rds"))
woreda_wgs84 <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", "points.Rds"))
#woreda_wgs84 <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", "woreda_details.Rds"))

# Reproject to Ethiopia Projection ---------------------------------------------
# Reproject to UTM. Better for distance calculations (eg, for setting grid cell size)
roads <- spTransform(roads, UTM_ETH)
woreda <- spTransform(woreda_wgs84, UTM_ETH)

# Point Locations in Woreda ----------------------------------------------------
coords_df <- coordinates(woreda) %>%
  as.data.frame() %>%
  dplyr::rename(long = V1,
                lat = V2) 
woreda_points <- bind_cols(coords_df, woreda@data)
coordinates(woreda_points) <- ~long+lat
crs(woreda_points) <- CRS(UTM_ETH)

# Crete Raster BaseLayer -------------------------------------------------------
r <- raster(xmn=woreda@bbox[1,1], 
            xmx=woreda@bbox[1,2], 
            ymn=woreda@bbox[2,1], 
            ymx=woreda@bbox[2,2], 
            crs=UTM_ETH, 
            resolution = 3*1000)

# Pixels =======================================================================
roads_panel_df <- lapply(c(1996:2016), function(year){
  print(year)
  
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
  
  #### df
  roads_r_df <- as(roads_r, "SpatialPixelsDataFrame")
  roads_r_df <- as.data.frame(roads_r_df)
  colnames(roads_r_df) <- c("value", "x", "y")
  roads_r_df$year <- year
  return(roads_r_df)
}) %>% bind_rows()

roads_panel_df$value <- roads_panel_df$value %>% factor()

# 1996 =========================================================================
year = 1996
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

#### Add Year to Names
roads_r_1996 <- roads_r
cost_t_1996 <- cost_t

# 2016 =========================================================================
year = 2016
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

#### Add Year to Names
roads_r_2016 <- roads_r
cost_t_2016 <- cost_t

# Time and Paths ===============================================================
time_1996 <- costDistance(cost_t_1996,
                   woreda_points[1,],
                   woreda_points[210,]) %>% as.numeric()

path_1996 <- shortestPath(cost_t_1996,
                   woreda_points[1,],
                   woreda_points[210,],
             output = "SpatialLines")



time_2016 <- costDistance(cost_t_2016,
                          woreda_points[1,],
                          woreda_points[210,]) %>% as.numeric()

path_2016 <- shortestPath(cost_t_2016,
                          woreda_points[1,],
                          woreda_points[210,],
                          output = "SpatialLines")

# Figures ======================================================================
p <- ggplot() +
  geom_tile(data=roads_panel_df, 
            aes(x=x, y=y, fill=value), alpha=0.8) +
  scale_fill_manual(values = c("gray50", rev(brewer.pal(10, "RdYlGn")))) +
  theme_void() +
  coord_quickmap() +
  facet_wrap(~year, ncol = 3) 
ggsave(p, filename = file.path("~/Desktop/rastertest.png"), height=12, width=5)
  

