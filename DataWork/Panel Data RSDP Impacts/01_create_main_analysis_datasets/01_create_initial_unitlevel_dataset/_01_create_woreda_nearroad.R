# Create Woreda Level Shapefile

# Create clean woreda level shapefile to merge into. Cut out areas within
# 1km of the road to prevent against capturing affects of just capturing the roads.

# Load Data --------------------------------------------------------------------
#### Woredas
woreda <- readOGR(dsn = file.path(rawdata_file_path, "woreda_population", "HDX_CSA"), layer = "Ethioworeda")
woreda <- spTransform(woreda, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
woreda$uid <- 1:nrow(woreda)

woreda_blank <- woreda
woreda_blank@data <- woreda@data %>%
  dplyr::select(uid)

woreda_blank_utm <- spTransform(woreda_blank, CRS(UTM_ETH))

#### Roads
roads <- readRDS(file.path(project_file_path, "Data", "FinalData", "roads", "RoadNetworkPanelData_1996_2016.Rds"))

# Improved roads
roads$improved <- roads$Speed2016 > roads$Speed1996
roads <- roads[roads$improved %in% T,]

# Project
roads <- spTransform(roads, CRS(UTM_ETH))

# Cut Road Areas Out -----------------------------------------------------------
roads_1km_buff <- gBuffer_chunks(roads, width=1000, 51)
roads_5km_buff <- gBuffer_chunks(roads, width=5000, 51)

woreda_clean <- lapply(1:nrow(woreda_blank_utm), function(i){
  print(i) # Where are we?

  woreda_blank_utm_i <- woreda_blank_utm[i,]
  
  # Cleans up self-intersection issues
  woreda_blank_utm_i <- gBuffer(woreda_blank_utm_i, byid=T, width=0)
  
  roads_1km_buff_i <- raster::intersect(roads_1km_buff, woreda_blank_utm_i)
  roads_5km_buff_i <- raster::intersect(roads_5km_buff, woreda_blank_utm_i)
  
  # Catch errors in removing roads from polygons. An error occurs when, through
  # erase/cropping process, no part of the woreda is left. These are small
  # woredas with lots of roads.
  woreda_blank_utm_i_e <- NULL
  tryCatch({  
    
    if(is.null(roads_1km_buff_i)){
      woreda_blank_utm_i_e <- NULL # If no road intersection, remove; no improved roads.
    } else{
      woreda_blank_utm_i_e <- woreda_blank_utm_i %>% erase(roads_1km_buff_i) %>% crop(roads_5km_buff_i)
    }

    return(woreda_blank_utm_i_e)
  }, 
  error = function(e) return(NULL)
  )
  
  
  return(woreda_blank_utm_i_e)
  
}) %>% 
  unlist() %>% # remove NULLs
  do.call(what="rbind")

#### Cropped Only, around larger buffer, so still capture roads within
woreda_cropped_only <- lapply(1:nrow(woreda_blank_utm), function(i){
  print(i) # Where are we?
  
  woreda_blank_utm_i <- woreda_blank_utm[i,]
  
  # Cleans up self-intersection issues
  woreda_blank_utm_i <- gBuffer(woreda_blank_utm_i, byid=T, width=0)
  
  #roads_1km_buff_i <- raster::intersect(roads_1km_buff, woreda_blank_utm_i)
  roads_5km_buff_i <- raster::intersect(roads_5km_buff, woreda_blank_utm_i)
  
  # Catch errors in removing roads from polygons. An error occurs when, through
  # erase/cropping process, no part of the woreda is left. These are small
  # woredas with lots of roads.
  woreda_blank_utm_i_e <- NULL
  tryCatch({  
    
    if(is.null(roads_5km_buff_i)){
      woreda_blank_utm_i_e <- NULL # If no road intersection, remove; no improved roads.
    } else{
      woreda_blank_utm_i_e <- woreda_blank_utm_i %>% crop(roads_5km_buff_i)
    }
    
    return(woreda_blank_utm_i_e)
  }, 
  error = function(e) return(NULL)
  )
  
  
  return(woreda_blank_utm_i_e)
  
}) %>% 
  unlist() %>% # remove NULLs
  do.call(what="rbind")

woreda_clean <- spTransform(woreda_clean, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
woreda_cropped_only <- spTransform(woreda_cropped_only, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Export -----------------------------------------------------------------------
saveRDS(woreda_clean, file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", "points.Rds"))
saveRDS(woreda_clean %>% st_as_sf(), file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", "polygons.Rds"))
saveRDS(woreda@data, file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", "woreda_details.Rds"))

saveRDS(woreda_cropped_only, file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", "points_cropped_only.Rds"))



