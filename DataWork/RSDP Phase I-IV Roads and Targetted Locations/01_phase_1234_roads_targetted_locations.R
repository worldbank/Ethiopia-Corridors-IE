# Create Line for RSDP 1-3 AND endpoints, focusing on larger roads.

# RSDP 1-3 focused on larger roads, where phase 4 focused on URRAP. Dates of 
# phases are as follows:
# -- RSDP I - From July 1997 to June 2002 (5 years plan)
# -- RSDP II - From July 2002 to June 2007 (5 years plan)
# -- RSDP III - From July 2007 to June 2010 (3 years plan)
# -- RSDP IV - From July 2010 to June 2015 (5 years plan)
# -- RSDP V - From July 2015 to June 2020 (Ongoing)

# Both phase III and IV overlap in 2010. Looking at data, we see a number of roads
# improved in 2010 that were the start of IV (lots of roads 20km/hr after improvement).
# Consequently, here we focus on roads completed in 2009 and before. 

# Reverse order of dataframe
rev_df <- function(df) df[dim(df)[1]:1,]

# Load and Prep Data -----------------------------------------------------------
roads_sdf <- readRDS(file.path(data_file_path, "RSDP Roads", "FinalData", "RoadNetworkPanelData_1996_2016.Rds"))
roads_sdf$id <- 1 # useful to have a variable the same for all obs when aggreagting roads later
roads_sdf <- roads_sdf %>% spTransform(CRS(UTM_ETH))

# Separate into Phases ---------------------------------------------------------
#roads_sdf_p123 <- roads_sdf[roads_sdf$Complete_G %in% 1997:2009,]
roads_sdf <- roads_sdf[roads_sdf$Speed2016 > roads_sdf$Speed1996,]

extract_line_endpoints_roadi <- function(i, road){
  # For road segment "i" of "road", extracts of the endpoints. Deals with issue
  # that some segment (one row of "road") may be comprised of multiple lines
  # that aren't in the same order.
  
  # 1. Extract road i
  # 2. Create list of dataframes of coordinates of lines
  # 3. Sort dataframes 
  # -- 3.1 Sort dataframes so average differece in latitude is positive (difference refers to previous point)
  # -- 3.2. Manually adjust - some segments above step doesn't work, so fix manually
  # 4. Append
  # 5. Grab endpoints
  # 6. Prep spatial points
  
  print(i)
  
  ## 1. Extract road_i
  road_i <- road[i,]
  
  ## 2. Create list of dataframes of coordinates of lines
  points_list <- road_i %>% 
    coordinates() %>% 
    purrr::flatten() %>%
    lapply(as.data.frame)
  
  ## 3. Sort
  # 3.1 Within each dataframe, sort so that longitude is assending. First, calculate
  # difference. If mostly negative, then reverse. This ensures (kinda) that each segment
  # is in the same order.
  points_list <- lapply(points_list, function(points_list_i){
    
    names(points_list_i) <- c("X1", "X2")
    
    X2_diff_median <- points_list_i$X2 %>% diff() %>% median()
    
    if(X2_diff_median < 0) points_list_i <- rev_df(points_list_i)
    
    return(points_list_i)
  }) 
  
  # 3.2 Manually adjust select segments
  # Made up of 3 segments, only use ones at ends
  if("Kebribeyah-Hatrishek-Aware" %in% road_i$LINKNAME & length(points_list) == 3) points_list[[1]] <- NULL
  
  # 4. Append
  points <- points_list %>% 
    bind_rows()
  
  ## 5. Grab endpoints
  points_end <- bind_rows(
    points %>% head(1),
    points %>% tail(1)
  )
  
  ## 6. Prep spatial points
  points_end$n_seg <- length(points_list)
  points_end$LINKNAME <- road_i$LINKNAME

  coordinates(points_end) <- ~X1+X2
  crs(points_end) <- crs(road)
  
  points_end$id <- i
  
  return(points_end)
}

endpoints <- lapply(1:nrow(roads_sdf), 
                    extract_line_endpoints_roadi, 
                    roads_sdf) %>% 
  do.call(what = "rbind")

endpoints$uid <- 1:nrow(endpoints)

# Check instances where segment/link made up of more than 1 line ---------------
# endpoints$id[endpoints$n_seg > 1] %>% unique()
# # 
# i <- 1091
# plot(roads_sdf[i,])
# plot(endpoints[endpoints$id %in% i,], add=T, col="red", pch=16)

# Add Regional Capitals --------------------------------------------------------
regional_capitals <- read.csv(file.path(data_file_path, "Region Capitals - MST", "RawData", "region_capitals.csv"), stringsAsFactors = F)
regional_capitals <- regional_capitals %>%
  distinct(latitude, longitude, .keep_all = T)
coordinates(regional_capitals) <- ~longitude+latitude
crs(regional_capitals) <- CRS("+init=epsg:4326")
regional_capitals <- regional_capitals %>% spTransform(crs(UTM_ETH))

regional_capitals_df <- regional_capitals %>%
  as.data.frame() %>%
  dplyr::rename(X1 = longitude,
                X2 = latitude)

# Create Unique Endpoints ------------------------------------------------------
endpoints_unique_df <- endpoints %>% 
  as.data.frame() %>%
  bind_rows(regional_capitals_df) %>%
  distinct(X1, X2, .keep_all = T)

coordinates(endpoints_unique_df) <- ~X1+X2
crs(endpoints_unique_df) <- crs(roads_sdf)

# Export -----------------------------------------------------------------------
roads_sdf <- spTransform(roads_sdf, CRS("+init=epsg:4326"))
endpoints <- spTransform(endpoints, CRS("+init=epsg:4326"))
endpoints_unique_df <- spTransform(endpoints_unique_df, CRS("+init=epsg:4326"))

saveRDS(roads_sdf, file.path(data_file_path, "RSDP Phase I-IV Roads and Targetted Locations", "FinalData", "roads_rsdp_i_iv.Rds"))
saveRDS(endpoints, file.path(data_file_path, "RSDP Phase I-IV Roads and Targetted Locations", "FinalData", "targetted_locations.Rds"))
saveRDS(endpoints_unique_df, file.path(data_file_path, "RSDP Phase I-IV Roads and Targetted Locations", "FinalData", "targetted_locations_unique.Rds"))


