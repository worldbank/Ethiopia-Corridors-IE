# Compute Market Access

# Load Data --------------------------------------------------------------------
location_traveltimes <- readRDS(file.path(panel_rsdp_imp_data_file_path, DATASET_TYPE, "individual_datasets", "ma1_travel_times_for_market_access.Rds"))
woreda <- readRDS(file.path(panel_rsdp_imp_data_file_path, DATASET_TYPE, "individual_datasets", "polygons_no_road_cut.Rds"))
gpw <- raster(file.path(data_file_path, "Gridded Population of the World", "RawData", "gpw-v4-population-density_2000.tif"))
ntl <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", "RawData", "Individual Files", "eth_dmspols_1996.tif"))
globcover_urban <- raster(file.path(data_file_path, "Globcover", "RawData", "1992_2015_data", "ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif"), (1996 - 1991))

globcover_urban <- crop(globcover_urban, woreda)
globcover_urban[] <- as.numeric(globcover_urban[] %in% 190)

# Extract Population -----------------------------------------------------------
# Add woreda population from gpw. First, use geometry, then centroid. If population
# from geometry is NA, then use centroid

## Population using geometry
gpw <- crop(gpw, woreda)
woreda$pop_geom <- velox(gpw)$extract(sp = woreda, fun = function(x) sum(x, na.rm=T)) %>% as.vector()

## Population using centroid
# Use when geometry returns NA
woreda_pop_centroid <- gCentroid(woreda, byid=T)
woreda$pop_centroid <- raster::extract(gpw, woreda_pop_centroid)

## If population using geometry is NA, use from centroid
woreda$pop_geom[is.na(woreda$pop_geom)] <- woreda$pop_centroid[is.na(woreda$pop_geom)]

## Cleanup
woreda@data <- woreda@data %>%
  dplyr::rename(dest_pop2000 = pop_geom,
                dest_uid = cell_id)

# Extract NTL ------------------------------------------------------------------
# Add woreda population from gpw. First, use geometry, then centroid. If population
# from geometry is NA, then use centroid

## Population using geometry
ntl <- crop(ntl, woreda)
woreda$ntl_geom <- velox(ntl)$extract(sp = woreda, fun = function(x) sum(x, na.rm=T)) %>% as.vector()

## Population using centroid
# Use when geometry returns NA
woreda_ntl_centroid <- gCentroid(woreda, byid=T)
woreda$ntl_centroid <- raster::extract(ntl, woreda_ntl_centroid)

## If population using geometry is NA, use from centroid
woreda$ntl_geom[is.na(woreda$ntl_geom)] <- woreda$ntl_centroid[is.na(woreda$ntl_geom)]

## Cleanup
woreda@data <- woreda@data %>%
  dplyr::rename(dest_ntl1996 = ntl_geom)

# Extract Globcover Urban ------------------------------------------------------
# Add woreda population from gpw. First, use geometry, then centroid. If population
# from geometry is NA, then use centroid

## Population using geometry
woreda$gcu_geom <- velox(globcover_urban)$extract(sp = woreda, fun = function(x) sum(x, na.rm=T)) %>% as.vector()

## Population using centroid
# Use when geometry returns NA
woreda_gcu_centroid <- gCentroid(woreda, byid=T)
woreda$gcu_centroid <- raster::extract(ntl, woreda_gcu_centroid)

## If population using geometry is NA, use from centroid
woreda$gcu_geom[is.na(woreda$gcu_geom)] <- woreda$gcu_centroid[is.na(woreda$gcu_geom)]

## Cleanup
woreda@data <- woreda@data %>%
  dplyr::rename(dest_gc_urban1996 = gcu_geom)

# Merge Data -------------------------------------------------------------------
location_traveltimes <- merge(location_traveltimes, 
                              woreda@data, 
                              by="dest_uid")

# Calculate Market Access ------------------------------------------------------
# Remove cases where travel time is zero
location_traveltimes <- location_traveltimes[!(location_traveltimes$travel_time %in% 0),]

# Travel time to minutes
location_traveltimes$travel_time <- location_traveltimes$travel_time * 60

# Population divided by travel time
location_traveltimes$pop_DIV_tt_theta1 <- location_traveltimes$dest_pop2000 / (location_traveltimes$travel_time^1)
location_traveltimes$pop_DIV_tt_theta2 <- location_traveltimes$dest_pop2000 / (location_traveltimes$travel_time^2)
location_traveltimes$pop_DIV_tt_theta5 <- location_traveltimes$dest_pop2000 / (location_traveltimes$travel_time^5)
location_traveltimes$pop_DIV_tt_theta8 <- location_traveltimes$dest_pop2000 / (location_traveltimes$travel_time^8)

location_traveltimes$ntl_DIV_tt_theta1 <- location_traveltimes$dest_ntl1996 / (location_traveltimes$travel_time^1)
location_traveltimes$ntl_DIV_tt_theta2 <- location_traveltimes$dest_ntl1996 / (location_traveltimes$travel_time^2)
location_traveltimes$ntl_DIV_tt_theta5 <- location_traveltimes$dest_ntl1996 / (location_traveltimes$travel_time^5)
location_traveltimes$ntl_DIV_tt_theta8 <- location_traveltimes$dest_ntl1996 / (location_traveltimes$travel_time^8)

location_traveltimes$gcu_DIV_tt_theta1 <- location_traveltimes$dest_gc_urban1996 / (location_traveltimes$travel_time^1)
location_traveltimes$gcu_DIV_tt_theta2 <- location_traveltimes$dest_gc_urban1996 / (location_traveltimes$travel_time^2)
location_traveltimes$gcu_DIV_tt_theta5 <- location_traveltimes$dest_gc_urban1996 / (location_traveltimes$travel_time^5)
location_traveltimes$gcu_DIV_tt_theta8 <- location_traveltimes$dest_gc_urban1996 / (location_traveltimes$travel_time^8)

#### Calculate Market Access
MA_df <- location_traveltimes[, list(
  MA_pop2000_theta1 = sum(pop_DIV_tt_theta1), 
  MA_pop2000_theta2 = sum(pop_DIV_tt_theta2), 
  MA_pop2000_theta5 = sum(pop_DIV_tt_theta5),
  MA_pop2000_theta8 = sum(pop_DIV_tt_theta8),
  
  MA_ntl2000_theta1 = sum(ntl_DIV_tt_theta1), 
  MA_ntl2000_theta2 = sum(ntl_DIV_tt_theta2), 
  MA_ntl2000_theta5 = sum(ntl_DIV_tt_theta5),
  MA_ntl2000_theta8 = sum(ntl_DIV_tt_theta8),
  
  MA_gcu2000_theta1 = sum(gcu_DIV_tt_theta1), 
  MA_gcu2000_theta2 = sum(gcu_DIV_tt_theta2), 
  MA_gcu2000_theta5 = sum(gcu_DIV_tt_theta5),
  MA_gcu2000_theta8 = sum(gcu_DIV_tt_theta8)
), by=list(orig_uid, year)] %>%
  as.data.frame() %>%
  dplyr::rename(cell_id = orig_uid)

#### Calculate Market Access - Excluding within 100km
location_traveltimes_far <- location_traveltimes[!(location_traveltimes$distance > 100 * 1000),]

MA_exclude_100km_df <- location_traveltimes_far[, list(
  MA_pop2000_theta1_exclude100km = sum(pop_DIV_tt_theta1), 
  MA_pop2000_theta2_exclude100km = sum(pop_DIV_tt_theta2),
  MA_pop2000_theta5_exclude100km = sum(pop_DIV_tt_theta5),
  MA_pop2000_theta8_exclude100km = sum(pop_DIV_tt_theta8),
  
  MA_ntl2000_theta1_exclude100km = sum(ntl_DIV_tt_theta1), 
  MA_ntl2000_theta2_exclude100km = sum(ntl_DIV_tt_theta2), 
  MA_ntl2000_theta5_exclude100km = sum(ntl_DIV_tt_theta5),
  MA_ntl2000_theta8_exclude100km = sum(ntl_DIV_tt_theta8),
  
  MA_gcu2000_theta1_exclude100km = sum(gcu_DIV_tt_theta1), 
  MA_gcu2000_theta2_exclude100km = sum(gcu_DIV_tt_theta2), 
  MA_gcu2000_theta5_exclude100km = sum(gcu_DIV_tt_theta5),
  MA_gcu2000_theta8_exclude100km = sum(gcu_DIV_tt_theta8)
), by=list(orig_uid, year)] %>%
  as.data.frame() %>%
  dplyr::rename(cell_id = orig_uid)

# Merge Market Access Measures -------------------------------------------------
MA_all_df <- merge(MA_df, MA_exclude_100km_df, by=c("cell_id" , "year"), all=T)

# Export -----------------------------------------------------------------------
saveRDS(MA_all_df, file.path(panel_rsdp_imp_data_file_path, DATASET_TYPE, "individual_datasets", "ma2_market_access.Rds"))



