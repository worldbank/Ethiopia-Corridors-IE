# Compute Market Access

# Load Data --------------------------------------------------------------------
location_traveltimes <- readRDS(file.path(panel_rsdp_imp_data_file_path, DATASET_TYPE, "individual_datasets", "travel_times_woredas.Rds"))
woreda <- readRDS(file.path(panel_rsdp_imp_data_file_path, DATASET_TYPE, "individual_datasets", "polygons_no_road_cut.Rds"))
gpw <- raster(file.path(data_file_path, "Gridded Population of the World", "RawData", "gpw-v4-population-density_2000.tif"))

# Woreda Population ------------------------------------------------------------
# Add woreda population from gpw. First, use geometry, then centroid. If population
# from geometry is NA, then use centroid

## Population using geometry
gpw <- crop(gpw, woreda)
woreda$pop_geom <- velox(gpw)$extract(sp = woreda, fun = function(x) sum(x, na.rm=T)) %>% as.vector()

## Population using centroid
# Use when geometry returns NA
woreda_pop_centroid <- gCentroid(woreda, byid=T)
woreda$pop_centroid <- extract(gpw, woreda_pop_centroid)

## If population using geometry is NA, use from centroid
woreda$pop_geom[is.na(woreda$pop_geom)] <- woreda$pop_centroid[is.na(woreda$pop_geom)]

## Cleanup
woreda@data <- woreda@data %>%
  dplyr::rename(dest_pop2000 = pop_geom,
                dest_uid = cell_id)

# Merge Data -------------------------------------------------------------------
location_traveltimes <- merge(location_traveltimes, 
                              woreda@data, 
                              by="dest_uid")

# Calculate Market Access ------------------------------------------------------
# Remove cases where travel time is zero
location_traveltimes <- location_traveltimes[!(location_traveltimes$travel_time %in% 0),]

# Population divided by travel time
location_traveltimes$pop_DIV_tt_theta1 <- location_traveltimes$dest_pop2000 / (location_traveltimes$travel_time^1)
location_traveltimes$pop_DIV_tt_theta2 <- location_traveltimes$dest_pop2000 / (location_traveltimes$travel_time^2)
location_traveltimes$pop_DIV_tt_theta5 <- location_traveltimes$dest_pop2000 / (location_traveltimes$travel_time^5)
location_traveltimes$pop_DIV_tt_theta8 <- location_traveltimes$dest_pop2000 / (location_traveltimes$travel_time^8)

#### Calculate Market Access
MA_df <- location_traveltimes[, list(
  MA_pop2000_theta1 = sum(pop_DIV_tt_theta1), 
  MA_pop2000_theta2 = sum(pop_DIV_tt_theta2), 
  MA_pop2000_theta5 = sum(pop_DIV_tt_theta5),
  MA_pop2000_theta8 = sum(pop_DIV_tt_theta8)
), by=list(orig_uid, year)] %>%
  as.data.frame() %>%
  dplyr::rename(cell_id = orig_uid)

#### Calculate Market Access - Excluding within 100km
location_traveltimes_far <- location_traveltimes[!(location_traveltimes$distance > 100 * 1000),]

MA_exclude_100km_df <- location_traveltimes_far[, list(
  MA_pop2000_theta1_exclude100km = sum(pop_DIV_tt_theta1), 
  MA_pop2000_theta2_exclude100km = sum(pop_DIV_tt_theta2),
  MA_pop2000_theta5_exclude100km = sum(pop_DIV_tt_theta5),
  MA_pop2000_theta8_exclude100km = sum(pop_DIV_tt_theta8)
), by=list(orig_uid, year)] %>%
  as.data.frame() %>%
  dplyr::rename(cell_id = orig_uid)

# Merge Market Access Measures -------------------------------------------------
MA_all_df <- merge(MA_df, MA_exclude_100km_df, by=c("cell_id" , "year"), all=T)

# Export -----------------------------------------------------------------------
saveRDS(MA_all_df, file.path(panel_rsdp_imp_data_file_path, DATASET_TYPE, "individual_datasets", "ma2_market_access.Rds"))



