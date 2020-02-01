# Travel Time

# Load Data --------------------------------------------------------------------
#### Travel Times
location_traveltimes <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", "woreda_traveltimes_distances.Rds"))

#### Population
woreda_pop <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", "woreda_details.Rds"))
woreda_pop <- woreda_pop@data %>%
  dplyr::select(uid, Pop2007) %>%
  dplyr::rename(dest_uid = uid,
                dest_pop2007 = Pop2007) %>%
  as.data.table()

# Merge Datasets ---------------------------------------------------------------
location_traveltimes <- merge(location_traveltimes, woreda_pop, by="dest_uid")

# Calculate Market Access ------------------------------------------------------
# Remove cases where travel time is zero
location_traveltimes <- location_traveltimes[!(location_traveltimes$travel_time %in% 0),]

# Population divided by travel time
location_traveltimes$pop_DIV_tt_theta1 <- location_traveltimes$dest_pop2007 / (location_traveltimes$travel_time^1)
location_traveltimes$pop_DIV_tt_theta5 <- location_traveltimes$dest_pop2007 / (location_traveltimes$travel_time^5)

# Calculate Market Access
MA_df <- location_traveltimes[, list(MA_pop2007_theta1 = sum(pop_DIV_tt_theta1), 
                                 MA_pop2007_theta5 = sum(pop_DIV_tt_theta5)), by=list(orig_uid, year)] %>%
  as.data.frame() %>%
  dplyr::rename(uid = orig_uid)

# Export -----------------------------------------------------------------------
saveRDS(MA_df, file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", "woreda_market_access.Rds"))



