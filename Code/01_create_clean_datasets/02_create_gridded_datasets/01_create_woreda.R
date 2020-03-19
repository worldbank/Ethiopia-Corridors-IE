# Create Points at Globcover Level

# Load Data --------------------------------------------------------------------
woreda <- readOGR(dsn = file.path(rawdata_file_path, "woreda_population", "HDX_CSA"), layer = "Ethioworeda")
woreda <- spTransform(woreda, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
woreda$uid <- 1:nrow(woreda)

woreda_blank <- woreda
woreda_blank@data <- woreda@data %>%
  dplyr::select(uid)

# Export -----------------------------------------------------------------------
saveRDS(woreda_blank, file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", "points.Rds"))
saveRDS(woreda_blank %>% st_as_sf(), file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", "polygons.Rds"))

saveRDS(woreda@data, file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", "woreda_details.Rds"))
