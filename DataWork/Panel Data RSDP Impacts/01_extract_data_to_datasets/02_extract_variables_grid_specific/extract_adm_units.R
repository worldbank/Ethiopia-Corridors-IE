# Extract GADM to Points

# Load Data --------------------------------------------------------------------
points <- readRDS(file.path(panel_rsdp_imp_data_file_path, DATASET_TYPE, "individual_datasets", "points.Rds"))

# HDX WOREDA
woreda <- readOGR(dsn = file.path(data_file_path, "Woreda Population", "RawData"), layer = "Ethioworeda")
woreda <- spTransform(woreda, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Add Data ---------------------------------------------------------------------
points_OVER_woreda <- sp::over(points, woreda)

points$woreda_code <- points_OVER_woreda$Z_CODE
points$zone_code <- points_OVER_woreda$Z_CODE
points$region_code <- points_OVER_woreda$W_CODE

points$woreda_pop2007 <- points_OVER_woreda$Pop2007
points$woreda_density2007 <- points_OVER_woreda$Density

# Export -----------------------------------------------------------------------
saveRDS(points@data, file.path(panel_rsdp_imp_data_file_path, DATASET_TYPE, "individual_datasets", "adm_units.Rds"))

