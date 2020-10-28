# Extract GADM to Points

# Load Data --------------------------------------------------------------------
points <- readRDS(file.path(panel_rsdp_imp_data_file_path, DATASET_TYPE, "individual_datasets", "points.Rds"))

# HDX WOREDA
woreda <- readRDS(file.path(data_file_path, "Woreda Population", "FinalData", "woreda.Rds"))
woreda <- spTransform(woreda, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Add Data ---------------------------------------------------------------------
points_OVER_woreda <- sp::over(points, woreda)

points$woreda_id <- points_OVER_woreda$woreda_id
# points$W_CODE <- points_OVER_woreda$W_CODE
# points$Z_CODE <- points_OVER_woreda$Z_CODE
# points$R_CODE <- points_OVER_woreda$R_CODE

points$woreda_pop2007 <- points_OVER_woreda$Pop2007
points$woreda_density2007 <- points_OVER_woreda$Density

# Export -----------------------------------------------------------------------
saveRDS(points@data, file.path(panel_rsdp_imp_data_file_path, DATASET_TYPE, "individual_datasets", "adm_units.Rds"))

