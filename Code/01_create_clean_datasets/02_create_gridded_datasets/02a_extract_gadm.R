# Extract GADM to Points

# Load Data --------------------------------------------------------------------
points <- readRDS(file.path(finaldata_file_path, DATASET_TYPE,"individual_datasets", "points.Rds"))
coordinates(points) <- ~long+lat
crs(points) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# GADM
eth_adm3 <- readRDS(file.path(rawdata_file_path, "GADM", "gadm36_ETH_3_sp.rds"))

# HDX WOREDA
woreda <- readOGR(dsn = file.path(rawdata_file_path, "woreda_population", "HDX_CSA"), layer = "Ethioworeda")
woreda <- spTransform(woreda, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
woreda$uid <- 1:nrow(woreda)

# Add Data ---------------------------------------------------------------------
points_OVER_gadm <- sp::over(points, eth_adm3)
points$GADM_ID_1 <- points_OVER_gadm$NAME_1 %>% as.factor()
#points$GADM_ID_2 <- points_OVER_gadm$NAME_2 %>% as.factor() 
#points$GADM_ID_3 <- points_OVER_gadm$NAME_3 %>% as.factor()

#points$GADM_GID_1 <- points_OVER_gadm$GID_1 %>% as.factor()
#points$GADM_GID_2 <- points_OVER_gadm$GID_2 %>% as.factor() 
#points$GADM_GID_3 <- points_OVER_gadm$GID_3 %>% as.factor()

# Add Other Woreda Data --------------------------------------------------------
points_OVER_woreda <- sp::over(points, woreda)

points$woreda_hdx_w_uid <- points_OVER_woreda$uid

points$woreda_hdx_z_name <- points_OVER_woreda$Z_NAME
points$woreda_hdx_z_code <- points_OVER_woreda$Z_CODE

points$woreda_hdx_w_name <- points_OVER_woreda$W_NAME
points$woreda_hdx_w_name <- points_OVER_woreda$W_NAME

points$woreda_hdx_w_pop2007 <- points_OVER_woreda$Pop2007
points$woreda_hdx_w_density <- points_OVER_woreda$Density

# Export -----------------------------------------------------------------------
saveRDS(points@data, file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", "points_gadm.Rds"))

