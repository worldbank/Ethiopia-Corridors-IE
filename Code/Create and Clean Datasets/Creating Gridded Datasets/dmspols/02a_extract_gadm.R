# Extract GADM to Points

# Load Data --------------------------------------------------------------------
points <- readRDS(file.path(outputs_for_grid, TYPE, "points.Rds"))
coordinates(points) <- ~long+lat
crs(points) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

eth_adm3 <- readRDS(file.path(rawdata_file_path, "GADM", "gadm36_ETH_3_sp.rds"))

# Add Data ---------------------------------------------------------------------
points_OVER_gadm <- sp::over(points, eth_adm3)
points$GADM_ID_1 <- points_OVER_gadm$NAME_1 %>% as.factor()
points$GADM_ID_2 <- points_OVER_gadm$NAME_2 %>% as.factor() 
points$GADM_ID_3 <- points_OVER_gadm$NAME_3 %>% as.factor()

# Export -----------------------------------------------------------------------
saveRDS(points@data, file.path(outputs_for_grid, TYPE, "points_gadm.Rds"))
