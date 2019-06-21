# Extract GADM to Points

# Load Data --------------------------------------------------------------------
points <- readRDS(file.path(outputs_for_grid, TYPE, "points.Rds"))
coordinates(points) <- ~long+lat
crs(points) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

setwd(file.path(rawdata_file_path, "GADM"))
eth_adm3 <- getData('GADM', country='ETH', level=3)

# Add Data ---------------------------------------------------------------------
points_OVER_gadm <- sp::over(points, eth_adm3)
points$GADM_ID_1 <- points_OVER_gadm$GID_1 %>% as.factor %>% as.numeric
points$GADM_ID_2 <- points_OVER_gadm$GID_2 %>% as.factor %>% as.numeric
points$GADM_ID_3 <- points_OVER_gadm$GID_3 %>% as.factor %>% as.numeric

# Export -----------------------------------------------------------------------
saveRDS(points@data, file.path(outputs_for_grid, TYPE, "points_gadm.Rds"))

