# Create Points at DMSPOLS Level

# Determine Pixels to Keep -----------------------------------------------------
dmspols <- raster(file.path(rawdata_file_path, "Nighttime Lights", "DMSP_OLS","Stacked", "eth_dmspols_allyears.tif"),1)

# Only keep cells with Value for NTL (Removes area not in Ethiopia according to GEE)
dmspols_df <- dmspols[] %>% as.data.frame
cells_to_keep <- !is.na(dmspols_df$.)

# Only keep cells within boundary of Ethiopia according to GADM
dmspols_coords <- coordinates(dmspols) %>% 
  as.data.frame %>%
  dplyr::rename(long = x) %>%
  dplyr::rename(lat = y)
coordinates(dmspols_coords) <- ~long+lat
crs(dmspols_coords) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

setwd(file.path(rawdata_file_path, "GADM"))
eth_adm3 <- getData('GADM', country='ETH', level=3)
points_OVER_gadm <- sp::over(dmspols_coords, eth_adm3)

cells_to_keep <- cells_to_keep & !is.na(points_OVER_gadm$NAME_0)

# Individual Points ------------------------------------------------------------
dmspols_coords <- coordinates(dmspols) %>% 
                    as.data.frame %>%
                    dplyr::rename(long = x) %>%
                    dplyr::rename(lat = y)
dmspols_coords <- dmspols_coords[cells_to_keep,]
dmspols_coords$cell_id <- 1:nrow(dmspols_coords)

saveRDS(dmspols_coords, file.path(outputs_for_grid, "DMSPOLS", "points.Rds"))

# Polygon of Points ------------------------------------------------------------

dmspols_poly <- polygonize(dmspols, na.rm=F)

dmspols_poly <- dmspols_poly[cells_to_keep,]
dmspols_poly$cell_id <- 1:nrow(dmspols_poly)

saveRDS(dmspols_poly, file.path(outputs_for_grid, "DMSPOLS", "polygons.Rds"))

# VIIRS Panel ------------------------------------------------------------------
dmspols_panel <- lapply(1:22, function(i){
    print(i)
    dmspols_i <- raster(file.path(rawdata_file_path, "Nighttime Lights", "DMSP_OLS","Stacked", "eth_dmspols_allyears.tif"),i)[] %>% 
      as.data.frame %>%
      dplyr::rename(dmspols = ".")
    dmspols_i$year <- i + 1991
    
    dmspols_i <- dmspols_i[cells_to_keep,]
    dmspols_i$cell_id <- 1:nrow(dmspols_i)
    
    return(dmspols_i)
  }) %>%
  bind_rows

saveRDS(dmspols_panel, file.path(outputs_for_grid, "DMSPOLS", "points_dmspols.Rds"))

