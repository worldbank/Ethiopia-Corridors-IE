# Create Points at Globcover Level

# Determine Pixels to Keep -----------------------------------------------------
setwd(file.path(rawdata_file_path, "GADM"))
eth_adm3 <- getData('GADM', country='ETH', level=3)

globcover <- raster(file.path(rawdata_file_path, "esa_globcover", "scratch","ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif"),1) %>% crop(eth_adm3, snap="out")

# Only keep cells with Value for NTL (Removes area not in Ethiopia according to GEE)
globcover_df <- globcover[] %>% as.data.frame
cells_to_keep <- !is.na(globcover_df$.)

# Only keep cells within boundary of Ethiopia according to GADM
globcover_coords <- coordinates(globcover) %>% 
  as.data.frame %>%
  dplyr::rename(long = x) %>%
  dplyr::rename(lat = y)
coordinates(globcover_coords) <- ~long+lat
crs(globcover_coords) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

points_OVER_gadm <- sp::over(globcover_coords, eth_adm3)
cells_to_keep <- cells_to_keep & !is.na(points_OVER_gadm$NAME_0)

# Individual Points ------------------------------------------------------------
globcover_coords <- coordinates(globcover) %>% 
                    as.data.frame %>%
                    dplyr::rename(long = x) %>%
                    dplyr::rename(lat = y)
globcover_coords <- globcover_coords[cells_to_keep,]
globcover_coords$cell_id <- 1:nrow(globcover_coords)

saveRDS(globcover_coords, file.path(outputs_for_grid, "globcover", "points.Rds"))

# VIIRS Panel ------------------------------------------------------------------
rm(list=ls()[!(ls() %in% c("cells_to_keep", "rawdata_file_path", "outputs_for_grid", "eth_adm3"))])
for(i in 1:10) gc()

globcover_panel <- lapply(1:24, function(i){
    print(i)
  
    if(i %in% 1:24){
      globcover_i <- crop(raster(file.path(rawdata_file_path, "esa_globcover", "scratch","ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif"),i), eth_adm3, snap="out")[] %>% 
        as.data.frame %>%
        dplyr::rename(globcover = ".")
      globcover_i$year <- i + 1991
    }

    
    globcover_i <- globcover_i[cells_to_keep,]
    globcover_i$cell_id <- 1:nrow(globcover_i)
    
    return(globcover_i)
  }) %>%
  bind_rows

saveRDS(globcover_panel, file.path(outputs_for_grid, "globcover", "points_globcover.Rds"))

