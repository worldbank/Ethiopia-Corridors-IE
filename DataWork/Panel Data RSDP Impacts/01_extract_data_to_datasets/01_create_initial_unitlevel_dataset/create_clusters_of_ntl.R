# Identify Clusters of Lights

set.seed(42)
# Load Data --------------------------------------------------------------------
dmspols_1996 <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", "RawData", "Individual Files", "eth_dmspols_1996.tif"))
dmspols_1997 <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", "RawData", "Individual Files", "eth_dmspols_1997.tif"))
dmspols_1998 <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", "RawData", "Individual Files", "eth_dmspols_1998.tif"))
dmspols_1999 <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", "RawData", "Individual Files", "eth_dmspols_1999.tif"))
dmspols_2000 <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", "RawData", "Individual Files", "eth_dmspols_2000.tif"))
dmspols_2001 <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", "RawData", "Individual Files", "eth_dmspols_2001.tif"))
dmspols_2002 <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", "RawData", "Individual Files", "eth_dmspols_2002.tif"))
dmspols_2003 <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", "RawData", "Individual Files", "eth_dmspols_2003.tif"))
dmspols_2004 <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", "RawData", "Individual Files", "eth_dmspols_2004.tif"))
dmspols_2005 <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", "RawData", "Individual Files", "eth_dmspols_2005.tif"))
dmspols_2006 <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", "RawData", "Individual Files", "eth_dmspols_2006.tif"))
dmspols_2007 <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", "RawData", "Individual Files", "eth_dmspols_2007.tif"))
dmspols_2008 <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", "RawData", "Individual Files", "eth_dmspols_2008.tif"))
dmspols_2009 <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", "RawData", "Individual Files", "eth_dmspols_2009.tif"))
dmspols_2010 <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", "RawData", "Individual Files", "eth_dmspols_2010.tif"))
dmspols_2011 <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", "RawData", "Individual Files", "eth_dmspols_2011.tif"))
dmspols_2012 <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", "RawData", "Individual Files", "eth_dmspols_2012.tif"))
dmspols_2013 <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", "RawData", "Individual Files", "eth_dmspols_2013.tif"))

# Define raster layer of clusters ----------------------------------------------
dmspols_2013_binary <- dmspols_2013
dmspols_2013_binary[] <- as.numeric(dmspols_1996[] > 0 |
                                      dmspols_1997[] > 0 |
                                      dmspols_1998[] > 0 |
                                      dmspols_1999[] > 0 |
                                      dmspols_2000[] > 0 |
                                      dmspols_2001[] > 0 |
                                      dmspols_2002[] > 0 |
                                      dmspols_2003[] > 0 |
                                      dmspols_2004[] > 0 |
                                      dmspols_2005[] > 0 |
                                      dmspols_2006[] > 0 |
                                      dmspols_2007[] > 0 |
                                      dmspols_2008[] > 0 |
                                      dmspols_2009[] > 0 |
                                      dmspols_2010[] > 0 |
                                      dmspols_2011[] > 0 |
                                      dmspols_2012[] > 0 |
                                      dmspols_2013[] > 0)
# dmspols_2013_binary[] <- as.numeric(dmspols_1996[] >= 1 |
#                                       dmspols_1997[] >= 1 |
#                                       dmspols_1998[] >= 1 |
#                                       dmspols_1999[] >= 1 |
#                                       dmspols_2000[] >= 1 |
#                                       dmspols_2001[] >= 1 |
#                                       dmspols_2002[] >= 1 |
#                                       dmspols_2003[] >= 1 |
#                                       dmspols_2004[] >= 1 |
#                                       dmspols_2005[] >= 1 |
#                                       dmspols_2006[] >= 1 |
#                                       dmspols_2007[] >= 1 |
#                                       dmspols_2008[] >= 1 |
#                                       dmspols_2009[] >= 1 |
#                                       dmspols_2010[] >= 1 |
#                                       dmspols_2011[] >= 1 |
#                                       dmspols_2012[] >= 1 |
#                                       dmspols_2013[] >= 1)
dmspols_2013_clumps <- clump(dmspols_2013_binary, directions=8)

clumps_unique_values <- unique(dmspols_2013_clumps[])[!is.na(unique(dmspols_2013_clumps[]))]

# Polygonize clusters ----------------------------------------------------------
## Polgyzonize raster grids
clump_sp_all <- rasterToPolygons(dmspols_2013_clumps, 
                                 n=4, 
                                 na.rm=TRUE, 
                                 digits=12, 
                                 dissolve=F)
clump_sp_all$cluster_n_cells <- 1

## Collapse grids of same cluster
clumps_sp <- raster::aggregate(clump_sp_all, 
                               by="clumps",
                               list(list(sum, 'cluster_n_cells')))

# Group together close together clusters ---------------------------------------

## Centroid
points_sp <- coordinates(clumps_sp) %>%
  as.data.frame() %>%
  dplyr::rename(lon = V1,
                lat = V2) %>%
  bind_cols(clumps_sp@data)

## Spatially Define and project
coordinates(points_sp) <- ~lon+lat
crs(points_sp) <- CRS("+init=epsg:4326")
points_sp <- spTransform(points_sp, CRS(UTM_ETH))

saveRDS(points_sp, file.path(panel_rsdp_imp_data_file_path, "clusters_of_ntl", "individual_datasets", "polygons_ALL_NO_SUBSET.Rds"))

## Back to dataframe
points <- as.data.frame(points_sp)

## Clusters
points_dist <- points[,c("lat", "lon")] %>% dist()
clumps_sp$wardheirch_clust_id <- hclust(points_dist, method = "ward.D2") %>%
  cutree(h = 10000)

clumps_sp <- raster::aggregate(clumps_sp, by = "wardheirch_clust_id", sums=list(list(sum, 'cluster_n_cells')))

clumps_sp@data <- clumps_sp@data %>%
  dplyr::select(-c(wardheirch_clust_id)) %>% 
  dplyr::mutate(cell_id = 1:n()) # prevous cell_id summed version; fresh, aggregated version

# Restrict to cells with more than 3 years persistent lights -------------------
# data <- lapply(1992:2013, function(year){
#   print(year)
#   dmspols <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", "RawData", "Individual Files", paste0("eth_dmspols_",year,".tif")))
#   dmspols_vx <- velox(dmspols)
#   clumps_sp$dmspols_sum0greater <- dmspols_vx$extract(sp=clumps_sp, fun=function(x){sum(x > 0, na.rm=T)}) %>% as.numeric
#   clumps_sp$year <- year
#   return(clumps_sp@data)
# }) %>%
#   bind_rows()
# 
# data <- data %>%
#   arrange(year) %>%
#   group_by(cell_id) %>%
#   
#   # Make binary value: if number of positive
#   mutate(dmspols_sum0greater_bin = dmspols_sum0greater > 0,
#          dmspols_sum0greater_bin = dmspols_sum0greater_bin %>% replace_na(0)) %>%
#   
#   # Sum binary value from last 3 time period
#   mutate(N_pos = runSum(dmspols_sum0greater_bin, n = 2)) %>%
#   
#   # For each cluster maximum "summed" value
#   mutate(N_pos_max = max(N_pos, na.rm=T)) %>%
#   ungroup() %>%
#   
#   filter(N_pos_max %in% 2)

# Export -----------------------------------------------------------------------
# We save "polygon" and "points" file, where "points" is actually just the polygon.
# We do this to make compatible with some scripts that also process grid data

# Make cut/no cut the same -- just to replicating naming convention of woreda

## Dataframe with number of cells 
saveRDS(clumps_sp, file.path(panel_rsdp_imp_data_file_path, "clusters_of_ntl", "individual_datasets", "cluster_n_cells.Rds"))
clumps_sp$cluster_n_cells <- NULL

## Main Files - 1km road cut out
saveRDS(clumps_sp, file.path(panel_rsdp_imp_data_file_path, "clusters_of_ntl", "individual_datasets", "polygons.Rds"))
saveRDS(clumps_sp, file.path(panel_rsdp_imp_data_file_path, "clusters_of_ntl", "individual_datasets", "points.Rds"))

## Full Data Files - 1km road not cut of
saveRDS(clumps_sp, file.path(panel_rsdp_imp_data_file_path, "clusters_of_ntl", "individual_datasets", "polygons_no_road_cut.Rds"))
saveRDS(clumps_sp, file.path(panel_rsdp_imp_data_file_path, "clusters_of_ntl", "individual_datasets", "points_no_road_cut.Rds"))





