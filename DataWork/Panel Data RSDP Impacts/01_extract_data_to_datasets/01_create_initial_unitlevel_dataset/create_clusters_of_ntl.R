# Identify Clusters of Lights

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
dmspols_2013_binary[] <- as.numeric(dmspols_1996[] >= 1 |
                                      dmspols_1997[] >= 1 |
                                      dmspols_1998[] >= 1 |
                                      dmspols_1999[] >= 1 |
                                      dmspols_2000[] >= 1 |
                                      dmspols_2001[] >= 1 |
                                      dmspols_2002[] >= 1 |
                                      dmspols_2003[] >= 1 |
                                      dmspols_2004[] >= 1 |
                                      dmspols_2005[] >= 1 |
                                      dmspols_2006[] >= 1 |
                                      dmspols_2007[] >= 1 |
                                      dmspols_2008[] >= 1 |
                                      dmspols_2009[] >= 1 |
                                      dmspols_2010[] >= 1 |
                                      dmspols_2011[] >= 1 |
                                      dmspols_2012[] >= 1 |
                                      dmspols_2013[] >= 1)
dmspols_2013_clumps <- clump(dmspols_2013_binary, directions=8)

clumps_unique_values <- unique(dmspols_2013_clumps[])[!is.na(unique(dmspols_2013_clumps[]))]

# Polygonize clusters ----------------------------------------------------------
clumps_sp <- lapply(clumps_unique_values, function(clump_i){
  print(clump_i)
  clump_i_sp <- rasterToPolygons(dmspols_2013_clumps, 
                                 fun=function(x){x==clump_i}, 
                                 n=4, na.rm=TRUE, 
                                 digits=12, 
                                 dissolve=F)
  clump_i_sp$cell_id <- clump_i
  clump_i_sp$cluster_n_cells <- nrow(clump_i_sp)
  return(clump_i_sp)
}) %>% do.call(what="rbind")

clumps_sp <- raster::aggregate(clumps_sp, 
                               by="cell_id",
                               list(list(mean, 'cluster_n_cells')))

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





