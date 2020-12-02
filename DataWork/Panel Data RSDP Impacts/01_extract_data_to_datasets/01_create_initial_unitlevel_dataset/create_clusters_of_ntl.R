# Identify Clusters of Lights

# Load Data --------------------------------------------------------------------
dmspols_2013 <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", "RawData", "Individual Files", "eth_dmspols_2013.tif"))

# Define raster layer of clusters ----------------------------------------------
dmspols_2013_binary <- dmspols_2013
dmspols_2013_binary[] <- as.numeric(dmspols_2013_binary[] >= 1)
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

# Export -----------------------------------------------------------------------
# We save "polygon" and "points" file, where "points" is actually just the polygon.
# We do this to make compatible with some scripts that also process grid data

# Make cut/no cut the same -- just to replicating naming convention of woreda

## Main Files - 1km road cut out
saveRDS(clumps_sp, file.path(panel_rsdp_imp_data_file_path, "clusters_of_ntl", "individual_datasets", "polygons.Rds"))
saveRDS(clumps_sp, file.path(panel_rsdp_imp_data_file_path, "clusters_of_ntl", "individual_datasets", "points.Rds"))

## Full Data Files - 1km road not cut of
saveRDS(clumps_sp, file.path(panel_rsdp_imp_data_file_path, "clusters_of_ntl", "individual_datasets", "polygons_no_road_cut.Rds"))
saveRDS(clumps_sp, file.path(panel_rsdp_imp_data_file_path, "clusters_of_ntl", "individual_datasets", "points_no_road_cut.Rds"))

