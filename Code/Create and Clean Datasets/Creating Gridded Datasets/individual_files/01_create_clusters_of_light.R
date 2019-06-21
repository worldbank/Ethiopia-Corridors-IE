# Identify Clusters of Lights

# Load Data --------------------------------------------------------------------
dmspols_2013 <- raster(file.path(rawdata_file_path, "Nighttime Lights", "DMSP_OLS", "Individual Files", "eth_dmspols_2013.tif"))

dmspols_2013_binary <- dmspols_2013
dmspols_2013_binary[] <- as.numeric(dmspols_2013_binary[] >= 1)
dmspols_2013_clumps <- clump(dmspols_2013_binary, directions=4)

clumps_unique_values <- unique(dmspols_2013_clumps[])[!is.na(unique(dmspols_2013_clumps[]))]

clumps_sp <- lapply(clumps_unique_values, function(clump_i){
  print(clump_i)
  clump_i_sp <- rasterToPolygons(dmspols_2013_clumps, 
                                          fun=function(x){x==clump_i}, 
                                          n=4, na.rm=TRUE, 
                                          digits=12, 
                                          dissolve=F)
  clump_i_sp$cluster_id <- clump_i
  clump_i_sp$cluster_n_cells <- nrow(clump_i_sp)
  return(clump_i_sp)
}) %>% do.call(what="rbind")

clumps_sp <- raster::aggregate(clumps_sp, 
                                 by="cluster_id",
                                 list(list(mean, 'cluster_n_cells')))

clumps_sf <- st_as_sf(clumps_sp)
st_write(clumps_sf, file.path(finaldata_file_path, "urban_cluster_dataset", "individual_files", "urban_cluster.geojson"), delete_dsn=T)

