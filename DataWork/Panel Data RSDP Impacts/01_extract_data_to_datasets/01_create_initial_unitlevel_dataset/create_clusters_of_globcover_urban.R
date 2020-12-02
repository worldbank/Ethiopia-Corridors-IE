# Identify Clusters of Lights

# Load Data --------------------------------------------------------------------
gc <- raster(file.path(data_file_path, "Globcover", "RawData", "1992_2015_data", "ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif"), 24)
woreda <- readRDS(file.path(project_file_path, "Data", "Woreda Population", "FinalData", "woreda.Rds"))

## Crop/Mask to Ethiopia
woreda <- spTransform(woreda, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
gc <- gc %>% crop(woreda) %>% mask(woreda)

# Define raster layer of clusters ----------------------------------------------
gc_binary <- gc
gc_binary[] <- as.numeric(gc[] %in% 190)
gc_clumps <- clump(gc_binary, directions=8)

clumps_unique_values <- unique(gc_clumps[])[!is.na(unique(gc_clumps[]))]

# Polygonize clusters ----------------------------------------------------------
clumps_sp <- lapply(clumps_unique_values, function(clump_i){
  print(clump_i)
  clump_i_sp <- rasterToPolygons(gc_clumps, 
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
saveRDS(clumps_sp, file.path(panel_rsdp_imp_data_file_path, "clusters_of_globcover_urban", "individual_datasets", "polygons.Rds"))
saveRDS(clumps_sp, file.path(panel_rsdp_imp_data_file_path, "clusters_of_globcover_urban", "individual_datasets", "points.Rds"))

## Full Data Files - 1km road not cut of
saveRDS(clumps_sp, file.path(panel_rsdp_imp_data_file_path, "clusters_of_globcover_urban", "individual_datasets", "polygons_no_road_cut.Rds"))
saveRDS(clumps_sp, file.path(panel_rsdp_imp_data_file_path, "clusters_of_globcover_urban", "individual_datasets", "points_no_road_cut.Rds"))

