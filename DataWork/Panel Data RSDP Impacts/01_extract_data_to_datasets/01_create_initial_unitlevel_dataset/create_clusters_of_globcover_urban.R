# Identify Clusters of Lights

set.seed(42)
# Load Data --------------------------------------------------------------------
## Woredas
woreda <- readRDS(file.path(project_file_path, "Data", "Woreda Population", "FinalData", "woreda.Rds"))
woreda <- spTransform(woreda, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))

## Globcover
# Create raster that is 1 if urban in any time period 
urban_constant <- raster(file.path(data_file_path, "Globcover", "RawData", "1992_2015_data", "ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif"), band=(1992-1991)) %>% crop(extent(woreda))
urban_constant[] <- as.numeric(urban_constant[] %in% c(190))

for(year in 1993:2015){
  print(year)
  urban_yyyy <- raster(file.path(data_file_path, "Globcover", "RawData", "1992_2015_data", "ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif"), band=(year-1991)) %>% crop(extent(woreda))
  urban_constant[] <- as.numeric((urban_constant[] %in% 1) | (urban_yyyy[] %in% c(190))) 
}

for(year in 2016:2018){
  print(year)
  urban_yyyy <- raster(file.path(data_file_path, "Globcover", "RawData", "2016_2018_data", paste0("C3S-LC-L4-LCCS-Map-300m-P1Y-",year,"-v2.1.1.tif"))) %>% crop(extent(woreda))
  urban_constant[] <- as.numeric((urban_constant[] %in% 1) | (urban_yyyy[] %in% c(190))) 
}

# Load Data --------------------------------------------------------------------
## Crop/Mask to Ethiopia
gc_binary <- urban_constant %>% crop(woreda) %>% mask(woreda)

# Define raster layer of clusters ----------------------------------------------
gc_clumps <- clump(gc_binary, directions=8)

clumps_unique_values <- unique(gc_clumps[])[!is.na(unique(gc_clumps[]))]

# Polygonize clusters ----------------------------------------------------------
clumps_sp <- lapply(clumps_unique_values, function(clump_i){
  print(paste(clump_i, "/", length(clumps_unique_values)))
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

saveRDS(points_sp, file.path(panel_rsdp_imp_data_file_path, "clusters_of_globcover_urban", "individual_datasets", "polygons_ALL_NO_SUBSET.Rds"))

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

# Restrict to cells with more than 3 years persistent urban --------------------
# data <- lapply(1992:2018, function(year){
#   print(year)
#   
#   if(year %in% 1992:2015){
#     urbangc <- raster(file.path(data_file_path, "Globcover", "RawData", "1992_2015_data", "ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif"), band=(year-1991)) %>% crop(extent(woreda))
#   } else{
#     urbangc <- raster(file.path(data_file_path, "Globcover", "RawData", "2016_2018_data", paste0("C3S-LC-L4-LCCS-Map-300m-P1Y-",year,"-v2.1.1.tif"))) %>% crop(extent(woreda))
#   }
#   urbangc[] <- as.numeric(urbangc[] %in% c(190))
#   
#   urbangc_vx <- velox(urbangc)
#   clumps_sp$globcover_urban_sum <- urbangc_vx$extract(sp=clumps_sp, fun=function(x){sum(x > 0, na.rm=T)}) %>% as.numeric
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
#   mutate(globcover_urban_sum_bin = globcover_urban_sum > 0,
#          globcover_urban_sum_bin = globcover_urban_sum_bin %>% replace_na(0)) %>%
#   
#   # Sum binary value from last 3 time period
#   mutate(N_pos = runSum(globcover_urban_sum_bin, n = 4)) %>%
#   
#   # For each cluster maximum "summed" value
#   mutate(N_pos_max = max(N_pos, na.rm=T)) %>%
#   ungroup() %>%
#   
#   filter(N_pos_max %in% 4)
# 
# clumps_sp <- clumps_sp[clumps_sp$cell_id %in% data$cell_id,]

# Export -----------------------------------------------------------------------
# We save "polygon" and "points" file, where "points" is actually just the polygon.
# We do this to make compatible with some scripts that also process grid data

# Make cut/no cut the same -- just to replicating naming convention of woreda

## Dataframe with number of cells 
saveRDS(clumps_sp, file.path(panel_rsdp_imp_data_file_path, "clusters_of_globcover_urban", "individual_datasets", "cluster_n_cells.Rds"))
clumps_sp$cluster_n_cells <- NULL

## Main Files - 1km road cut out
saveRDS(clumps_sp, file.path(panel_rsdp_imp_data_file_path, "clusters_of_globcover_urban", "individual_datasets", "polygons.Rds"))
saveRDS(clumps_sp, file.path(panel_rsdp_imp_data_file_path, "clusters_of_globcover_urban", "individual_datasets", "points.Rds"))

## Full Data Files - 1km road not cut of
saveRDS(clumps_sp, file.path(panel_rsdp_imp_data_file_path, "clusters_of_globcover_urban", "individual_datasets", "polygons_no_road_cut.Rds"))
saveRDS(clumps_sp, file.path(panel_rsdp_imp_data_file_path, "clusters_of_globcover_urban", "individual_datasets", "points_no_road_cut.Rds"))



