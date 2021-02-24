# Identify Clusters of Lights

set.seed(42)
# Load Data --------------------------------------------------------------------
eth_adm <- readRDS(file.path(data_file_path, "GADM", "RawData", "gadm36_ETH_0_sp.rds"))

dmspols_extent <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS_DMSPOLS_Intercalibrated", "Harmonized_DN_NTL_1996_calDMSP.tif")) %>% crop(eth_adm) %>% extent()

dmspols_1996 <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS_DMSPOLS_Intercalibrated", "Harmonized_DN_NTL_1996_calDMSP.tif")) %>% crop(dmspols_extent) 
dmspols_1997 <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS_DMSPOLS_Intercalibrated", "Harmonized_DN_NTL_1997_calDMSP.tif")) %>% crop(dmspols_extent) 
dmspols_1998 <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS_DMSPOLS_Intercalibrated", "Harmonized_DN_NTL_1998_calDMSP.tif")) %>% crop(dmspols_extent) 
dmspols_1999 <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS_DMSPOLS_Intercalibrated", "Harmonized_DN_NTL_1999_calDMSP.tif")) %>% crop(dmspols_extent) 
dmspols_2000 <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS_DMSPOLS_Intercalibrated", "Harmonized_DN_NTL_2000_calDMSP.tif")) %>% crop(dmspols_extent) 
dmspols_2001 <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS_DMSPOLS_Intercalibrated", "Harmonized_DN_NTL_2001_calDMSP.tif")) %>% crop(dmspols_extent) 
dmspols_2002 <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS_DMSPOLS_Intercalibrated", "Harmonized_DN_NTL_2002_calDMSP.tif")) %>% crop(dmspols_extent)  
dmspols_2003 <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS_DMSPOLS_Intercalibrated", "Harmonized_DN_NTL_2003_calDMSP.tif")) %>% crop(dmspols_extent) 
dmspols_2004 <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS_DMSPOLS_Intercalibrated", "Harmonized_DN_NTL_2004_calDMSP.tif")) %>% crop(dmspols_extent) 
dmspols_2005 <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS_DMSPOLS_Intercalibrated", "Harmonized_DN_NTL_2005_calDMSP.tif")) %>% crop(dmspols_extent) 
dmspols_2006 <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS_DMSPOLS_Intercalibrated", "Harmonized_DN_NTL_2006_calDMSP.tif")) %>% crop(dmspols_extent) 
dmspols_2007 <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS_DMSPOLS_Intercalibrated", "Harmonized_DN_NTL_2007_calDMSP.tif")) %>% crop(dmspols_extent) 
dmspols_2008 <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS_DMSPOLS_Intercalibrated", "Harmonized_DN_NTL_2008_calDMSP.tif")) %>% crop(dmspols_extent) 
dmspols_2009 <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS_DMSPOLS_Intercalibrated", "Harmonized_DN_NTL_2009_calDMSP.tif")) %>% crop(dmspols_extent) 
dmspols_2010 <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS_DMSPOLS_Intercalibrated", "Harmonized_DN_NTL_2010_calDMSP.tif")) %>% crop(dmspols_extent) 
dmspols_2011 <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS_DMSPOLS_Intercalibrated", "Harmonized_DN_NTL_2011_calDMSP.tif")) %>% crop(dmspols_extent) 
dmspols_2012 <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS_DMSPOLS_Intercalibrated", "Harmonized_DN_NTL_2012_calDMSP.tif")) %>% crop(dmspols_extent) 
dmspols_2013 <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS_DMSPOLS_Intercalibrated", "Harmonized_DN_NTL_2013_calDMSP.tif")) %>% crop(dmspols_extent) 
dmspols_2014 <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS_DMSPOLS_Intercalibrated", "Harmonized_DN_NTL_2014_simVIIRS.tif")) %>% crop(dmspols_extent) 
dmspols_2015 <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS_DMSPOLS_Intercalibrated", "Harmonized_DN_NTL_2015_simVIIRS.tif")) %>% crop(dmspols_extent) 
dmspols_2016 <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS_DMSPOLS_Intercalibrated", "Harmonized_DN_NTL_2016_simVIIRS.tif")) %>% crop(dmspols_extent) 
dmspols_2017 <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS_DMSPOLS_Intercalibrated", "Harmonized_DN_NTL_2017_simVIIRS.tif")) %>% crop(dmspols_extent) 
dmspols_2018 <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS_DMSPOLS_Intercalibrated", "Harmonized_DN_NTL_2018_simVIIRS.tif")) %>% crop(dmspols_extent) 

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
dmspols_2013_binary <- dmspols_2013_binary %>% mask(eth_adm)
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

saveRDS(clumps_sp, file.path(panel_rsdp_imp_data_file_path, "clusters_of_ntlall", "individual_datasets", "polygons_ALL_NO_SUBSET.Rds"))

# Restrict to cells with more than 3 years persistent lights -------------------
data <- lapply(1992:2018, function(year){
  print(year)
  
  if(year %in% 1992:2018){
    dmspols <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS_DMSPOLS_Intercalibrated", paste0("Harmonized_DN_NTL_",year,"_calDMSP.tif")))
  } else{
    dmspols <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS_DMSPOLS_Intercalibrated", paste0("Harmonized_DN_NTL_",year,"_simVIIRS.tif")))
  }
  
  dmspols_vx <- velox(dmspols)
  clumps_sp$dmspols_sum0greater <- dmspols_vx$extract(sp=clumps_sp, fun=function(x){sum(x > 0, na.rm=T)}) %>% as.numeric
  clumps_sp$year <- year
  return(clumps_sp@data)
}) %>%
  bind_rows()

dataa <- data %>%
  arrange(year) %>%
  group_by(cell_id) %>%
  
  # Make binary value: if number of positive
  mutate(dmspols_sum0greater_bin = dmspols_sum0greater > 0,
         dmspols_sum0greater_bin = dmspols_sum0greater_bin %>% replace_na(0)) %>%
  
  # Sum binary value from last 3 time period
  mutate(N_pos = runSum(dmspols_sum0greater_bin, n = 3)) %>%
  
  # For each cluster maximum "summed" value
  mutate(N_pos_max = max(N_pos, na.rm=T)) %>%
  ungroup() %>%
  
  filter(N_pos_max %in% 3)

# Export -----------------------------------------------------------------------
# We save "polygon" and "points" file, where "points" is actually just the polygon.
# We do this to make compatible with some scripts that also process grid data

# Make cut/no cut the same -- just to replicating naming convention of woreda

## Dataframe with number of cells 
saveRDS(clumps_sp, file.path(panel_rsdp_imp_data_file_path, "clusters_of_ntlall", "individual_datasets", "cluster_n_cells.Rds"))
clumps_sp$cluster_n_cells <- NULL

## Main Files - 1km road cut out
saveRDS(clumps_sp, file.path(panel_rsdp_imp_data_file_path, "clusters_of_ntlall", "individual_datasets", "polygons.Rds"))
saveRDS(clumps_sp, file.path(panel_rsdp_imp_data_file_path, "clusters_of_ntlall", "individual_datasets", "points.Rds"))

## Full Data Files - 1km road not cut of
saveRDS(clumps_sp, file.path(panel_rsdp_imp_data_file_path, "clusters_of_ntlall", "individual_datasets", "polygons_no_road_cut.Rds"))
saveRDS(clumps_sp, file.path(panel_rsdp_imp_data_file_path, "clusters_of_ntlall", "individual_datasets", "points_no_road_cut.Rds"))





