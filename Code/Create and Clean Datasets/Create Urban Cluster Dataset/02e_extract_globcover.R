# Extract DMSPOLS-Intercalibrated to Points

# Load Data --------------------------------------------------------------------
#### Urban Clusters
clumps_sf <- read_sf(file.path(finaldata_file_path, "urban_cluster_dataset", "individual_files", "urban_cluster.geojson")) %>% as("Spatial")

extract_globcover <- function(year){
  print(year)
  
  globcover <- raster(file.path(rawdata_file_path, "esa_globcover", "scratch","ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif"),(year - 1991)) %>% crop(extent(clumps_sf), snap="out")
  
  globcover_urban <- globcover
  globcover_urban[] <- as.numeric(globcover_urban[] %in% c(190))
  
  clumps_sf$gc_urban_mean <- velox(globcover_urban)$extract(sp=clumps_sf, fun=mean)
  clumps_sf$gc_urban_sum <- velox(globcover_urban)$extract(sp=clumps_sf, fun=sum)
  
  clumps_sf$year <- year
  return(clumps_sf@data)
}

clumps_sf_gc <- lapply(1992:2015, extract_globcover) %>% bind_rows

# Export -----------------------------------------------------------------------
saveRDS(clumps_sf_gc, file.path(finaldata_file_path, "urban_cluster_dataset", "individual_files", "urban_cluster_globcover.Rds"))


