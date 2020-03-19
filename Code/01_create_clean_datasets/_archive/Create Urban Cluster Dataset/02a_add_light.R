# Add Nighttime Lights to Cluster 

library(velox)
library(ggplot2)
library(sf)

# Load Data --------------------------------------------------------------------
#### Urban Clusters
clumps_sf <- read_sf(file.path(finaldata_file_path, "urban_cluster_dataset", "individual_files", "urban_cluster.geojson")) %>% as("Spatial")

# Extract Data -----------------------------------------------------------------
#### DMSPOLS
cluster_ntl <- lapply(1992:2013, function(year){
  print(year)
  dmspols <- raster(file.path(rawdata_file_path, "Nighttime Lights", "DMSP_OLS", "Individual Files", paste0("eth_dmspols_",year,".tif")))
  
  # Average NTL
  clumps_sf$dmspols <- velox(dmspols)$extract(sp=clumps_sf, fun=mean)
  
  # Above threshold NTL
  for(threshold in c(1,2,5,10,20,30,40)){
    dmspols_thresh <- dmspols
    dmspols_thresh[] <- as.numeric(dmspols_thresh[] >= threshold)
    clumps_sf[[paste0("dmspols_",threshold,"_above")]] <- velox(dmspols_thresh)$extract(sp=clumps_sf, fun=sum)
  }
  
  clumps_sf$year <- year
  
  return(clumps_sf@data)
  
}) %>% bind_rows

# Export -----------------------------------------------------------------------
saveRDS(cluster_ntl, file.path(finaldata_file_path, "urban_cluster_dataset", "individual_files", "urban_cluster_dmspols.Rds"))

#ggplot() + 
#  geom_line(data=cluster_ntl[cluster_ntl$cluster_id %in% 45:55,], aes(x=year, y=dmspols_2_above, group=cluster_id, color=factor(cluster_id)))


