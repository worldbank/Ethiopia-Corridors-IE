# Extract DMSPOLS-Intercalibrated to Points

# Load Data --------------------------------------------------------------------
#### Urban Clusters
clumps_sf <- read_sf(file.path(finaldata_file_path, "urban_cluster_dataset", "individual_files", "urban_cluster.geojson")) %>% as("Spatial")

read_ntl <- function(year){
  # Load NTL and crop to points extent. When 2 nigthttime light datasets for a year,
  # average together
  
  print(year)
  
  raster_filepaths <- list.files(file.path(rawdata_file_path, "Nighttime Lights", "DMSP_OLS_INTERCALIBRATED_ZHANG2016"),
             pattern=as.character(year),
             full.names = T)
  
  if(length(raster_filepaths) == 1){
    ntl <- raster(raster_filepaths[1]) %>% crop(extent(clumps_sf))
  } else if(length(raster_filepaths) == 2){
    ntl_1 <- raster(raster_filepaths[1]) %>% crop(extent(clumps_sf))
    ntl_2 <- raster(raster_filepaths[2]) %>% crop(extent(clumps_sf))

    ntl <- ntl_1
    ntl[] <- (ntl_1[] + ntl_2[])/2
  }
  
  ntl[] <- ntl[]*0.01
  
  return(ntl)
}

# Extract Data -----------------------------------------------------------------
#### DMSPOLS
cluster_ntl <- lapply(1992:2012, function(year){
  print(year)
  dmspols <- read_ntl(year)
  
  dmspols[][dmspols[] %in% 0] <- NA
  dmspols[dmspols == 0] <- NA
  
  # Average NTL
  clumps_sf$dmspols_zhang <- velox(dmspols)$extract(sp=clumps_sf, fun=function(x) mean(x,na.rm=T))
  clumps_sf$dmspols_zhang_median <- velox(dmspols)$extract(sp=clumps_sf, fun=function(x) median(x,na.rm=T))
  clumps_sf$dmspols_zhang_max <- velox(dmspols)$extract(sp=clumps_sf, fun=function(x) max(x,na.rm=T))
  clumps_sf$dmspols_zhang_sd <- velox(dmspols)$extract(sp=clumps_sf, fun=function(x) sd(x,na.rm=T))
  
  clumps_sf$dmspols_zhang[clumps_sf$dmspols_zhang %in% c(Inf,-Inf)] <- 0
  clumps_sf$dmspols_zhang_median[clumps_sf$dmspols_zhang_median %in% c(Inf,-Inf)] <- 0
  clumps_sf$dmspols_zhang_max[clumps_sf$dmspols_zhang_max %in% c(Inf,-Inf)] <- 0
  clumps_sf$dmspols_zhang_sd[clumps_sf$dmspols_zhang_sd %in% c(Inf,-Inf)] <- 0
  
  # Above threshold NTL
  for(threshold in c(1,2,5,10,20,30,40)){
    dmspols_thresh <- dmspols
    dmspols_thresh[] <- as.numeric(dmspols_thresh[] >= threshold)
    clumps_sf[[paste0("dmspols_zhang_",threshold,"_above")]] <- velox(dmspols_thresh)$extract(sp=clumps_sf, fun=sum)
    clumps_sf[[paste0("dmspols_zhang_",threshold,"_above")]][is.na(clumps_sf[[paste0("dmspols_zhang_",threshold,"_above")]])] <- 0
  }
  
  clumps_sf$year <- year
  
  return(clumps_sf@data)
  
}) %>% bind_rows

# Export -----------------------------------------------------------------------
saveRDS(cluster_ntl, file.path(finaldata_file_path, "urban_cluster_dataset", "individual_files", "urban_cluster_dmspols_zhang2016.Rds"))

