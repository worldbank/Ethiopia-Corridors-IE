# Extract DMSP-OLS to Points

# Load Data --------------------------------------------------------------------
sdf <- readRDS(file.path(panel_rsdp_imp_data_file_path, DATASET_TYPE, "individual_datasets", "points.Rds"))

# Add Data ---------------------------------------------------------------------
extract_dmspols_to_points <- function(year, sdf){
  print(year)
  dmspols <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", "RawData", "Stacked", "eth_dmspols_allyears.tif"),(year-1991))
  
  dmspols_vx <- velox(dmspols)
  
  if(grepl("grid", DATASET_TYPE)){
    sdf$dmspols <- dmspols_vx$extract_points(sp=sdf) %>% as.numeric
  } else{
    sdf$dmspols <- dmspols_vx$extract(sp=sdf, fun=function(x){mean(x, na.rm=T)}) %>% as.numeric
    sdf$dmspols_2 <- dmspols_vx$extract(sp=sdf, fun=function(x){mean(x >= 2, na.rm=T)}) %>% as.numeric
    sdf$dmspols_6 <- dmspols_vx$extract(sp=sdf, fun=function(x){mean(x >= 6, na.rm=T)}) %>% as.numeric
  }
  
  sdf$year <- year
  return(sdf@data)
}

sdf_all <- lapply(1992:2013, extract_dmspols_to_points, sdf) %>% bind_rows

# Export -----------------------------------------------------------------------
saveRDS(sdf_all, file.path(panel_rsdp_imp_data_file_path, DATASET_TYPE, "individual_datasets", "dmspols.Rds"))

