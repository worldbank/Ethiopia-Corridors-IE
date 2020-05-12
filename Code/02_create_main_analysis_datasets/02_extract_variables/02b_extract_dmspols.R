# Extract DMSP-OLS to Points

# Load Data --------------------------------------------------------------------
points <- readRDS(file.path(finaldata_file_path, DATASET_TYPE,"individual_datasets", "points.Rds"))

if(grepl("grid", DATASET_TYPE)){
  coordinates(points) <- ~long+lat
  crs(points) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
}

# Add Data ---------------------------------------------------------------------
extract_dmspols_to_points <- function(year, points){
  print(year)
  dmspols <- raster(file.path(rawdata_file_path, "Nighttime Lights", "DMSP_OLS","Stacked", "eth_dmspols_allyears.tif"),(year-1991))
  
  if(grepl("grid", DATASET_TYPE)){
    points$dmspols <- extract(dmspols, points)
  } else{
    points$dmspols <- velox(dmspols)$extract(sp=points, fun=function(x){mean(x, na.rm=T)}) %>% as.numeric
    points$dmspols_2 <- velox(dmspols)$extract(sp=points, fun=function(x){mean(x >= 2, na.rm=T)}) %>% as.numeric
    points$dmspols_6 <- velox(dmspols)$extract(sp=points, fun=function(x){mean(x >= 6, na.rm=T)}) %>% as.numeric
  }
  
  points$year <- year
  return(points@data)
}

points_all <- lapply(1992:2013, extract_dmspols_to_points, points) %>% bind_rows

# Export -----------------------------------------------------------------------
saveRDS(points_all, file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", "points_dmspols.Rds"))

