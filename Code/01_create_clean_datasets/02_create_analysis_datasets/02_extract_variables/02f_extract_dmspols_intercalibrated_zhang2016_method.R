# Extract DMSPOLS-Intercalibrated to Points

# Load Data --------------------------------------------------------------------
points <- readRDS(file.path(finaldata_file_path, DATASET_TYPE,"individual_datasets", "points.Rds"))
if(grepl("grid", DATASET_TYPE)){
  coordinates(points) <- ~long+lat
  crs(points) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
}

read_ntl <- function(year){
  # Load NTL and crop to points extent. When 2 nigthttime light datasets for a year,
  # average together
  
  print(year)
  
  raster_filepaths <- list.files(file.path(rawdata_file_path, "Nighttime Lights", "DMSP_OLS_INTERCALIBRATED_ZHANG2016"),
             pattern=as.character(year),
             full.names = T)
  
  if(length(raster_filepaths) == 1){
    ntl <- raster(raster_filepaths[1]) %>% crop(extent(points))
  } else if(length(raster_filepaths) == 2){
    ntl_1 <- raster(raster_filepaths[1]) %>% crop(extent(points))
    ntl_2 <- raster(raster_filepaths[2]) %>% crop(extent(points))

    ntl <- ntl_1
    ntl[] <- (ntl_1[] + ntl_2[])/2
  }
  
  ntl[] <- ntl[]*0.01
  
  return(ntl)
}

# Add Data ---------------------------------------------------------------------
extract_raster_to_points <- function(year, points){
  print(year)
  dmspols <- read_ntl(year)

  if(grepl("grid", DATASET_TYPE)){
    points$dmspols_zhang <- raster::extract(dmspols, points)
  } else{
    points$dmspols_zhang <- velox(dmspols)$extract(sp=points, fun=function(x){mean(x, na.rm=T)}) %>% as.numeric
  }
  
  points$year <- year
  return(points@data)
}

points_all <- lapply(1992:2012, extract_raster_to_points, points) %>% bind_rows

# Export -----------------------------------------------------------------------
saveRDS(points_all, file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", "points_dmspols_zhang2016.Rds"))

