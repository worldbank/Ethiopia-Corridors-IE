# Extract Globcover to Points

# Load Data --------------------------------------------------------------------
polygons <- readRDS(file.path(panel_rsdp_imp_data_file_path, DATASET_TYPE, "individual_datasets", "polygons.Rds"))

# Add Data ---------------------------------------------------------------------
extract_globcover <- function(year){
  print(year)
  
  if(year %in% 1992:2015) globcover <- raster(file.path(data_file_path, "Globcover", "RawData", "1992_2015_data", "ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif"), (year-1991)) %>% crop(extent(polygons))
  if(year %in% 2016:2018) globcover <- raster(file.path(data_file_path, "Globcover", "RawData", "2016_2018_data", paste0("C3S-LC-L4-LCCS-Map-300m-P1Y-",year,"-v2.1.1.tif"))) %>% crop(extent(polygons))

  globcover_urban_fun <- function(r){
    r[] <- as.numeric(r[] %in% c(190))
    return(r)
  }
  
  globcover_cropland_fun <- function(r){
    r[] <- as.numeric(r[] %in% c(10,11,12,20,30))
    return(r)
  }
  
  globcover_cropland_rainfed_fun <- function(r){
    r[] <- as.numeric(r[] %in% c(10))
    return(r)
  }
  
  globcover_cropland_irrigated_fun <- function(r){
    r[] <- as.numeric(r[] %in% c(20))
    return(r)
  }
  
  globcover_cropland_mosaic_fun <- function(r){
    r[] <- as.numeric(r[] %in% c(30))
    return(r)
  }
  
  globcover_urban <- calc(globcover, fun=globcover_urban_fun)
  globcover_cropland <- calc(globcover, fun=globcover_cropland_fun)
  #globcover_cropland_rainfed <- calc(globcover, fun=globcover_cropland_rainfed_fun)
  #globcover_cropland_irrigated <- calc(globcover, fun=globcover_cropland_irrigated_fun)
  #globcover_cropland_mosaic <- calc(globcover, fun=globcover_cropland_mosaic_fun)
  
  polygons$globcover_urban <- velox(globcover_urban)$extract(polygons, fun=mean, small = T)
  polygons$globcover_cropland <- velox(globcover_cropland)$extract(polygons, fun=mean, small = T)
  #polygons$globcover_cropland_rainfed <- velox(globcover_cropland_rainfed)$extract(polygons, fun=mean)
  #polygons$globcover_cropland_irrigated <- velox(globcover_cropland_irrigated)$extract(polygons, fun=mean)
  #polygons$globcover_cropland_mosaic <- velox(globcover_cropland_mosaic)$extract(polygons, fun=mean)
  
  # If not a grid dataset, also take the sum (for total cells classifed as urban, for example)
  if(!grepl("grid", DATASET_TYPE)){
    polygons$globcover_urban_sum <- velox(globcover_urban)$extract(polygons, fun=sum, small = T)
    polygons$globcover_cropland_sum <- velox(globcover_cropland)$extract(polygons, fun=sum, small = T)
    #polygons$globcover_cropland_rainfed_sum <- velox(globcover_cropland_rainfed)$extract(polygons, fun=sum)
    #polygons$globcover_cropland_irrigated_sum <- velox(globcover_cropland_irrigated)$extract(polygons, fun=sum)
    #polygons$globcover_cropland_mosaic_sum <- velox(globcover_cropland_mosaic)$extract(polygons, fun=sum)
  }
  
  polygons$year <- year
  
  return(polygons@data)
}

polygons_globcover <- lapply(1992:2018, extract_globcover) %>% bind_rows

# Export -----------------------------------------------------------------------
saveRDS(polygons_globcover, file.path(panel_rsdp_imp_data_file_path, DATASET_TYPE, "individual_datasets", "globcover.Rds"))

