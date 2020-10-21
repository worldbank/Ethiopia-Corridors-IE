# Merge Datasets Together

# Load Data / Create Dataset Lists -----------------------------------------------
# Load data that we'll merge datasets into and create lists of datasets to merge
# into this dataset.

#### Load dataset to merge into
points_all <- readRDS(file.path(finaldata_file_path, DATASET_TYPE,"individual_datasets", "points_dmspols.Rds")) 

#### Names of datasets to merge in
# Separate by time invarient (merge by cell_id) and time varying (merge by cell_id 
# and year)
DATASETS_TIME_INVARIANT <- c("woreda_details.Rds", "points_distance_cities.Rds", "points_distance_rsdp_phases.Rds",
                             "points_area.Rds")
DATASETS_TIME_VARYING <- c("points_viirs.Rds",
                           "points_ndvi.Rds",
                           "points_globcover.Rds",
                           "points_dmspols_zhang2016.Rds",
                           "woreda_market_access.Rds",
                           "points_precipitation.Rds",
                           "points_temperature.Rds",
                           "points_roadlength_km.Rds",
                           "points_distance_rdsep_speed_surface.Rds", # road sep, but var names don't need to be changed, so include here
                           "road_accessibility_scaleAreaTRUE.Rds",
                           "road_accessibility_scaleAreaFALSE.Rds")
DATASETS_TIME_VARYING_ALWAYS_FULL_WOREDA <- c("points_distance_roads_byspeed.Rds",
                                              "points_distance_improved_roads_byspeed_after.Rds")
DATASETS_ROAD_SEP_CHNG_NAMES <- c("road_accessibility_scaleAreaTRUE_rdsep.Rds",
                       "road_accessibility_scaleAreaFALSE_rdsep.Rds",
                       "woreda_market_access_rdsep.Rds")

# Merge ------------------------------------------------------------------------
for(dataset in DATASETS_TIME_VARYING){
  print(dataset)
  dataset_temp <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", dataset)) %>% data.table 
  points_all <- merge(points_all, dataset_temp, by=c("uid", "year"), all=T)
  rm(dataset_temp); gc()
}

for(dataset in DATASETS_TIME_VARYING_ALWAYS_FULL_WOREDA){
  print(dataset)
  dataset_temp <- readRDS(file.path(finaldata_file_path, "woreda_panel_hdx_csa", "individual_datasets", dataset)) %>% data.table 
  points_all <- merge(points_all, dataset_temp, by=c("uid", "year"), all=T)
  rm(dataset_temp); gc()
}

for(dataset in DATASETS_TIME_INVARIANT){
  print(dataset)
  dataset_temp <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", dataset)) %>% data.table 
  points_all <- merge(points_all, dataset_temp, by="uid", all.x=T, all=T)
  rm(dataset_temp); gc()
}

for(dataset in DATASETS_ROAD_SEP_CHNG_NAMES){
  print(dataset)
  
  ## Load data
  dataset_temp <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", dataset)) %>% data.table 
  
  ## Rename variables
  vars_to_change <- names(dataset_temp)[!(names(dataset_temp) %in% c("uid", "year"))]
  names(dataset_temp)[names(dataset_temp) %in% vars_to_change] <-
    paste0(names(dataset_temp)[names(dataset_temp) %in% vars_to_change], "_rdsep")
  
  for(var in names(dataset_temp)[!(names(dataset_temp) %in% c("uid", "year"))]){
    names(dataset_temp)
  }
  
  ## Merge
  points_all <- merge(points_all, dataset_temp, by=c("uid", "year"), all=T)
  rm(dataset_temp); gc()
  
}

# Export -----------------------------------------------------------------------

# Can remove this later after re-run everything; exlcuding woredas with no 
# roads - before we included whole woreda
points <- readRDS(file.path(finaldata_file_path, DATASET_TYPE,"individual_datasets", "points.Rds")) 
points_all <- points_all[points_all$uid %in% points$uid,]

saveRDS(points_all, file.path(finaldata_file_path, DATASET_TYPE, "merged_datasets", "grid_data.Rds"))
