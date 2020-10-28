# Merge Datasets Together

#### Parameters
# Vector of all baseline and endline years that would later use
YEARS_USE <- c(1996, 2012, 2106)

# Load Data / Create Dataset Lists -----------------------------------------------

## Load dataset to merge into
points_all <- readRDS(file.path(panel_rsdp_imp_data_file_path, "dmspols_grid_ethiopia", 
                                "individual_datasets", "dmspols.Rds")) %>% 
  data.table

points_all <- points_all[points_all$year %in% YEARS_USE,]

#### Names of datasets to merge in
# Separate by:
#  -- time invarient (merge by cell_id)
#  -- time varying (merge by cell_id and year)

DATASETS_TIME_INVARIANT <- c("distance_roads_any_improved_ever.Rds",
                             "distance_roads_any_2016_ever.Rds",
                             "distance_hypothetical_road_least_cost_mst.Rds",
                             "distance_cities.Rds")

DATASETS_TIME_VARYING <- c("viirs.Rds",
                           #"distance_roads_improved_by_speedlimit_after.Rds",
                           #"distance_roads_by_speedlimit.Rds",
                           "temperature.Rds",
                           "precipitation.Rds",
                           "ndvi.Rds",
                           "globcover.Rds",
                           "dmspols_intercalibrated_zhang.Rds")

# Merge ------------------------------------------------------------------------
for(dataset in DATASETS_TIME_VARYING){
  print(dataset)
  
  dataset_temp <- readRDS(file.path(panel_rsdp_imp_data_file_path, "dmspols_grid_ethiopia", 
                                    "individual_datasets", dataset)) %>% data.table
  
  dataset_temp <- dataset_temp[dataset_temp$year %in% YEARS_USE,]
  
  points_all <- merge(points_all, dataset_temp, by=c("cell_id", "year"), all=T)
}

for(dataset in DATASETS_TIME_INVARIANT){
  print(dataset)
  
  dataset_temp <- readRDS(file.path(panel_rsdp_imp_data_file_path, "dmspols_grid_ethiopia", 
                                    "individual_datasets", dataset)) %>% data.table 
  
  points_all <- merge(points_all, dataset_temp, by="cell_id", all=T)
}

# Export -----------------------------------------------------------------------
saveRDS(points_all, file.path(panel_rsdp_imp_data_file_path, "dmspols_grid_ethiopia", "merged_datasets", "panel_data.Rds"))






