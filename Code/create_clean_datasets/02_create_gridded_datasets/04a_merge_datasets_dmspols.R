# Merge Datasets Together

# Load Data / Create Dataset Lists -----------------------------------------------
# Load data that we'll merge datasets into and create lists of datasets to merge
# into this dataset.

#### Load dataset to merge into
points_all <- readRDS(file.path(finaldata_file_path, DATASET_TYPE,"individual_datasets", "points_dmspols.Rds")) %>% data.table 

#### Names of datasets to merge in
# Separate by time invarient (merge by cell_id) and time varying (merge by cell_id 
# and year)
DATASETS_TIME_INVARIANT <- c("points_gadm.Rds", "points_distance_cities.Rds", "points_distance_rsdp_phases.Rds")
DATASETS_TIME_VARYING <- c("points_viirs.Rds",
                           "points_ndvi.Rds",
                           "points_globcover.Rds",
                           "points_dmspols_zhang2016.Rds",
                           "points_distance_roads_byspeed.Rds",
                           "points_distance_improved_roads_byspeed_before.Rds",
                           "points_distance_improved_roads_byspeed_after.Rds")

# Merge ------------------------------------------------------------------------
for(dataset in DATASETS_TIME_VARYING){
  print(dataset)
  dataset_temp <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", dataset)) %>% data.table 
  points_all <- merge(points_all, dataset_temp, by=c("cell_id", "year"), all=T)
  rm(dataset_temp); gc()
}

for(dataset in DATASETS_TIME_INVARIANT){
  print(dataset)
  dataset_temp <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", dataset)) %>% data.table 
  points_all <- merge(points_all, dataset_temp, by="cell_id", all=T)
  rm(dataset_temp); gc()
}

# Export -----------------------------------------------------------------------
saveRDS(points_all, file.path(finaldata_file_path, DATASET_TYPE, "merged_datasets", "grid_data.Rds"))







# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
#### PURGATORY -----------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Load and Merge ---------------------------------------------------------------
points <- readRDS(file.path(outputs_for_grid, "DMSPOLS", "points.Rds")) %>% data.table 
points_dmspols <- readRDS(file.path(outputs_for_grid, "DMSPOLS", "points_dmspols.Rds")) %>% data.table 

points_all <- merge(points_dmspols, points, by="cell_id")

# Generate Cells for 5 percent sample ------------------------------------------
if(RANDOM_SAMPLE){
  cells_sample <- sample(x=unique(points_all$cell_id), 
                         size=ceiling(length(unique(points_all$cell_id))*0.20),
                         replace=F)
  points_all <- points_all[points_all$cell_id %in% cells_sample,]
}


# Merge in Variables -----------------------------------------------------------

for(dataset in DATASETS_TIME_VARYING){
  print(dataset)
  points_data <- readRDS(file.path(outputs_for_grid, "DMSPOLS", dataset)) %>% data.table 
  if(RANDOM_SAMPLE) points_data <- points_data[points_data$cell_id %in% cells_sample,]
  
  if(dataset == "points_distance_improved_roads_byspeed_2016file.Rds") names(points_data)[!(names(points_data) %in% c("cell_id", "year"))] <- paste0(names(points_data)[!(names(points_data) %in% c("cell_id", "year"))],"_improved")
  
  points_all <- merge(points_all, points_data, by=c("cell_id", "year"), all = T)
  rm(points_data)
  gc()
}

for(dataset in DATASETS_TIME_INVARIANT){
  print(dataset)
  points_data <- readRDS(file.path(outputs_for_grid, "DMSPOLS", dataset)) %>% data.table 
  if(RANDOM_SAMPLE) points_data <- points_data[points_data$cell_id %in% cells_sample,]
  
  points_all <- merge(points_all, points_data, by="cell_id")
  rm(points_data)
  gc()
}

# Add in Market Access ---------------------------------------------------------
append_MA_constantpop <- function(year){
  
  # If odd year, use previous year
  if(year %% 2 == 1){
    year_data <- year - 1
  } else{
    year_data <- year
  }
  
  print(year)
  points_MA <- readRDS(file.path(outputs_for_grid, "DMSPOLS", paste0("MA_",year_data,"_constantpopTrue.Rds"))) %>% dplyr::select(cell_id, MA)  %>% data.table 
  if(RANDOM_SAMPLE) points_MA <- points_MA[points_MA$cell_id %in% cells_sample,]
  
  points_MA$year <- year
  return(points_MA)
}

points_data_MA <- lapply(1996:2016, append_MA_constantpop) %>% 
                    bind_rows %>%
                    dplyr::rename(MA_constantpop = MA)

points_all <- merge(points_all, points_data_MA, by=c("cell_id", "year"), all.x=T)

# Export -----------------------------------------------------------------------
if(RANDOM_SAMPLE){
  saveRDS(points_all, file.path(finaldata_file_path, "dmspols_grid_dataset", "dmspols_level_dataset_5percentsample.Rds"))
} else{
  saveRDS(points_all, file.path(finaldata_file_path, "dmspols_grid_dataset", "dmspols_level_dataset.Rds"))
}

#saveRDS(points_all[points_all$year %in% seq(1992,2016,2),], file.path(finaldata_file_path, "dmspols_grid_dataset", "dmspols_level_dataset_evenyears.Rds"))





