# Merge Datasets Together

# TODO: Include prepping data in here.

# Memory intensive, so clean up garbage
for(i in 1:10) gc()

# Set seed, for creating a random subsample
set.seed(42)

DATASETS_TIME_INVARIANT <- c("points_gadm.Rds")
DATASETS_TIME_VARYING <- c("points_viirs.Rds",
                           "points_ndvi.Rds",
                           #"points_distance_roads_bytype.Rds",
                           "points_globcover.Rds",
                           "points_dmspols_zhang2016.Rds",
                           "points_distance_improved_roads_byspeed_2016file.Rds",
                           "points_distance_roads_byspeed_2016file.Rds")

# Load and Merge ---------------------------------------------------------------
points <- readRDS(file.path(outputs_for_grid, "DMSPOLS", "points.Rds")) %>% data.table 
points_dmspols <- readRDS(file.path(outputs_for_grid, "DMSPOLS", "points_dmspols.Rds")) %>% data.table 

points_all <- merge(points_dmspols, points, by="cell_id")

for(dataset in DATASETS_TIME_VARYING){
  print(dataset)
  points_data <- readRDS(file.path(outputs_for_grid, "DMSPOLS", dataset)) %>% data.table 
  
  if(dataset == "points_distance_improved_roads_byspeed_2016file.Rds") names(points_data)[!(names(points_data) %in% c("cell_id", "year"))] <- paste0(names(points_data)[!(names(points_data) %in% c("cell_id", "year"))],"_improved")
  
  points_all <- merge(points_all, points_data, by=c("cell_id", "year"), all = T)
  rm(points_data)
  gc()
}

for(dataset in DATASETS_TIME_INVARIANT){
  print(dataset)
  points_data <- readRDS(file.path(outputs_for_grid, "DMSPOLS", dataset)) %>% data.table 
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
  points_MA$year <- year
  return(points_MA)
}

points_data_MA <- lapply(1996:2016, append_MA_constantpop) %>% 
                    bind_rows %>%
                    dplyr::rename(MA_constantpop = MA)

points_all <- merge(points_all, points_data_MA, by=c("cell_id", "year"), all.x=T)

# Export -----------------------------------------------------------------------
saveRDS(points_all, file.path(finaldata_file_path, "dmspols_grid_dataset", "dmspols_level_dataset.Rds"))
saveRDS(points_all[points_all$year %in% seq(1992,2016,2),], file.path(finaldata_file_path, "dmspols_grid_dataset", "dmspols_level_dataset_evenyears.Rds"))
saveRDS(points_all[points_all$cell_id %in% sample(x=unique(points_all$cell_id), 
                                          size=ceiling(length(unique(points_all$cell_id))*0.20),
                                          replace=F),],
        file.path(finaldata_file_path, "dmspols_grid_dataset", "dmspols_level_dataset_5percentsample.Rds"))
        




