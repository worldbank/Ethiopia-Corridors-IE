# Merge Datasets Together

# Load Data / Create Dataset Lists -----------------------------------------------

#### Load dataset to merge into
points_all <- readRDS(file.path(panel_rsdp_imp_data_file_path, "clusters_of_ntl", "individual_datasets", "dmspols.Rds"))

points_cluster_df <- points_all %>%
  dplyr::select(cell_id, cluster_n_cells) %>%
  distinct()

points_all$cluster_n_cells <- NULL

#### Names of datasets to merge in
# Separate by:
#  -- time invarient (merge by cell_id)
#  -- time varying (merge by cell_id and year)

DATASETS_TIME_INVARIANT <- c("distance_roads_by_rsdp_phase.Rds",
                             "distance_roads_any_improved_ever.Rds",
                             "distance_roads_any_2016_ever.Rds",
                             "distance_hypothetical_road_least_cost_mst.Rds",
                             "distance_hypothetical_road_min_dist_mst.Rds",
                             "distance_cities.Rds")

DATASETS_TIME_VARYING <- c("viirs.Rds",
                           "temperature.Rds",
                           "precipitation.Rds",
                           "ndvi.Rds",
                           "globcover.Rds",
                           "dmspols_intercalibrated_zhang.Rds",
                           #"distance_roads_improved_by_speedlimit_before.Rds",
                           "distance_roads_improved_by_speedlimit_after.Rds",
                           "distance_roads_by_speedlimit.Rds")

# Merge ------------------------------------------------------------------------
for(dataset in DATASETS_TIME_VARYING){
  print(dataset)
  dataset_temp <- readRDS(file.path(panel_rsdp_imp_data_file_path, "clusters_of_ntl", "individual_datasets", dataset)) 
  dataset_temp$cluster_n_cells <- NULL
  points_all <- merge(points_all, dataset_temp, by=c("cell_id", "year"), all=T)

}

for(dataset in DATASETS_TIME_INVARIANT){
  print(dataset)
  dataset_temp <- readRDS(file.path(panel_rsdp_imp_data_file_path, "clusters_of_ntl", "individual_datasets", dataset))
  dataset_temp$cluster_n_cells <- NULL
  points_all <- merge(points_all, dataset_temp, by="cell_id", all=T)
}

points_all <- merge(points_all, points_cluster_df, by="cell_id", all=T)

# Export -----------------------------------------------------------------------
saveRDS(points_all, file.path(panel_rsdp_imp_data_file_path, "clusters_of_ntl", "merged_datasets", "panel_data.Rds"))


