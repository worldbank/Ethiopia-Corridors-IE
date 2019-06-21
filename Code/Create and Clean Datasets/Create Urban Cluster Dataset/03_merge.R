# Identify Clusters of Lights

# Load Data --------------------------------------------------------------------
dmspols <- readRDS(file.path(finaldata_file_path, "urban_cluster_dataset", "individual_files", "urban_cluster_dmspols.Rds"))
dmspols_zhang <- readRDS(file.path(finaldata_file_path, "urban_cluster_dataset", "individual_files", "urban_cluster_dmspols_zhang2016.Rds"))
dist_roads <- readRDS(file.path(finaldata_file_path, "urban_cluster_dataset", "individual_files", "urban_cluster_distance_roads_byspeed_2016file.Rds"))
dist_improved_roads <- readRDS(file.path(finaldata_file_path, "urban_cluster_dataset", "individual_files", "urban_cluster_distance_improved_roads_byspeed_2016file.Rds"))
globcover <- readRDS(file.path(finaldata_file_path, "urban_cluster_dataset", "individual_files", "urban_cluster_globcover.Rds"))

names(dist_improved_roads)[grepl("distance_",names(dist_improved_roads))] <- paste0(names(dist_improved_roads)[grepl("distance_",names(dist_improved_roads))], "_improved")

dist_roads <- subset(dist_roads, select=-c(cluster_n_cells))
dist_improved_roads <- subset(dist_improved_roads, select=-c(cluster_n_cells))
dmspols_zhang <- subset(dmspols_zhang, select=-c(cluster_n_cells))
globcover <- subset(globcover, select=-c(cluster_n_cells))

# Merge ------------------------------------------------------------------------
df_all <- merge(dmspols, dist_roads, by=c("cluster_id", "year"))
df_all <- merge(df_all, dmspols_zhang, by=c("cluster_id", "year"))
df_all <- merge(df_all, dist_improved_roads, by=c("cluster_id", "year"))
df_all <- merge(df_all, globcover, by=c("cluster_id", "year"))

# Export -----------------------------------------------------------------------
write.csv(df_all, file.path(finaldata_file_path, "urban_cluster_dataset", "urban_cluster_data.csv"), row.names=F)
saveRDS(df_all, file.path(finaldata_file_path, "urban_cluster_dataset", "urban_cluster_data.Rds"))


