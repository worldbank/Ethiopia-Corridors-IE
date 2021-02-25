# Globcover Urban Master

#### DATASET
# Defines dataset to run analysis on. Either at woreda level, grid level, or
# grid subsample:
# OPTIONS:
# --"dmspols_grid_ethiopia": Grid in Ethiopia
# --"dmspols_grid_nearroad": Near 10km of any road as of 2016
# --"dmspols_grid_nearroad_randomsample": Random sample of above
# --"woreda": Woreda polygons
# --"clusters_of_globcover_urban": Urban Clusters from Globcover
# --"clusters_of_ntl": Urban Clusters from NTL
# --"clusters_of_ntlall": Urban Clusters from NTL

DATASET_TYPE <- "clusters_of_globcover_urban"

# Other Parameters -------------------------------------------------------------
GRID_DATASET <- grepl("grid", DATASET_TYPE)

if(GRID_DATASET){
  CHUNK_SIZE_DIST_ROADS <- 1250
} else{
  CHUNK_SIZE_DIST_ROADS <- 3
}

MCCORS_DIST_ROADS <- 1 

# Close to road threshold (kilometers)
DIST_THRESH <- 5

# Run Scripts ------------------------------------------------------------------
## CREATE DATASET
source(file.path(rsdp_impact_prep_data_code_file_path, "01_create_initial_unitlevel_dataset", paste0("create_",DATASET_TYPE,".R")))

## EXTRACT VARIABLES
source(file.path(rsdp_impact_prep_data_code_file_path, "02_extract_variables", "extract_distance_cities.R"))
source(file.path(rsdp_impact_prep_data_code_file_path, "02_extract_variables", "extract_distance_hypothetical_road_least_cost_mst.R"))
source(file.path(rsdp_impact_prep_data_code_file_path, "02_extract_variables", "extract_distance_hypothetical_road_min_dist_mst.R"))
source(file.path(rsdp_impact_prep_data_code_file_path, "02_extract_variables", "extract_distance_roads_any_2012_ever.R"))
source(file.path(rsdp_impact_prep_data_code_file_path, "02_extract_variables", "extract_distance_roads_any_2016_ever.R"))
source(file.path(rsdp_impact_prep_data_code_file_path, "02_extract_variables", "extract_distance_roads_any_improved_by2012.R"))
source(file.path(rsdp_impact_prep_data_code_file_path, "02_extract_variables", "extract_distance_roads_any_improved_ever.R"))
source(file.path(rsdp_impact_prep_data_code_file_path, "02_extract_variables", "extract_distance_roads_improved_by_speedlimit_after.R"))
#source(file.path(rsdp_impact_prep_data_code_file_path, "02_extract_variables", "extract_distance_roads_improved_by_speedlimit_after.R"))
#source(file.path(rsdp_impact_prep_data_code_file_path, "02_extract_variables", "extract_distance_roads_by_rsdp_phase.R"))
#source(file.path(rsdp_impact_prep_data_code_file_path, "02_extract_variables", "extract_distance_roads_by_speedlimit.R"))
source(file.path(rsdp_impact_prep_data_code_file_path, "02_extract_variables", "extract_dmspols_intercalibrated_zhang.R"))
source(file.path(rsdp_impact_prep_data_code_file_path, "02_extract_variables", "extract_dmspols.R"))
source(file.path(rsdp_impact_prep_data_code_file_path, "02_extract_variables", "extract_dmspolsharmon.R"))
source(file.path(rsdp_impact_prep_data_code_file_path, "02_extract_variables", "extract_globcover.R"))
#source(file.path(rsdp_impact_prep_data_code_file_path, "02_extract_variables", "extract_gpw.R"))
source(file.path(rsdp_impact_prep_data_code_file_path, "02_extract_variables", "extract_ndvi.R"))
source(file.path(rsdp_impact_prep_data_code_file_path, "02_extract_variables", "extract_precipitation.R"))
source(file.path(rsdp_impact_prep_data_code_file_path, "02_extract_variables", "extract_temperature.R"))
#source(file.path(rsdp_impact_prep_data_code_file_path, "02_extract_variables", "extract_viirs.R"))













rsdp_impact_code_file_path <- file.path(code_file_path, "DataWork", "Panel Data RSDP Impacts")
rsdp_impact_prep_data_code_file_path <- file.path(code_file_path, "DataWork", "Panel Data RSDP Impacts", "01_extract_data_to_datasets")
rsdp_impact_analysis_code_file_path <- file.path(code_file_path, "DataWork", "Panel Data RSDP Impacts", "02_prep_dataset_and_analysis")




