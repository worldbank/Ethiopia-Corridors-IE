# Ethiopia IE

# Extract Data Master
# NOTE: Run _ethiopia_ie_master.R beforehand

# Create unit-level datasets and extract data to those units. Creates grid and
# woreda level datasets.

# Parameters -------------------------------------------------------------------

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

DATASET_TYPE <- "dmspols_grid_ethiopia"

# Some scripts check whether DATASET_TYPE is a grid or polygon (eg, woreda) level.
# Inidates whether grid level for if/else statements for script
GRID_DATASET <- grepl("grid", DATASET_TYPE)

#### CHUNK SIZE
# For some functions, we break up the dataset into chunks. These are vectorized
# functions; however, vectorizing across the whole sample (eg, 1km grid across
# all of Ethiopia) would take up too much memory. Consequently, we vectorize
# into manageable chunks. Chunk size differs depending on grid level or woreda
# level.
if(GRID_DATASET){
  CHUNK_SIZE_DIST_ROADS <- 1250
} else{
  CHUNK_SIZE_DIST_ROADS <- 3
}

# Some functions set up to use multiple cores (mclapply). Number of cores to use.
MCCORS_DIST_ROADS <- 1 

# Close to road threshold (kilometers)
DIST_THRESH <- 2 

# Run Script Parameters --------------------------------------------------------
#### RUN SCRIPT PARAMETERS
CREATE_UNIT_LEVEL_DATASET <- F

EXTRACT_DATA <- T
OVERWRITE_EXTRACTED_DATA <- T # Checks if data already extracted. If T, re-extracts
# data. If F, skips extracting data

# RUN SCRIPTS ==================================================================

# ** Create Unit Level Datasets ------------------------------------------------
if(CREATE_UNIT_LEVEL_DATASET){
  
  # scripts <- c("create_dmspols_grid_nearroad.R",
  #              "create_dmspols_grid_nearroad_randomsample.R",
  #              "create_woreda.R",
  #              "create_clusters_of_globcover_urban.R",
  #              "create_clusters_of_ntl.R")
  
  # for(script_i in scripts){
  #   print(paste(script_i, "----------------------------------------------------"))
  #   source(file.path(rsdp_impact_prep_data_code_file_path, "01_create_initial_unitlevel_dataset", script_i))
  # } 
  
  source(file.path(rsdp_impact_prep_data_code_file_path, 
                   "01_create_initial_unitlevel_dataset", 
                   paste0("create_",DATASET_TYPE,".R")))
  
}

# ** Extract Data to Grids -----------------------------------------------------
if(EXTRACT_DATA){
  
  ## Scripts for all unit types
  scripts_all_units <- file.path(rsdp_impact_prep_data_code_file_path, 
                                 "02_extract_variables") %>%
    list.files(pattern = ".R", full.names = T)
  
  ## Scripts specific to units
  if(GRID_DATASET | grepl("clusters", DATASET_TYPE)){
    scripts_unit_specific <- file.path(rsdp_impact_prep_data_code_file_path, 
                                       "02_extract_variables_grid_cluster_specific") %>%
      list.files(pattern = ".R", full.names = T)
  } 
  
  if(grepl("woreda", DATASET_TYPE)){
    scripts_unit_specific <- file.path(rsdp_impact_prep_data_code_file_path, 
                                       "02_extract_variables_woreda_specific") %>%
      list.files(pattern = ".R", full.names = T)
  }
  
  ## Market Access Scripts
  if(grepl("woreda|clusters_", DATASET_TYPE)){
    scripts_market_access <- file.path(rsdp_impact_prep_data_code_file_path, 
                                       "02_extract_market_access") %>%
      list.files(pattern = ".R", full.names = T)
    
    scripts_all_units <- c(scripts_all_units, scripts_market_access)
  }
  
  ## Merge scripts
  scripts <- c(scripts_all_units, scripts_unit_specific) %>% sort()
  
  ## Check which data already extracted
  if(OVERWRITE_EXTRACTED_DATA %in% F){
    ## List of all datasets to be created
    dataset_names <- scripts %>% 
      str_replace_all(".*/", "") %>% 
      str_replace_all("extract_", "") %>%
      paste0("ds") # from .R to .Rds
    
    ## List of datasets already extracted
    extracted_datasets <- file.path(panel_rsdp_imp_data_file_path,
                                    DATASET_TYPE,
                                    "individual_datasets") %>%
      list.files()
    
    ## Updated list of scripts to extract
    scripts <- scripts[!(dataset_names %in% extracted_datasets)]
  }
  
  ## Remove select scripts for Ethiopia grid
  rm_eth_grid <- c("extract_distance_roads_improved_by_speedlimit_after.R",
                   "extract_distance_roads_improved_by_speedlimit_before.R",
                   "extract_distance_roads_by_speedlimit.R")
  if(DATASET_TYPE %in% "dmspols_grid_ethiopia"){
    rm_eth_grid_rx <- rm_eth_grid %>% paste(collapse = "|")
    scripts <- scripts[!grepl(rm_eth_grid_rx, scripts)]
  }
  
  ## Remove select scrips for all
  rm_all <- c("extract_distance_roads_improved_by_speedlimit_before.R")
  rm_all_rx <- rm_all %>% paste(collapse = "|")
  scripts <- scripts[!grepl(rm_all_rx, scripts)]
  
  ## Run scripts
  for(script_i in rev(scripts)){
    print(paste(script_i, "----------------------------------------------------"))
    source(script_i)
  } 
  
}








