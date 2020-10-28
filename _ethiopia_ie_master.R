# Ethiopia IE 
# Master R Script

# In order to source a script as a local job, include this at top of script
# source("~/Documents/Github/Ethiopia-Corridors-IE/Code/_ethiopia_ie_master.R")

# Parameters -------------------------------------------------------------------

#### DATASET
# Defines dataset to run analysis on. Either at woreda level, grid level, or
# grid subsample:
# OPTIONS:
# --"dmspols_grid_ethiopia": Grid in Ethiopia
# --"dmspols_grid_nearroad": Near 10km of any road as of 2016
# --"dmspols_grid_nearroad_randomsample": Random sample of above
# --"woreda": Woreda polygons
DATASET_TYPE <- "dmspols_grid_nearroad"

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

#### YEAR SUBSETS
road_year <- list(all = 1996:2016,     
                  dmspols = 1996:2012, 
                  viirs = 2013:2016,  
                  phase1 = 1997:2002, # 1997:2002
                  phase2 = 2003:2007, # 2002:2007
                  phase3 = 2008:2010, # 2007:2010
                  phase4 = 2011:2016) # 2010:2015

# Some functions set up to use multiple cores (mclapply). Number of cores to use.
MCCORS_DIST_ROADS <- 1 

# Ethiopia UTM
UTM_ETH <- '+init=epsg:20138' # Ethiopia UTM

# Close to road threshold (kilometers)
DIST_THRESH <- 2 

#### RUN SCRIPT PARAMETERS
CREATE_UNIT_LEVEL_DATASETS <- F

EXTRACT_DATA <- F
OVERWRITE_EXTRACTED_DATA <- F # Checks if data already extracted. If T, re-extracts
# data. If F, skips extracting data

# Filepaths --------------------------------------------------------------------
if(Sys.info()[["user"]] == "WB554990") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE - Merge Budget Data With Shapefile"
if(Sys.info()[["user"]] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE"
if(Sys.info()[["user"]] == "robmarty") project_file_path <- "~/Dropbox/World Bank/IEs/Ethiopia IE"
if(Sys.info()[["user"]] == "andm2")    project_file_path <- "C:/Users/andm2/Dropbox/WorldBank/Ethiopia IE"

if(Sys.info()[["user"]] == "robmarty") code_file_path <- "~/Documents/Github/Ethiopia-Corridors-IE"
if(Sys.info()[["user"]] == "andm2")    code_file_path <- "G:/Work/Ethiopia/Ethiopia-Corridors-IE"

data_file_path <- file.path(project_file_path, "Data")
panel_rsdp_imp_file_path <- file.path(project_file_path, "Data", "Panel Data RSDP Impacts")
panel_rsdp_imp_data_file_path <- file.path(panel_rsdp_imp_file_path, "Data")
#figures_file_path <- file.path(project_file_path,"Outputs", "Results", "Figures")
#tables_file_path <- file.path(project_file_path,"Outputs", "Results", "Tables")

rsdp_impact_code_file_path <- file.path(code_file_path, "DataWork", "Panel Data RSDP Impacts")
rsdp_impact_prep_data_code_file_path <- file.path(code_file_path, "DataWork", "Panel Data RSDP Impacts", "01_extract_data_to_datasets")

# Packages ---------------------------------------------------------------------
library(AER)
library(estimatr)
library(labelled)
library(clusterSEs)
library(rgdal)
library(raster)
library(terra)
library(velox)
library(dplyr)
library(rgeos)
library(parallel)
library(pbmcapply)
library(data.table)
library(haven)
library(spex)
library(sf)
library(tidyr)
library(lfe)
library(reshape)
library(dplyr)
library(tibble)
library(ggplot2)
library(data.table)
library(coefplot)
library(stringr)
library(spdep)
library(doBy)
library(stargazer)
library(scales)
library(rasterVis)
library(ggpubr)
library(readr)
library(gdistance)
library(shp2graph)

# Functions
source("https://raw.githubusercontent.com/ramarty/fast-functions/master/R/functions_in_chunks.R")
source(file.path(code_file_path, "Functions", "commonly_used.R"))

# RUN SCRIPTS ==================================================================

# ** Create Unit Level Datasets ------------------------------------------------
if(CREATE_UNIT_LEVEL_DATASETS){
  
  scripts <- c("create_dmspols_grid_nearroad.R",
               "create_dmspols_grid_nearroad_randomsample.R",
               "create_woreda.R")
  
  for(script_i in scripts){
    print(paste(script_i, "----------------------------------------------------"))
    source(file.path(rsdp_impact_prep_data_code_file_path, "01_create_initial_unitlevel_dataset", script_i))
  } 
  
}

# ** Extract Data to Grids -----------------------------------------------------
if(EXTRACT_DATA){
  
  ## Scripts for all unit types
  scripts_all_units <- file.path(rsdp_impact_prep_data_code_file_path, 
                                 "02_extract_variables") %>%
    list.files(pattern = ".R", full.names = T)
  
  ## Scripts specific to units
  if(GRID_DATASET){
    scripts_unit_specific <- file.path(rsdp_impact_prep_data_code_file_path, 
                                       "02_extract_variables_grid_specific") %>%
      list.files(pattern = ".R", full.names = T)
  } else{
    scripts_unit_specific <- file.path(rsdp_impact_prep_data_code_file_path, 
                                       "02_extract_variables_woreda_specific") %>%
      list.files(pattern = ".R", full.names = T)
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
  
  ## Run scripts
  for(script_i in scripts){
    print(paste(script_i, "----------------------------------------------------"))
    source(script_i)
  } 
  
}








