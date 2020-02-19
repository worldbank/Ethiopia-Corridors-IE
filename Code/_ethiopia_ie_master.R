# Ethiopia IE 
# Master R Script

# Filepaths --------------------------------------------------------------------
if(Sys.info()[["user"]] == "WB554990") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE - Merge Budget Data With Shapefile"
if(Sys.info()[["user"]] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE"
if(Sys.info()[["user"]] == "robmarty") project_file_path <- "~/Dropbox/World Bank/IEs/Ethiopia IE"

if(Sys.info()[["user"]] == "robmarty") code_file_path <- "~/Documents/Github/Ethiopia-Corridors-IE/Code"

rawdata_file_path <- file.path(project_file_path, "Data", "RawData")
outputs_for_grid <- file.path(project_file_path, "Data", "IntermediateData", "Outputs for Grid")
finaldata_file_path <- file.path(project_file_path, "Data", "FinalData")
figures_file_path <- file.path(project_file_path,"Outputs", "Results", "Figures")
tables_file_path <- file.path(project_file_path,"Outputs", "Results", "Tables")

# Parameters -------------------------------------------------------------------

# dmspols_grid_dataset_nearroad
#DATASET_TYPE <- "dmspols_grid_dataset_randomsample"
#DATASET_TYPE <- "woreda_panel_hdx_csa"
DATASET_TYPE <- "dmspols_grid_dataset_nearroad"

if(DATASET_TYPE %in% c("woreda_panel_hdx_csa")){
  CHUNK_SIZE_DIST_ROADS <- 3
} else{
  CHUNK_SIZE_DIST_ROADS <- 1250
}

# Parameters for Grid Analysis

MCCORS_DIST_ROADS <- 1
TYPE <- c("DMSPOLS") # globcover, DMSPOLS
UTM_ETH <- '+init=epsg:20138'
DIST_THRESH <- 2

# Packages ---------------------------------------------------------------------
library(rgdal)
library(raster)
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
library(doBy)
library(stargazer)
library(scales)
library(rasterVis)
library(ggpubr)
library(readr)
library(gdistance)
source("https://raw.githubusercontent.com/ramarty/fast-functions/master/R/functions_in_chunks.R")

# Run Scripts ------------------------------------------------------------------
##### Extract Data to Grids
if(F){
grid_scripts <- c("02a_extract_gadm.R", 
                  #"02b_extract_ndvi.R",
                  #"02b_extract_viirs.R",
                  "02c_extract_distance_improved_roads_by_speedlimit_after.R",
                  #"02c_extract_distance_improved_roads_by_speedlimit_before.R",
                  #"02c_extract_distance_roads_by_phase.R",
                  #"02c_extract_distance_roads_by_speedlimit.R",
                  "02d_distance_cities.R",
                  "02e_extract_globcover.R",
                  "02f_extract_dmspols_intercalibrated_zhang2016_method.R",
                  "04b_merge_datasets_dmspols.R", 
                  "05_create_variables_for_analysis.R")
for(script_i in grid_scripts){
  print(paste(script_i, "----------------------------------------------------"))
  source(file.path(code_file_path, "create_clean_datasets", "02_create_gridded_datasets", script_i))
} 
}








