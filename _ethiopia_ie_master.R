# Ethiopia IE 
# Master R Script

# In order to source a script as a local job, include this at top of script
# source("~/Documents/Github/Ethiopia-Corridors-IE/Code/_ethiopia_ie_master.R")

# Filepaths --------------------------------------------------------------------
if(Sys.info()[["user"]] == "WB554990") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE - Merge Budget Data With Shapefile"
if(Sys.info()[["user"]] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE"
if(Sys.info()[["user"]] == "robmarty") project_file_path <- "~/Dropbox/World Bank/IEs/Ethiopia IE"

if(Sys.info()[["user"]] == "robmarty") code_file_path <- "~/Documents/Github/Ethiopia-Corridors-IE"

rawdata_file_path <- file.path(project_file_path, "Data", "RawData")
outputs_for_grid <- file.path(project_file_path, "Data", "IntermediateData", "Outputs for Grid")
finaldata_file_path <- file.path(project_file_path, "Data", "FinalData")
figures_file_path <- file.path(project_file_path,"Outputs", "Results", "Figures")
tables_file_path <- file.path(project_file_path,"Outputs", "Results", "Tables")

# Parameters -------------------------------------------------------------------

#### DATASET
# Defines dataset to run analysis on. Either at woreda level, grid level, or
# grid subsample:
# OPTIONS:
# --"dmspols_grid_dataset_nearroad": DMSP-OLS level dataset restricted to cells
#                                    near a road
# --"dmspols_grid_dataset_randomsample": DMSP-OLS level dataset; random sample
# --"woreda_panel_hdx_csa": Woreda level

#DATASET_TYPE <- "woreda_panel_hdx_csa"
DATASET_TYPE <- "woreda_panel_hdx_csa_nearroad"

#DATASET_TYPE <- "dmspols_grid_dataset_nearroad"

#### CHUNK SIZE
# For some functions, we break up the dataset into chunks. These are vectorized
# functions; however, vectorizing across the whole sample (eg, 1km grid across
# all of Ethiopia) would take up too much memory. Consequently, we vectorize
# into manageable chunks. Chunk size differs depending on grid level or woreda
# level.
if(DATASET_TYPE %in% c("woreda_panel_hdx_csa",
                       "woreda_panel_hdx_csa_nearroad")){
  CHUNK_SIZE_DIST_ROADS <- 3
} else{
  CHUNK_SIZE_DIST_ROADS <- 1250
}

#### YEAR SUBSETS
road_year <- list(all = 1996:2016,     
                  dmspols = 1996:2012, 
                  viirs = 2013:2016,  
                  phase1 = 1997:2002, # 1997:2002
                  phase2 = 2003:2007, # 2002:2007
                  phase3 = 2008:2010, # 2007:2010
                  phase4 = 2011:2016) # 2010:2015


# Parameters for Grid Analysis
MCCORS_DIST_ROADS <- 1
TYPE <- c("DMSPOLS") # globcover, DMSPOLS
UTM_ETH <- '+init=epsg:20138'
DIST_THRESH <- 2

GRID_DATASET <- grepl("grid", DATASET_TYPE)


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
library(spdep)
library(doBy)
library(stargazer)
library(scales)
library(rasterVis)
library(ggpubr)
library(readr)
library(gdistance)
source("https://raw.githubusercontent.com/ramarty/fast-functions/master/R/functions_in_chunks.R")

# Common Functions -------------------------------------------------------------
lm_confint_tidy <- function(lm, years_since_variable){
  lm_confint <- confint(lm) %>% 
    as.data.frame
  names(lm_confint) <- c("p025", "p975")
  lm_confint$b <- (lm_confint$p025 + lm_confint$p975)/2
  lm_confint$variable <- row.names(lm_confint)
  
  lm_confint <- lm_confint[!grepl("cluster_id)|year)|Intercept)", lm_confint$variable),]
  lm_confint$years_since_improved <- gsub(years_since_variable, "", lm_confint$variable) %>% as.numeric
  
  return(lm_confint)
}

lm_post_confint_tidy <- function(lm){
  
  lm_confint <- confint(lm) %>% 
    as.data.frame
  names(lm_confint) <- c("p025", "p975")
  lm_confint$b <- (lm_confint$p025 + lm_confint$p975)/2
  lm_confint$variable <- row.names(lm_confint)
  
  lm_confint$tvalue <- summary(lm)$coefficients[,3] %>% as.vector()
  lm_confint$pvalue <- summary(lm)$coefficients[,4] %>% as.vector()
  
  return(lm_confint)
}

pause_gc <- function(GRID_DATASET){
  if(GRID_DATASET){
    Sys.sleep(1)
    gc()
    Sys.sleep(1)
    gc()
    Sys.sleep(1)
    gc()
    Sys.sleep(1)
  } 
  return(NULL)
}


# Run Scripts ------------------------------------------------------------------
##### Extract Data to Grids
if(T){
  grid_scripts <- c("02b_extract_dmspols.R", 
                    "02b_extract_ndvi.R",
                    "02b_extract_viirs.R",
                    "02b_extract_precip.R",
                    "02b_extract_temperature.R",
                    "02b_extract_viirs.R",
                    "02d_distance_cities.R",
                    "02e_extract_globcover.R",
                    "02f_extract_dmspols_intercalibrated_zhang2016_method.R")
  for(script_i in grid_scripts){
    print(paste(script_i, "----------------------------------------------------"))
    source(file.path(code_file_path, "02_create_main_analysis_datasets", "02_extract_variables", script_i))
  } 
}

if(T){
  grid_scripts <- c("02c_extract_distance_improved_roads_by_speedlimit_after.R",
                    "02c_extract_distance_improved_roads_by_speedlimit_before.R",
                    "02c_extract_distance_roads_by_phase.R",
                    "02c_extract_distance_roads_by_speedlimit.R",
                    "02c_extract_distance_anyroad_ever.R")
  for(script_i in grid_scripts){
    print(paste(script_i, "----------------------------------------------------"))
    source(file.path(code_file_path, "02_create_main_analysis_datasets", "03_extract distance_road", script_i))
  } 
}

if(T){
  grid_scripts <- c("03a_woreda_traveltime_dataset.R", 
                    "03b_computer_market_access.R")
  for(script_i in grid_scripts){
    print(paste(script_i, "----------------------------------------------------"))
    source(file.path(code_file_path, "02_create_main_analysis_datasets", "04_compute_market_access", script_i))
  } 
}







