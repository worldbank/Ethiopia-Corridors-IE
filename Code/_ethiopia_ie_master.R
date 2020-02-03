# Ethiopia IE 
# Master R Script

# Filepaths --------------------------------------------------------------------
if(Sys.info()[["user"]] == "WB554990") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE - Merge Budget Data With Shapefile"
if(Sys.info()[["user"]] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE"
if(Sys.info()[["user"]] == "robmarty") project_file_path <- "~/Dropbox/World Bank/IEs/Ethiopia IE"

rawdata_file_path <- file.path(project_file_path, "Data", "RawData")
outputs_for_grid <- file.path(project_file_path, "Data", "IntermediateData", "Outputs for Grid")
finaldata_file_path <- file.path(project_file_path, "Data", "FinalData")
figures_file_path <- file.path(project_file_path,"Outputs", "Results", "Figures")
tables_file_path <- file.path(project_file_path,"Outputs", "Results", "Tables")

# Parameters -------------------------------------------------------------------

DATASET_TYPE <- "dmspols_grid_dataset_randomsample"
#DATASET_TYPE <- "woreda_panel_hdx_csa"

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




