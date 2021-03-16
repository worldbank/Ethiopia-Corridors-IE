# Ethiopia IE 
# Master R Script

# In order to source a script as a local job, include this at top of script
# source("~/Documents/Github/Ethiopia-Corridors-IE/Code/_ethiopia_ie_master.R")

# Filepaths --------------------------------------------------------------------
if(Sys.info()[["user"]] == "WB554990") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE - Merge Budget Data With Shapefile"
if(Sys.info()[["user"]] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE"
if(Sys.info()[["user"]] == "robmarty") project_file_path <- "~/Dropbox/World Bank/IEs/Ethiopia IE"
if(Sys.info()[["user"]] == "andm2")    project_file_path <- "C:/Users/andm2/Dropbox/WorldBank/Ethiopia IE"

if(Sys.info()[["user"]] == "robmarty") code_file_path <- "~/Documents/Github/Ethiopia-Corridors-IE"
if(Sys.info()[["user"]] == "WB521633") code_file_path <- "C:/Users/wb521633/Documents/Github/Ethiopia-Corridors-IE"
if(Sys.info()[["user"]] == "andm2")    code_file_path <- "G:/Work/Ethiopia/Ethiopia-Corridors-IE"

if(Sys.info()[["user"]] == "robmarty") overleaf_path <- "~/Dropbox/Apps/Overleaf/The Impact of Ethiopia RSDP Evidence from Satellite Data"

data_file_path <- file.path(project_file_path, "Data")
panel_rsdp_imp_file_path <- file.path(project_file_path, "Data", "Panel Data RSDP Impacts")
panel_rsdp_imp_data_file_path <- file.path(panel_rsdp_imp_file_path, "Data")
#figures_file_path <- file.path(project_file_path,"Outputs", "Results", "Figures")
#tables_file_path <- file.path(project_file_path,"Outputs", "Results", "Tables")

rsdp_impact_code_file_path <- file.path(code_file_path, "DataWork", "Panel Data RSDP Impacts")
rsdp_impact_prep_data_code_file_path <- file.path(code_file_path, "DataWork", "Panel Data RSDP Impacts", "01_extract_data_to_datasets")
rsdp_impact_analysis_code_file_path <- file.path(code_file_path, "DataWork", "Panel Data RSDP Impacts", "02_prep_dataset_and_analysis")

# TODO: to change
paper_figures <- file.path(overleaf_path, "Figures")
paper_tables <- file.path(overleaf_path, "Tables")
#paper_figures <- file.path(project_file_path, "Paper", "figures")
#paper_tables <- file.path(project_file_path, "Paper", "tables")

# Parameters -------------------------------------------------------------------

#### YEAR SUBSETS
road_year <- list(all = 1996:2016,     
                  dmspols = 1996:2012, 
                  viirs = 2013:2016,  
                  phase1 = 1997:2002, # 1997:2002
                  phase2 = 2003:2007, # 2002:2007
                  phase3 = 2008:2010, # 2007:2010
                  phase4 = 2011:2016) # 2010:2015

# Ethiopia UTM
UTM_ETH <- '+init=epsg:20138' # Ethiopia UTM

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
library(RColorBrewer)
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
library(riverplot)
library(leaflet)
library(TTR)
library(tidyselect)
library(dvmisc)
library(purrr)
library(facetscales) # devtools::install_github("zeehio/facetscales")

# Functions
source("https://raw.githubusercontent.com/ramarty/fast-functions/master/R/functions_in_chunks.R")
source(file.path(code_file_path, "Functions", "commonly_used.R"))
source(file.path(code_file_path, "Functions", "rename_lm_vars.R"))


str_remove_vec <- function(x, rx){
  # Remove items in vector "x" that contain "rx"
  x[!grepl(rx, x)]
}