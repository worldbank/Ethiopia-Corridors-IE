# Ethiopia Corridors IE

# Github Organization
The Github repo is organized into the following main folders:

* __01_individual_dataset_work:__ Code that cleans and analyzes individual datasets.
* __02_create_main_analysis_datasets:__ For a dataset at a certain unit of analysis (grid level, woreda level, or survey dataset level), adds a number of variables to the dataset -- including RSDP variables and satellite variables.
* __03_rsdp_analysis:__ Analysis datasets generated in the previous step.
* __general_functions:__ Functions defined that are used throughout the project.

# Datasets

The datasets are stored in dropbox, in the /Data folder. Generally, there is one high-level folder per dataset with the following folders:

* __RawData:__ For original, unmodified data
* __FinalData:__ For modified data
* __Documentation:__ Any documentation for the data
* __Outputs:__ Any dataset specific outputs, such as figures or tables from analysis with the dataset.

## Survey and Census Data

* __Agriculture Survey:__
* __DHS:__ Demographic and Health Surveys (from USAID)
* __ERSS:__ Ethiopia Rural Socioeconomic Survey
* __LMMIS:__ Manufacturing Survey
* __LSMS:__ Living Standards Measurement Study
* __Price Data:__

## Data from Ethiopian Agencies

* __ERA - Proposed Expressway:__ Shapefile of planned expressways from ERA
* __ERA - Road Network:__ Shapefile and speeds of road network in 2004 and 2015
* __ETRE Traffic and Crashes:__ Traffic and crash data from the Ethiopian Toll Road Enterprise
* __RSDP Roads:__ Shapefile of road network annually from 1992 to 2016 with speeds
* __Traffic Counts:__
* __Weigh Bridges:__

## Geospatial Data - Raster or Satellite Based

* __DMSPOLS:__ Nighttime Lights, 1992 - 2013 at 1km resolution
* __Elevation:__
* __Globcover:__ Annual land cover data at 300 meter resolution from 1992 to 2018 from the European Space Agency (ESA) Globcover Dataset
* __Harmonized World Soil Database:__
* __NDVI:__ Normalized Difference Vegetation Index
* __Precipitation:__
* __Temperature:__
* __VIIRS:__ Nighttime Lights, 2012 - present at 750m resolution

## Geospatial Data - Vector or Points

* __CIESIN:__ Settlement data from the Center for International Earth Science Information Network (CIESIN)
* __City Population:__
* __GADM:__ Administrative boundaries
* __Gridded Population of the World:__
* __Hypothetical Road Networks:__ Minimum Spanning Tree
* __Town Population:__
* __Woreda Boundaries - 2013:__
* __Woreda Population:__

## Other
* __Panel Datasets for RSDP Impact Analysis:__ Panel datasets used for impact of RSDP analysis. (Datasets for multiple units of analysis - to keep everything together, include in this folder. Although might make sense to move back into above folders...)
