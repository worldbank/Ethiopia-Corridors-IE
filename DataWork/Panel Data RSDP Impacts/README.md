# Panel Data RSDP Impacts

This folder is for data cleaning and analysis for examining the impact of the RSDP on local economic activity and land cover change. The main units of analysis are a 1x1km grid (based of the DMSP-OLS grid) and woredas.

## Github Organization

* __01_extract_data_to_datasets:__ Creates blank datasets at grid and woreda level and extracts variables to these datasets, saving each variable type as a separate dataset

	* __01_create_initial_unitlevel_dataset:__ Creates blank datasets that variables from other datasets are merged into.
	* __02_extract_variables:__ Extracts variables to dataset. These variables relevant for both grid and woredas
	* __02_extract_variables_grid_specific:__ Extracts variables to dataset that are only relevant for grid level datasets
	* __02_extract_variables_woreda_specific:__ Extracts variables to dataset that are only relevant for woreda level datasets

* __02_prep_dataset_and_analysis:__ For each analysis type, merges datasets from above and preps a dataset for analysis, then runs analysis

	* __grid_ethiopia_longdifference:__ 1x1km grid level, using all grids in Ethiopia, using long difference 	* __grid_nearroad_longdifference:__ 1x1km grid level, using grids near a road, using long difference 	* __grid_nearroad_panel:__ 1x1km grid level, using grids near a road, using panel 	* __woreda_longdifference:__ Woreda level long difference 	* __woreda_panel:__ Woreda level panel 

## Dropbox Organization

The `[Dropbox]/Data/Panel Data RSDP Impacts/Data` folder has folders for each main unit of analysis and subset. It contains a folder for woredas, grid near roads and grid for all Ethiopia. Data and outputs for each dataset type are contained in these folder. Each of these folders contains the following subfolders:

* __individual_datasets:__ For variable specific datasets (e.g., dataset with distance to road, dataset with nighttime lights, dataset with globcover variables, etc).
* __merged_datasets:__ For datasets that have merged the above datasets together and for additional processed/cleaned datasets. These are cleaned datasets used for analysis 
* __results_datasets:__ In some cases, results are saved into a dataset, where the dataset is later loaded to then produce figures and tables. These "results datasets" go in this folder 
* __outputs:__ Contains a subfolder for figures, tables and a .tex file where figures and tables are compiled into a write-up.



