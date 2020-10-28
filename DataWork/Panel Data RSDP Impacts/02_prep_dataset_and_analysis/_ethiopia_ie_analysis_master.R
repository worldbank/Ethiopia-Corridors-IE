# Ethiopia IE

# Analysis Master
# NOTE: Run _ethiopia_ie_master.R beforehand

# Analysis is run at four different levels
# 1. 1x1km grid level, subset to grids near roads, panel dataset
# 2. 1x1km grid level, using all grids in Ethiopia, long difference dataset
# 3. Woreda level, panel dataset
# 4. Woreda level, long difference dataset

# Code is organize within each level (e.g., code for grid panel near roads, etc.)
# For each level, the (1) first set of code preps analysis datasets and the (2)
# second set of code runs the analysis, producing any tables and/or figures

# GRID LEVEL ANALYSIS ==========================================================

# Grid Level, Near Roads Subset, Panel -----------------------------------------
codepath_grid_nearroad_panel <- file.path(rsdp_impact_analysis_code_file_path, "grid_nearroad_panel")

# Merges individual datasets of extracted variables
source(file.path(codepath_grid_nearroad_panel, "01_merge_data.R"))

# Creates variables needed for analysis
source(file.path(codepath_grid_nearroad_panel, "02_clean_data.R"))

# Diff-in-diff/event study models
#  (a) Runs model for each depending variable and a number of subsets. Each model
#      result is saved in a separate file, as this takes a while.
#  (b) Appends the above results
#  (c) Makes figures of results
source(file.path(codepath_grid_nearroad_panel, "03a_analysis_coef_each_year_results.R"))
source(file.path(codepath_grid_nearroad_panel, "03b_analysis_coef_each_year_results_append.R"))
source(file.path(codepath_grid_nearroad_panel, "03c_analysis_coef_each_year_figures.R"))


# Grid Level, All Ethiopia Cells, Long Difference ------------------------------
codepath_grid_ethiopia_longdifference <- file.path(rsdp_impact_analysis_code_file_path, "grid_ethiopia_longdifference")

# Merges individual datasets of extracted variables
source(file.path(codepath_grid_ethiopia_longdifference, "01_merge_data.R"))

# Creates variables needed for analysis and creates first difference datasets
# Different datasets are created for using different baseline and endline years.
# Needed as DMSP-OLS ends in 2013, globcover ends in 2018 and roads data ends
# in 2016
source(file.path(codepath_grid_ethiopia_longdifference, "02_clean_data.R"))


# WOREDA LEVEL ANALYSIS ========================================================

# Woreda Level, Panel ----------------------------------------------------------

codepath_woreda_panel <- file.path(rsdp_impact_analysis_code_file_path, "woreda_panel")

# Merges individual datasets of extracted variables
source(file.path(codepath_woreda_panel, "01_merge_data.R"))

# Creates variables needed for analysis
source(file.path(codepath_woreda_panel, "02_clean_data.R"))


# Woreda Level, Long Difference -----------------------------------------------------

# Creates variables needed for analysis and creates first difference datasets
# Different datasets are created for using different baseline and endline years.
# Needed as DMSP-OLS ends in 2013, globcover ends in 2018 and roads data ends
# in 2016
# ** This dataset relies on the clean panel dataset, consequently, the
# 01_merge_data.R and 02_clean_data.R should be run before running this script
source(file.path(codepath_woreda_panel, "01_clean_data.R"))






