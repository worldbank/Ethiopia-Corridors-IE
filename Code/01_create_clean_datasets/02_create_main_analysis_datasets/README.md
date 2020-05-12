# Create Gridded Dataset

These files created a gridded dataset for analysis.

* __00_setup_python_environment.txt:__ Python is used to construct market access measures. This contains instructions for setting up the python environment in anaconda.
* __01_create_[dataset_type].R:__ Creates a blank gridded dataset at different units of analysis.
* __02_extract_[variable].R:__ Extracts variables to the gridded dataset, creating separate files for each variable.
* __03_[name].R:__ These files create market access measures for the gridded dataset, creating a separate file with just market access.
* __04_merge_datasets_dmspols.R__ This merges the individual datasets together.
* __05_create_variables_for_analysis.R__ This script cleans variables and creates variables needed for analysis.
