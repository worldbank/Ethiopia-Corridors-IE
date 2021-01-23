# Prep data for long difference

# NOTE: Uses clean panel data

#### Parameters
# 0 meters, meaning woreda must intersect
NEAR_THRESHOLD <- 0 

# Dataframe to define baseline/endline combinations. For each combiations,
# makes a different dataframe. 
# - DMSP-OLS until 2012
# - Globcover until 2018
# - Roads until 2016 (for globcover, could use roads in 2016 and globcover in 2018)
base_end_df <- data.frame(baseline = c(1996, 1996),
                          endline =  c(2012, 2016))

# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(panel_rsdp_imp_data_file_path, "woreda", "merged_datasets", "panel_data_clean.Rds"))

## Make sure have woreda in both years. Check why this always wouldn't be case?
data <- data %>%
  group_by(cell_id) %>%
  mutate(N_woreda = n()) %>%
  filter(N_woreda == max(N_woreda))

# Clean Data -------------------------------------------------------------------
for(i in 1:nrow(base_end_df)){

  #### Grab start/end years
  base_year <- base_end_df$baseline[i]
  end_year <- base_end_df$endline[i]
  
  #### First Difference Dataset
  
  ## First difference dataset of time varying variables 
  data_first_diff <- data %>%
    arrange(year) %>%
    filter(year %in% c(base_year, end_year)) %>%
    
    # Assume NA is 0 (for first difference... eg, road length)
    mutate_if(is.numeric, replace_na, replace = 0) %>%
    
    # First difference
    group_by(cell_id) %>%
    summarize_at(names(data) %>% str_subset("MA|road_length|dmspols|globcover|viirs|temp|precipitation|ndvi|distance_road_"), 
                 diff) %>%
    
    # Remove select variables
    dplyr::select(-c(dmspols_1996))
  
  ## Grab time invariant variables
  data_time_invar <- data %>%
    filter(year %in% base_year) %>%
    dplyr::select(c(cell_id, Z_CODE, distance_mst, dmspols_1996, area_polygon, distance_city_addisababa)) 
  
  ## Merge
  data_clean <- merge(data_first_diff, data_time_invar, by = "cell_id")
  
  #### Export
  file_name <- paste0("longdiff_data_clean_base",base_year,"_end",end_year)
  
  saveRDS(data_clean, file.path(panel_rsdp_imp_data_file_path, "woreda", "merged_datasets", 
                          paste0(file_name, ".Rds")))
  write_dta(data_clean, file.path(panel_rsdp_imp_data_file_path, "woreda", "merged_datasets", 
                            paste0(file_name, ".dta")))
}

