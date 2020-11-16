# Clean Data

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
data <- readRDS(file.path(panel_rsdp_imp_data_file_path, "dmspols_grid_ethiopia",
                          "merged_datasets", "panel_data.Rds"))

#data <- data[data$cell_id %in% unique(data$cell_id)[1:5000],] ## for testing

# Create Varibles --------------------------------------------------------------
calc_ihs <- function(x) log(x + sqrt(x^2 + 1))

data <- data %>%
  mutate(dmspols_ihs = calc_ihs(dmspols),
         dmspols_zhang_ihs = calc_ihs(dmspols_zhang))

# First Difference -------------------------------------------------------------
for(i in 1:nrow(base_end_df)){
  print(i)
  
  #### Grab start/end years
  base_year <- base_end_df$baseline[i]
  end_year <- base_end_df$endline[i]
  
  #### First Difference Dataset
  
  ## First difference dataset of time varying variables 
  data_first_diff <- data %>%
    arrange(year) %>%
    filter(year %in% c(base_year, end_year)) %>%
    
    # First difference
    group_by(cell_id) %>%
    summarize_at(names(data) %>% str_subset("dmspols|globcover|ndvi"), 
                 diff)
  
  ## Grab time invariant variables
  data_time_invar <- data %>%
    
    # Baseline values
    group_by(cell_id) %>%
    mutate(dmspols_zhang_1996 = dmspols_zhang[year == 1996]) %>%
    ungroup() %>%
    
    filter(year %in% base_year) %>%
    dplyr::select(c(cell_id, distance_mst, 
                    distance_anyimproved_ever, 
                    distance_anyimproved_by2012,
                    distance_anyroad2012,
                    distance_anyroad2016, 
                    distance_city_addisababa,
                    dmspols_zhang_1996,
                    W_CODE,
                    Z_CODE)) 
  
  ## Merge
  data_clean <- merge(data_first_diff, data_time_invar, by = "cell_id")
  
  ## Define "Near Road" Variables
  data_clean$near_anyimproved_ever_5km <- as.numeric(data_clean$distance_anyimproved_ever <= 5*1000)
  data_clean$near_anyimproved_by2012_5km   <- as.numeric(data_clean$distance_anyimproved_ever <= 5*1000)
  
  data_clean$near_anyroad2012_5km      <- as.numeric(data_clean$distance_anyroad2012 <= 5*1000)
  data_clean$near_anyroad2016_5km      <- as.numeric(data_clean$distance_anyroad2016 <= 5*1000)
  
  data_clean$near_mst_5km              <- as.numeric(data_clean$distance_mst <= 5*1000)
  
  ## NTL in Lit Cells
  data_clean$dmspols_zhang_ihs_base0na <- data_clean$dmspols_zhang_ihs
  data_clean$dmspols_zhang_ihs_base0na[data_clean$dmspols_zhang_1996 %in% 0] <- NA
  
  #### Export
  file_name <- paste0("longdiff_data_clean_base",base_year,"_end",end_year)
  
  saveRDS(data_clean, file.path(panel_rsdp_imp_data_file_path, "dmspols_grid_ethiopia", 
                                "merged_datasets", paste0(file_name, ".Rds")))
  write_dta(data_clean, file.path(panel_rsdp_imp_data_file_path, "dmspols_grid_ethiopia", 
                                  "merged_datasets", paste0(file_name, ".dta")))
}







