# Subset Main Dataset to Time Periods

for(road_years_group in c("dmspols",
                          "viirs",
                          "phase1",
                          "phase2",
                          "phase3",
                          "phase4")){
  
  print(paste(road_years_group, "--------------------------------------------"))
  
  data <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "merged_datasets", "grid_data_clean_all.Rds"))
  
  #### Prep Road variables
  # Restrict to roads improved in time period
  # 1. Start with list of all year
  # 2. Vector of years to remove
  # 3. str_replace_all to remove vector of years to remove
  # 4. Remove semicolons that separate years
  # 5. Grab first four characters; this is the first year and year we use
  road_years_i <- road_year[[road_years_group]]
  all_years <- 1996:2016
  road_years_remove <- all_years[!(all_years %in% road_years_i)] %>%
    paste(collapse="|")
  
  ## Years Since Improved & Post Improved
  data$improvedroad_year <- data$near_improvedroad_all_years %>%
    str_replace_all(road_years_remove, "") %>%
    str_replace_all(";", "") %>%
    substring(1,4) %>%
    as.numeric()
  data$near_improvedroad_all_years <- NULL
  data$years_since_improvedroad <- data$year - data$improvedroad_year
  data$post_improvedroad <- data$years_since_improvedroad %in% 0:100 # use 0:100, not >0, so NA-->0
  
  # Restrict to observations where road improved
  data <- data[!is.na(data$years_since_improvedroad),]
  
  data$improvedroad_below50after_year <- data$near_improvedroad_below50after_all_years %>%
    str_replace_all(road_years_remove, "") %>%
    str_replace_all(";", "") %>%
    substring(1,4) %>%
    as.numeric()
  data$near_improvedroad_below50after_all_years <- NULL
  data$years_since_improvedroad_below50after <- data$year - data$improvedroad_below50after_year
  data$post_improvedroad_below50after <- data$years_since_improvedroad_below50after %in% 0:100
  
  data$improvedroad_50aboveafter_year <- data$near_improvedroad_50aboveafter_all_years %>%
    str_replace_all(road_years_remove, "") %>%
    str_replace_all(";", "") %>%
    substring(1,4) %>%
    as.numeric()
  data$near_improvedroad_50aboveafter_all_years <- NULL
  data$years_since_improvedroad_50aboveafter <- data$year - data$improvedroad_50aboveafter_year
  data$post_improvedroad_50aboveafter <- data$years_since_improvedroad_50aboveafter %in% 0:100
  
  #### Lagged treatment
  data$pre_improvedroad_neg2_5 <- as.numeric(data$years_since_improvedroad %in% -2:-5) %>% as.numeric()
  data$pre_improvedroad_neg6_10 <- as.numeric(data$years_since_improvedroad %in% -6:-10) %>% as.numeric()
  
  data$pre_improvedroad_50aboveafter_neg2_5 <- as.numeric(data$years_since_improvedroad_50aboveafter %in% -2:-5) %>% as.numeric()
  data$pre_improvedroad_50aboveafter_neg6_10 <- as.numeric(data$years_since_improvedroad_50aboveafter %in% -6:-10) %>% as.numeric()
  
  data$pre_improvedroad_below50after_neg2_5 <- as.numeric(data$years_since_improvedroad_below50after %in% -2:-5) %>% as.numeric()
  data$pre_improvedroad_below50after_neg6_10 <- as.numeric(data$years_since_improvedroad_below50after %in% -6:-10) %>% as.numeric()
  
  #### Restrict Years of Analysis
  if(road_years_group %in% "viirs"){
    data <- data[data$year %in% 2012:2019,]
  }
  
  if(road_years_group %in% "dmspols"){
    data <- data[data$year %in% 1992:2012,]
  }
  
  #### Export
  saveRDS(data, file.path(finaldata_file_path, DATASET_TYPE, "merged_datasets", paste0("grid_data_clean_",road_years_group,".Rds")))
}
