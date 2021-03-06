# Number of Projects Near

control_vars <- "+ temp_avg + precipitation"

# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "merged_datasets", "grid_data_clean_all.Rds"))

data_diff <- data %>%
  arrange(year) %>%
  mutate(year = year %>% as.character()) %>%
  group_by(uid) %>%
  mutate_if(is.numeric,  function(x) c(NA, diff(x))) %>%
  mutate(road_length_50over_lag1 = lag(road_length_50over),
         road_length_50over_lag2 = lag(road_length_50over,2),
         road_length_50over_lag3 = lag(road_length_50over,3))

lm(dmspols ~ log(road_length_50over+1) + 
     log(road_length_50over_lag1+1) + 
     log(road_length_50over_lag2+1) + 
     log(road_length_50over_lag3+1), 
   data = data_diff) %>%
  summary()

 head(data_diff)




data_dmspols <- data %>%
  filter(year %in% c(1996, 2012)) %>%
  mutate(endline = as.numeric(year %in% 2012)) 

data_viirs <- data %>%
  filter(year %in% c(2013, 2016)) %>%
  mutate(endline = as.numeric(year %in% 2016)) 

data_full <- data %>%
  filter(year %in% c(1996, 2016)) %>%
  mutate(endline = as.numeric(year %in% 2016)) 


## EXPLORE
if(F){
  data_temp <- data_full %>%
    arrange(endline) %>%
    mutate(year = year %>% as.character()) %>%
    group_by(uid) %>%
    mutate_if(is.numeric, diff) %>%
    filter(endline %in% 1) # we have repeated values from diff, as not doing c(diff(), NA)
  
  lm(globcover_urban ~ road_length_50over, data=data_temp) %>% summary()
  lm(globcover_urban ~ road_length_50over_area, data=data_temp) %>% summary()
  
}


results_df <- data.frame(NULL)

dv <- "viirs_mean_ihs"
iv <- "road_length_30over"
iv_suffix <- "_neigh_withi_area"
addis_dist <- "All"

counter <- 1
for(dv in c("viirs_mean_2",
            "viirs_mean_6",
            "viirs_mean_ihs",
            "dmspols_zhang_ihs",
            "dmspols_zhang_2",
            "dmspols_zhang_6",
            "globcover_urban", 
            "globcover_cropland",
            "ndvi", 
            "ndvi_cropland")){
  for(iv in c("road_length_10over", 
              "road_length_15over", 
              "road_length_20over", 
              "road_length_25over", 
              "road_length_30over", 
              "road_length_35over", 
              "road_length_45over", 
              "road_length_50over", 
              "road_length_70over", 
              "road_length_120over", 
              "road_length_X_speed")){
    for(iv_suffix in c("", "_area", "_neigh", "_neigh_area", "_neigh_withi", "_neigh_withi_area")){
      for(addis_dist in c("All", "Far")){
        
        ## Grab dataset
        if (grepl("dmsp", dv)){
          data_temp <- data_dmspols
        } else if (grepl("viirs", dv)){
          data_temp <- data_viirs
        } else {
          data_temp <- data_full
        }
        
        ## Subset to Far from Addis
        if(addis_dist %in% "Far"){
          data_temp <- data_temp[data_temp$far_addis %in% 1,] 
        }
        
        ## _area doesn't exist so add
        #if(iv_suffix %in% "_area"){
        #  data_temp[[paste0(iv, "_area")]] <- data_temp[[iv]] / data_temp$Area
        #}
        
        #print(paste0(iv, iv_suffix))
        data_temp$road_var <- log(data_temp[[paste0(iv, iv_suffix)]]+1)
        data_temp$dv       <- data_temp[[dv]]
        
        ## First Difference
        data_temp <- data_temp %>%
          arrange(endline) %>%
          group_by(uid) %>%
          mutate(road_var = diff(road_var),
                 dv = diff(dv),
                 precipitation = diff(precipitation),
                 temp_avg = diff(temp_avg)) %>%
          filter(endline %in% 1) # we have repeated values from diff, as not doing c(diff(), NA)
        
        IVs <- "road_var"
        IVs_base <- "road_var*dmspols_1996_group_woreda - dmspols_1996_group_woreda"
        
        #### Level
        f <- as.formula(paste(dv, " ~ ", IVs, control_vars, "| 0 | 0 | uid"))
        lm <- felm(f, data=data_temp) %>%
          lm_post_confint_tidy %>%
          filter(variable != "temp_avg",
                 variable != "precipitation",
                 variable != "(Intercept)") %>%
          mutate(dv = dv,
                 iv = iv,
                 addis_dist = addis_dist,
                 iv_suffix = iv_suffix,
                 lm_type = "level")
        
        #### Base
        f_base <- as.formula(paste(dv, " ~ ", IVs_base, control_vars, "| 0 | 0 | uid"))
        lm_base <- felm(f_base, data=data_temp) %>%
          lm_post_confint_tidy %>%
          filter(variable != "temp_avg",
                 variable != "precipitation",
                 variable != "(Intercept)") %>%
          mutate(dv = dv,
                 iv = iv,
                 addis_dist = addis_dist,
                 iv_suffix = iv_suffix,
                 lm_type = "ntl_base")
        
        results_df <- bind_rows(results_df, lm)
        results_df <- bind_rows(results_df, lm_base)
        
        if((counter %% 10) == 0) print(counter)
        counter <- counter + 1
        
        
      }
    }
  }
}

saveRDS(results_df, file.path(finaldata_file_path, DATASET_TYPE, "results", "long_diff_first_diff.Rds"))
