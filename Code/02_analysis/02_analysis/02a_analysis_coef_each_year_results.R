# Analysis: Coefficient Each Year - Results

# Exports dataframe of results, to be used to make figures

# Overal Results ---------------------------------------------------------------
region_type <- "All"
addis_distance <- "All"
dv <- "dmspols_zhang_ihs"
ntl_group <- "All"
road_years_group <- "all"


for(road_years_group in c("all", 
                          "dmspols",
                          "viirs",
                          "phase1",
                          "phase2",
                          "phase3",
                          "phase4")){
  
  # Where are we?
  print(paste(road_years_group, "============================================"))
  
  data <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "merged_datasets", paste0("grid_data_clean_",road_years_group,".Rds")))
  
  
  results_df <- data.frame(NULL)
  
  for(region_type in c("All", "Dense", "Sparse")){ # "All", "Dense", "Sparse"
    for(addis_distance in c("All", "Far")){ # "All", "Far"
      for(dv in c("viirs_mean_ihs", "viirs_mean_2", "viirs_mean_6", "globcover_urban","globcover_cropland", "dmspols_ihs", "dmspols_zhang_ihs", "dmspols_zhang_2", "dmspols_zhang_6", "ndvi", "ndvi_cropland")){
        for(ntl_group in c("All", "1", "2", "3")){
          
          #### Where are we?
          print(paste(region_type, addis_distance, phase, dv, ntl_group))
          
          #### Temp data and subset
          data_temp <- data
          
          ## Subset by region type
          if(region_type %in% c("Dense", "Sparse")) data_temp <- data_temp[data_temp$region_type %in% region_type,]
          
          ## Subset by baseline nighttime lights
          if(ntl_group %in% c("1", "2", "3")) data_temp <- data_temp[data_temp$dmspols_zhang_1996_group_woreda %in% as.numeric(ntl_group),]
          
          ## Subset by All or Far from Addis
          if(addis_distance %in% "Far") data_temp <- data_temp[data_temp$far_addis %in% 1,]
          
          ## Add dependent variable as temp variable
          data_temp$dv <- data_temp[[dv]]
          
          #### Estimate Models // GADM_ID_3
          results_df_temp <- tryCatch({     
            bind_rows(
              felm(dv ~ years_since_improvedroad + temp_avg + precipitation | year + cell_id | 0 | woreda_hdx_w_uid, data=data_temp) %>%
                lm_confint_tidy("years_since_improvedroad") %>% mutate(var = "All"),
              
              felm(dv ~ years_since_improvedroad_50aboveafter + temp_avg + precipitation | year + cell_id | 0 | woreda_hdx_w_uid, data=data_temp) %>%
                lm_confint_tidy("years_since_improvedroad_50aboveafter") %>% mutate(var = "50 Above"),
              
              felm(dv ~ years_since_improvedroad_below50after + temp_avg + precipitation | year + cell_id | 0 | woreda_hdx_w_uid, data=data_temp) %>%
                lm_confint_tidy("years_since_improvedroad_below50after") %>% mutate(var = "Below 50")
            ) %>% mutate(region = region_type,
                         addis_distance = addis_distance,
                         dv = dv,
                         ntl_group = ntl_group,
                         road_years_group = road_years_group)
          }, error=function(e) data.frame(NULL))
          
          results_df <- bind_rows(results_df, results_df_temp)
          print(nrow(results_df))
          
          #gc()
          #rm(data_temp_improvedroad); gc()
          #rm(data_temp_improvedroad_50aboveafter); gc()
          #rm(data_temp_improvedroad_below50after); gc()
          #gc()
          
        }
      }
    }
  }
  
  
  # Export results within year group -------------------------------------------
  saveRDS(results_df, file.path(finaldata_file_path, DATASET_TYPE, "results", paste0("results_coef_each_year_yeargroup",road_years_group,".Rds")))
  
}





