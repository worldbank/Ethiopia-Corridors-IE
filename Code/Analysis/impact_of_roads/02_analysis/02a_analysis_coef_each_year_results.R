# Analysis: Coefficient Each Year - Results

# Exports dataframe of results, to be used to make figures

# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "merged_datasets", "grid_data_clean.Rds"))

#data$globcover_cropland %>% is.na %>% table()

#data <- data[!is.na(data$globcover_cropland),]
#data <- data %>%
#  group_by(cell_id) %>%
#  mutate(globcover_cropland_1996 = globcover_cropland[year == 1996]) %>%
#  mutate(globcover_cropland_2016 = globcover_cropland[year == 2016]) %>%
#  ungroup()

#felm(ndvi ~ years_since_improvedroad_below45after | year + cell_id | 0 | woreda_hdx_w_uid, data=data[(data$globcover_cropland_1996 >= .5) & (data$globcover_cropland_2016 >= .5),]) %>% summary()

# Functions --------------------------------------------------------------------
lm_confint_tidy <- function(lm, years_since_variable){
  lm_confint <- confint(lm) %>% 
    as.data.frame
  names(lm_confint) <- c("p025", "p975")
  lm_confint$b <- (lm_confint$p025 + lm_confint$p975)/2
  lm_confint$variable <- row.names(lm_confint)
  
  lm_confint <- lm_confint[!grepl("cluster_id)|year)|Intercept)", lm_confint$variable),]
  lm_confint$years_since_improved <- gsub(years_since_variable, "", lm_confint$variable) %>% as.numeric
  
  return(lm_confint)
}

# Overal Results ---------------------------------------------------------------
if(F){
  region_type <- "All"
  addis_distance <- "All"
  phase <- "phase_all"
  dv <- "globcover_urban"
  ntl_group <- "All"
}


results_df <- data.frame(NULL)

for(region_type in c("All", "Dense", "Sparse")){
  for(addis_distance in c("All", "Far")){
    for(phase in c("phase_all", "phase_1", "phase_2", "phase_3", "phase_4")){
      for(dv in c("globcover_urban","globcover_cropland", "ndvi", "dmspols_ihs", "dmspols_zhang_ihs", "dmspols_zhang_2", "dmspols_zhang_6")){
        for(ntl_group in c("All", "1", "2", "3")){
          
          # Printing so know where we be!
          print(paste(region_type, addis_distance, phase, dv, ntl_group))
          
          data_temp <- data
          
          #### Subset by region type
          if(region_type %in% c("Dense", "Sparse")) data_temp <- data_temp[data_temp$region_type %in% region_type,]
          
          #### Subset by baseline nighttime lights
          if(ntl_group %in% c("1", "2", "3")) data_temp <- data_temp[data_temp$dmspols_zhang_1996_group_woreda %in% as.numeric(ntl_group),]
          
          #### Subset by All or Far from Addis
          if(addis_distance %in% "Far") data_temp <- data_temp[data_temp$distance_city_addisababa >= 100*1000,]
          
          #### Add dependent variable as temp variable
          data_temp$dv <- data_temp[[dv]]
          
          #### Subsetting by Phase
          if(phase %in% "phase_all") phase_years <- 1997:2016
          if(phase %in% "phase_1")   phase_years <- 1997:2002
          if(phase %in% "phase_2")   phase_years <- 2003:2007
          if(phase %in% "phase_3")   phase_years <- 2008:2010
          if(phase %in% "phase_4")   phase_years <- 2011:2016
    
          data_temp_improvedroad              <- data_temp[data_temp$year_improvedroad              %in% phase_years,]
          data_temp_improvedroad_50aboveafter <- data_temp[data_temp$year_improvedroad_50aboveafter %in% phase_years,]
          data_temp_improvedroad_below50after <- data_temp[data_temp$year_improvedroad_below50after %in% phase_years,]
          
          #### Estimate Models // GADM_ID_3
          results_df_temp <- tryCatch({     
            bind_rows(
              felm(dv ~ years_since_improvedroad | year + cell_id | 0 | woreda_hdx_w_uid, data=data_temp_improvedroad) %>%
                lm_confint_tidy("years_since_improvedroad") %>% mutate(var = "All"),
              
              felm(dv ~ years_since_improvedroad_50aboveafter | year + cell_id | 0 | woreda_hdx_w_uid, data=data_temp_improvedroad_50aboveafter) %>%
                lm_confint_tidy("years_since_improvedroad_50aboveafter") %>% mutate(var = "50 Above"),
              
              felm(dv ~ years_since_improvedroad_below50after | year + cell_id | 0 | woreda_hdx_w_uid, data=data_temp_improvedroad_below50after) %>%
                lm_confint_tidy("years_since_improvedroad_below50after") %>% mutate(var = "Below 50")
            ) %>% mutate(region = region_type,
                         addis_distance = addis_distance,
                         phase = phase,
                         dv = dv,
                         ntl_group = ntl_group)
          }, error=function(e) data.frame(NULL))
          
          results_df <- bind_rows(results_df, results_df_temp)
          print(nrow(results_df))
          
        }
      }
    }
  }
}

# Export Results ---------------------------------------------------------------
saveRDS(results_df, file.path(finaldata_file_path, DATASET_TYPE, "results", "results_coef_each_year.Rds"))

