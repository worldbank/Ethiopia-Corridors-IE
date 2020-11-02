# Analysis: Coefficient Each Year - Results

# Exports dataframe of results, to be used to make figures

#### Parameters
OVERWRITE_FILES <- T

#### Default
dep_var <- "globcover_urban"
indep_var <- "years_since_improvedroad"
controls <- ""
addis_distance <- "All"
ntl_group <- "All"

# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(panel_rsdp_imp_data_file_path, "dmspols_grid_nearroad", "merged_datasets", "grid_data_clean.Rds"))

## Nighttime lights groups
ntl_non0_med <- data$dmspols_1996_woreda[data$dmspols_1996_woreda > 0] %>% median(na.rm=T)
data$ntl_group <- NA
data$ntl_group[data$dmspols_1996_woreda <= ntl_non0_med] <- "1"
data$ntl_group[data$dmspols_1996_woreda > ntl_non0_med] <- "2"

## Check Median NTL Value
data$dmspols_zhang[data$dmspols_zhang > 0 & data$year %in% 1996] %>% median(na.rm=T)

## NTL lit at baseline
data$dmspols_zhang_base0na <- data$dmspols_zhang
data$dmspols_zhang_base0na[data$dmspols_zhang_1996 %in% 0] <- NA

data$dmspols_zhang_ihs_base0na <- data$dmspols_zhang_ihs
data$dmspols_zhang_ihs_base0na[data$dmspols_zhang_1996 %in% 0] <- NA

# Estimate Model ---------------------------------------------------------------
results_df <- data.frame(NULL)

# "ndvi","ndvi_cropland", "globcover_urban", "globcover_cropland", "dmspols_zhang", "dmspols_zhang_ihs",  "dmspols_zhang_2", "dmspols_zhang_6", "dmspols", "dmspols_ihs"
for(dep_var in c("dmspols_zhang_base0na","dmspols_zhang_ihs_base0na")){
  for(indep_var in c("years_since_improvedroad", "years_since_improvedroad_50aboveafter", "years_since_improvedroad_below50after")){
    
    for(controls in c("", "+temp_avg+precipitation")){
    #for(controls in c("")){
      
      #for(addis_distance in c("All", "Far")){
      for(addis_distance in c("All", "Far")){
        
        #for(ntl_group in c("All", "1", "2")){
        for(ntl_group in c("All", "1", "2")){
          
          ## Check if exists          
          filename <- paste0(dep_var, "-", indep_var, "-", controls, "-", addis_distance, "-", ntl_group, ".Rds")
          file <- file.path(panel_rsdp_imp_data_file_path, 
                            "dmspols_grid_nearroad", "results_datasets", 
                            "individual_datasets",
                            filename)
          
          ## Update
          print(filename)
          
          if(!file.exists(file) | OVERWRITE_FILES){
            
            ### Prep Data
            data_temp <- data
            if(addis_distance %in% "Far") data_temp <- data_temp[data_temp$far_addis %in% 1,]
            if(ntl_group %in% "1")        data_temp <- data_temp[data_temp$ntl_group %in% "1",]
            if(ntl_group %in% "2")        data_temp <- data_temp[data_temp$ntl_group %in% "2",]
            if(ntl_group %in% "3")        data_temp <- data_temp[data_temp$ntl_group %in% "3",]
            
            ### Run model
            results_df_temp <- tryCatch({     
              
              paste(dep_var, "~", indep_var, controls, "| year + cell_id | 0 | woreda_id") %>%
                as.formula() %>%
                felm(data = data_temp) %>%
                lm_confint_tidy(indep_var)%>%
                mutate(addis_distance = addis_distance,
                       dep_var = dep_var,
                       ntl_group = ntl_group,
                       controls = controls,
                       indep_var = indep_var)
              
            }, error=function(e) data.frame(NULL))
            
            results_df <- bind_rows(results_df,
                                    results_df_temp)
            
            saveRDS(results_df, file)
          }
          
          
        }
      }
    }
  }
}




