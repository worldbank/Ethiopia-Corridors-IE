# Impact of RSDP

# Functions ====================================================================
felm_extractcoefs <- function(formula, data){
  model <- tryCatch(felm(formula, data=data), error = function(e) return("error"))
  
  #if(model == "error"){
  #  df_out <- as.data.frame(NULL)
  #} else{
    df_out <- summary(model)$coefficients %>% as.data.frame
    df_out$variable <- row.names(df_out)
    df_out$dep_var <- substring(model$model[2],23,70) %>% gsub(pattern="~.*| ",replacement="") 
  #}
  
  return(df_out)
  
}

# Load Data --------------------------------------------------------------------
data_all <- readRDS(file.path(project_file_path, "Data", "FinalData", "dmspols_grid_dataset", "dmspols_level_dataset_5percentsample_analysisvars.Rds"))

data_all <- data_all %>%
  group_by(cell_id) %>%
  mutate(ndvi_cropland_2000 = ndvi_cropland[year %in% 2000]) %>%
  mutate(ndvi_cropland_2001 = ndvi_cropland[year %in% 2001]) %>%
  mutate(ndvi_cropland_2002 = ndvi_cropland[year %in% 2002]) %>%
  mutate(ndvi_cropland_2003 = ndvi_cropland[year %in% 2003]) %>%
  mutate(ndvi_cropland_2004 = ndvi_cropland[year %in% 2004]) %>%
  mutate(ndvi_cropland_2005 = ndvi_cropland[year %in% 2005]) %>%
  mutate(ndvi_cropland_2006 = ndvi_cropland[year %in% 2006]) %>%
  mutate(ndvi_cropland_2007 = ndvi_cropland[year %in% 2007]) %>%
  mutate(ndvi_cropland_2008 = ndvi_cropland[year %in% 2008]) %>%
  mutate(ndvi_cropland_2009 = ndvi_cropland[year %in% 2009]) %>%
  mutate(ndvi_cropland_2010 = ndvi_cropland[year %in% 2010]) %>%
  mutate(ndvi_cropland_2011 = ndvi_cropland[year %in% 2011]) %>%
  mutate(ndvi_cropland_2012 = ndvi_cropland[year %in% 2012]) %>%
  mutate(ndvi_cropland_2013 = ndvi_cropland[year %in% 2013]) %>%
  mutate(ndvi_cropland_2014 = ndvi_cropland[year %in% 2014]) %>%
  mutate(ndvi_cropland_2015 = ndvi_cropland[year %in% 2015]) %>%
  ungroup()

data_all$crop_all_years <- (data_all$ndvi_cropland_2000 > 0) & 
  (data_all$ndvi_cropland_2001 > 0) & 
  (data_all$ndvi_cropland_2002 > 0) &
  (data_all$ndvi_cropland_2003 > 0) & 
  (data_all$ndvi_cropland_2004 > 0) & 
  (data_all$ndvi_cropland_2005 > 0) & 
  (data_all$ndvi_cropland_2006 > 0) & 
  (data_all$ndvi_cropland_2007 > 0) & 
  (data_all$ndvi_cropland_2008 > 0) & 
  (data_all$ndvi_cropland_2009 > 0) & 
  (data_all$ndvi_cropland_2010 > 0) & 
  (data_all$ndvi_cropland_2011 > 0) & 
  (data_all$ndvi_cropland_2012 > 0) & 
  (data_all$ndvi_cropland_2013 > 0) & 
  (data_all$ndvi_cropland_2014 > 0) & 
  (data_all$ndvi_cropland_2015 > 0) 

# Subset by Road Type ----------------------------------------------------------
road_variable <- "below50"
phase_cat <- "phase_12"
phase_cat <- "phase_34"

results_all <- data.frame(NULL)

for(dmspols_base_group in c(1, 2, 3, 123)){
  for(phase_cat in c("phase_all", "phase_12", "phase_34")){
    for(road_variable in c("below50", "50above", "all")){
      print(phase_cat)
      print(road_variable)
      print(dmspols_base_group)
      print("")
      
      data <- data_all
      
      if(dmspols_base_group %in% 1) data <- data[data$dmspols_1997_group %in% 1,]
      if(dmspols_base_group %in% 2) data <- data[data$dmspols_1997_group %in% 2,]
      if(dmspols_base_group %in% 3) data <- data[data$dmspols_1997_group %in% 3,]
      
      if(road_variable == "all") road_name <- "Imp. Rd, " 
      if(road_variable == "below50") road_name <- "Imp. Rd $<$ 50km/hr, " 
      if(road_variable == "50above") road_name <- "Imp. Rd $\\geq$ 50km/hr, " 
      
      #### Restrict to Phase/Road
      if(phase_cat == "phase_all"){
        year_start <- 1996
        year_end <- 2016
      }
      
      if(phase_cat == "phase_12"){
        year_start <- 1996
        year_end <- 2007
      }
      
      if(phase_cat == "phase_34"){
        year_start <- 2008
        year_end <- 2016
      }
      
      # Near improved now gives binary variable WHEN an improved road is improved (eg, 0 0 1 0 0), change so becomes
      # 0 0 1 1 1.
      data[[paste0("near_improved_", road_variable)]] <- as.numeric(as.character(data[[paste0("years_since_improved_", road_variable)]])) >= 0
      
      cells_near_improved_road_beginyear <- data$cell_id[(data$year == year_start) & (data[[paste0("near_improved_", road_variable)]] == 0)]
      cells_near_improved_road_endyear <- data$cell_id[(data$year == year_end) & (data[[paste0("near_improved_", road_variable)]] == 1)]
      cells_near_improved_road <- intersect(cells_near_improved_road_beginyear, cells_near_improved_road_endyear)
      
      #### Restrict to cells
      data <- data[data$cell_id %in% cells_near_improved_road,]
      
      data$years_since_improved <- data[[paste0("years_since_improved_", road_variable)]]
      data$years_since_improved <- data$years_since_improved %>% as.character %>% as.numeric %>% as.factor %>% relevel(ref="0")
      
      ####
      #data$years_since_improved[!is.na(data$dmspols_zhang)] %>% table
      #a <- data[data$cell_id == 7026,]
      
      felm_allroads_yearssince_dmspols <- felm_extractcoefs(dmspols_zhang ~ years_since_improved | cell_id + year | 0 | GADM_ID_3, data=data)
      felm_allroads_yearssince_dmspols1 <- felm_extractcoefs(dmspols_zhang_1 ~ years_since_improved | cell_id + year | 0 | GADM_ID_3, data=data)
      felm_allroads_yearssince_dmspols5 <- felm_extractcoefs(dmspols_zhang_5 ~ years_since_improved | cell_id + year | 0 | GADM_ID_3, data=data)
      felm_allroads_yearssince_urban <- felm_extractcoefs(globcover_urban ~ years_since_improved | cell_id + year | 0 | GADM_ID_3, data=data)
      felm_allroads_yearssince_cropland <- felm_extractcoefs(globcover_cropland ~ years_since_improved | cell_id + year | 0 | GADM_ID_3, data=data)
      felm_allroads_yearssince_ndvi <- felm_extractcoefs(ndvi ~ years_since_improved | cell_id + year | 0 | GADM_ID_3, data=data)
      felm_allroads_yearssince_ndvi_cropland <- felm_extractcoefs(ndvi_cropland ~ years_since_improved | cell_id + year | 0 | GADM_ID_3, data=data[data_all$crop_all_years %in% T,])
  
      df_out_i <- bind_rows(felm_allroads_yearssince_dmspols,
                          felm_allroads_yearssince_dmspols1,
                          felm_allroads_yearssince_dmspols5,
                          felm_allroads_yearssince_urban,
                          felm_allroads_yearssince_cropland,
                          felm_allroads_yearssince_ndvi,
                          felm_allroads_yearssince_ndvi_cropland)
      df_out_i$road_variable <- road_variable
      df_out_i$phase_cat <- phase_cat
      df_out_i$dmspols_base_group <- dmspols_base_group
  
      results_all <- bind_rows(results_all, df_out_i)
      
    }
  }
}

# Export -----------------------------------------------------------------------
saveRDS(results_all, file.path(finaldata_file_path, "lead_lag_results_coefficients", "results_by_phase_5percent.Rds"))




