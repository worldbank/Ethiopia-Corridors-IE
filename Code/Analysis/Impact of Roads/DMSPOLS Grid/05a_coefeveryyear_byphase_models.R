# Impact of RSDP

# Functions ====================================================================
felm_extractcoefs <- function(formula, data){
  model <- tryCatch(felm(formula, data=data), error = function(e) return("error"))
  
  if(model == "error"){
    df_out <- as.data.frame(NULL)
  } else{
    df_out <- summary(model)$coefficients %>% as.data.frame
    df_out$variable <- row.names(df_out)
    df_out$dep_var <- substring(model$model[2],23,70) %>% gsub(pattern="~.*| ",replacement="") 
  }
  
  return(df_out)
  
}

# Load Data --------------------------------------------------------------------
data_all <- readRDS(file.path(project_file_path, "Data", "FinalData", "dmspols_grid_dataset", "dmspols_level_dataset_5percentsample_analysisvars.Rds"))

# Subset by Road Type ----------------------------------------------------------
road_variable <- "below50"
phase_cat <- "phase_12"
phase_cat <- "phase_34"

results_all <- data.frame(NULL)

for(phase_cat in c("phase_all", "phase_12", "phase_34")){
  for(road_variable in c("below50", "50above", "all")){
    print(phase_cat)
    print(road_variable)
    print("")
    
    data <- data_all
    
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
    felm_allroads_yearssince_ndvi_cropland <- felm_extractcoefs(ndvi_cropland ~ years_since_improved | cell_id + year | 0 | GADM_ID_3, data=data)

    df_out_i <- bind_rows(felm_allroads_yearssince_dmspols,
                        felm_allroads_yearssince_dmspols1,
                        felm_allroads_yearssince_dmspols5,
                        felm_allroads_yearssince_urban,
                        felm_allroads_yearssince_cropland,
                        felm_allroads_yearssince_ndvi,
                        felm_allroads_yearssince_ndvi_cropland)
    df_out_i$road_variable <- road_variable
    df_out_i$phase_cat <- phase_cat

    results_all <- bind_rows(results_all, df_out_i)
    

  }
}

# Export -----------------------------------------------------------------------
saveRDS(results_all, file.path(finaldata_file_path, "lead_lag_results_coefficients", "results_by_phase_5percent.Rds"))




