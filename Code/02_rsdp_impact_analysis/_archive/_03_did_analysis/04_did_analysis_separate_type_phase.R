# Impact of RSDP

# Load Data --------------------------------------------------------------------
data_all <- readRDS(file.path(project_file_path, "Data", "FinalData", "dmspols_grid_dataset", "dmspols_level_dataset_5percentsample_analysisvars.Rds"))

# Subset by Road Type ----------------------------------------------------------
road_variable <- "below50"
phase_cat <- "phase_12"
phase_cat <- "phase_34"

for(phase_cat in c("phase_all", "phase_12", "phase_34")){
  for(road_variable in c("below50", "50above", "all")){
    print(road_variable)
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
    
    # Groups with Leads and Lags
    data$years_since_improved_group2 <- data$years_since_improved %>% as.character %>% as.numeric
    data$years_since_improved_group2[data$years_since_improved_group2 >= 4] <- 4
    data$years_since_improved_group2[data$years_since_improved_group2 <= -4] <- -4
    data$years_since_improved_group2 <- data$years_since_improved_group2 %>% as.factor %>% relevel(ref="0")
    
    felm_allroads_yearssince_dmspols <- felm(dmspols_zhang ~ factor(years_since_improved_group2) | cell_id + year | 0 | GADM_ID_3, data=data)
    felm_allroads_yearssince_dmspols1 <- felm(dmspols_zhang_1 ~ factor(years_since_improved_group2) | cell_id + year | 0 | GADM_ID_3, data=data)
    felm_allroads_yearssince_dmspols5 <- felm(dmspols_zhang_5 ~ factor(years_since_improved_group2) | cell_id + year | 0 | GADM_ID_3, data=data)
    felm_allroads_yearssince_urban <- felm(globcover_urban ~ factor(years_since_improved_group2) | cell_id + year | 0 | GADM_ID_3, data=data)
    felm_allroads_yearssince_cropland <- felm(globcover_cropland ~ factor(years_since_improved_group2) | cell_id + year | 0 | GADM_ID_3, data=data)
    #felm_allroads_yearssince_cropland_irrigated <- felm(globcover_cropland_irrigated ~ factor(years_since_improved_group2) | cell_id + year | 0 | GADM_ID_3, data=data)
    #felm_allroads_yearssince_cropland_rainfed <- felm(globcover_cropland_rainfed ~ factor(years_since_improved_group2) | cell_id + year | 0 | GADM_ID_3, data=data)
    #felm_allroads_yearssince_cropland_mosaic <- felm(globcover_cropland_mosaic ~ factor(years_since_improved_group2) | cell_id + year | 0 | GADM_ID_3, data=data)
    felm_allroads_yearssince_ndvi <- felm(ndvi ~ factor(years_since_improved_group2) | cell_id + year | 0 | GADM_ID_3, data=data)
    felm_allroads_yearssince_ndvi_cropland <- felm(ndvi_cropland ~ factor(years_since_improved_group2) | cell_id + year | 0 | GADM_ID_3, data=data)
    #felm_allroads_yearssince_ndvi_nocropland <- felm(ndvi_nocropland ~ factor(years_since_improved_group2) | cell_id + year | 0 | GADM_ID_3, data=data)
    
    stargazer(felm_allroads_yearssince_dmspols,
              felm_allroads_yearssince_dmspols1,
              felm_allroads_yearssince_dmspols5,
              felm_allroads_yearssince_urban,
              felm_allroads_yearssince_cropland,
              felm_allroads_yearssince_ndvi,
              felm_allroads_yearssince_ndvi_cropland,
              dep.var.labels.include = T,
              dep.var.labels = c("NTL","NTL $>$ 0","NTL $>$ 5","Urban",
                                 "Crop", "NDVI", "NDVI: Crop"),
              dep.var.caption = "",
              covariate.labels = c(paste0(road_name,"$t \\leq -4$"), 
                                   paste0(road_name,"$t-3$"),
                                   paste0(road_name,"$t-2$"),
                                   paste0(road_name,"$t-1$"),
                                   paste0(road_name,"$t+1$"),
                                   paste0(road_name,"$t+2$"),
                                   paste0(road_name,"$t+3$"),
                                   paste0(road_name,"$t \\geq 4$")),
              omit.stat = c("f","ser"),
              align=TRUE,
              no.space=TRUE,
              float=FALSE,
              column.sep.width="-15pt",
              digits=2,
              add.lines = list(
                c("Cell FE", rep("Y", 7)),
                c("Year FE", rep("Y", 7))),
              out = file.path(tables_file_path, paste0("results_did_",
                                                       road_variable,
                                                       "_",
                                                       phase_cat,
                                                       ".tex")))
    
  }
}





