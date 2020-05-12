# Market Access Analysis

# Analysis of market access at woreda level.

data <- readRDS(file.path(finaldata_file_path, "woreda_panel_hdx_csa", "merged_datasets", "grid_data_clean.Rds"))

MA_VAR <- "MA_pop2007_theta5_log"



for(MA_VAR in c("MA_pop2007_theta1_log", "MA_pop2007_theta5_log",
                "MA_pop2007_theta1_exclude100km_log", "MA_pop2007_theta5_exclude100km_log")){
  
  data$MA_VAR <- data[[MA_VAR]]
  
  lm_dmspols_zhang_ihs <- felm(dmspols_zhang_ihs ~ MA_VAR | uid + year | 0 | uid, data=data) 
  lm_dmspols_zhang_ihs_baselineNTL <- felm(dmspols_zhang_ihs ~ MA_VAR*dmspols_1996_group - dmspols_1996_group  | uid + year | 0 | uid, data=data) 
  lm_dmspols_zhang_ihs_baselineRegion <- felm(dmspols_zhang_ihs ~ MA_VAR*region_type - region_type  | uid + year | 0 | uid, data=data) 
  
  lm_dmspols_zhang_6 <- felm(dmspols_zhang_6 ~ MA_VAR | uid + year | 0 | uid, data=data) 
  lm_dmspols_zhang_6_baselineNTL <- felm(dmspols_zhang_6 ~ MA_VAR*dmspols_1996_group - dmspols_1996_group  | uid + year | 0 | uid, data=data) 
  lm_dmspols_zhang_6_baselineRegion <- felm(dmspols_zhang_6 ~ MA_VAR*region_type - region_type  | uid + year | 0 | uid, data=data) 
  
  globcover_urban <- felm(globcover_urban ~ MA_VAR | uid + year | 0 | uid, data=data) 
  globcover_urban_baselineNTL <- felm(globcover_urban ~ MA_VAR*dmspols_1996_group - dmspols_1996_group  | uid + year | 0 | uid, data=data) 
  globcover_urban_baselineRegion <- felm(globcover_urban ~ MA_VAR*region_type - region_type  | uid + year | 0 | uid, data=data) 
  
  globcover_cropland <- felm(globcover_cropland ~ MA_VAR | uid + year | 0 | uid, data=data) 
  globcover_cropland_baselineNTL <- felm(globcover_cropland ~ MA_VAR*dmspols_1996_group - dmspols_1996_group  | uid + year | 0 | uid, data=data) 
  globcover_cropland_baselineRegion <- felm(globcover_cropland ~ MA_VAR*region_type - region_type  | uid + year | 0 | uid, data=data) 
  
  stargazer(lm_dmspols_zhang_ihs,
            lm_dmspols_zhang_ihs_baselineNTL,
            lm_dmspols_zhang_ihs_baselineRegion,
            
            lm_dmspols_zhang_6,
            lm_dmspols_zhang_6_baselineNTL,
            lm_dmspols_zhang_6_baselineRegion,
            
            globcover_urban,
            globcover_urban_baselineNTL,
            globcover_urban_baselineRegion,
            
            globcover_cropland,
            globcover_cropland_baselineNTL,
            globcover_cropland_baselineRegion,
            
            dep.var.labels.include = T,
            dep.var.labels = c("DMSP-OLS (Log)", "DMSP-OLS Above Median", "Globcover: Urban", "Globcover: Cropland"),
            dep.var.caption = "",
            covariate.labels = c("log(MA)",
                                 "log(MA) X DMSP Low",
                                 "log(MA) X DMSP High",
                                 "log(MA) X Dense Region"),
            omit.stat = c("f","ser"),
            align=TRUE,
            no.space=TRUE,
            float=FALSE,
            column.sep.width="-15pt",
            digits=2,
            add.lines = list(
              c("Woreda FE", rep("Y", 12)),
              c("Year FE", rep("Y", 12))),
            out = file.path(tables_file_path, paste0("results_",MA_VAR,".tex")))
}



# MA Change --------------------------------------------------------------------
data_group <- data %>%
  group_by(year, dmspols_1996_group) %>%
  summarise(dmspols = mean(dmspols, na.rm=T),
             dmspols_zhang_ihs = mean(dmspols_zhang_ihs),
             MA_pop2007_theta1_log = mean(MA_pop2007_theta1_log, na.rm=T),
             MA_pop2007_theta5_log = mean(MA_pop2007_theta5_log, na.rm=T),
             
             MA_pop2007_theta1_exclude100km_log = mean(MA_pop2007_theta1_exclude100km_log, na.rm=T),
             MA_pop2007_theta5_exclude100km_log = mean(MA_pop2007_theta5_exclude100km_log, na.rm=T))




ggplot(data_group[data_group$dmspols_1996_group %in% 1,]) +
  #geom_line(aes(x=year, y=dmspols_zhang_ihs)) +
  geom_line(aes(x=year, y=MA_pop2007_theta1_log/200))





# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "merged_datasets", "grid_data_clean.Rds"))
data$region_type <- data$region_type %>% as.character() %>% factor(levels = c("Sparse", "Dense"))


data$dmspols_zhang_2 <- data$dmspols_zhang >= 2
data$far_addis <- as.numeric(data$distance_city_addisababa >= 100*1000)

data_w <- data %>% 
  group_by(GADM_ID_3, year, region_type) %>%
  summarise(dmspols_zhang_ihs = mean(dmspols_zhang_ihs, na.rm=T),
            globcover_urban = mean(globcover_urban, na.rm=T),
            globcover_cropland = mean(globcover_cropland, na.rm=T),
            post_improvedroad = max(post_improvedroad, na.rm=T),
            post_improvedroad_50aboveafter = max(post_improvedroad_50aboveafter, na.rm=T),
            post_improvedroad_below50after = max(post_improvedroad_below50after, na.rm=T),
            dmspols_zhang_1996 = mean(dmspols_zhang_1996, na.rm=T),
            dmspols_zhang_6 = mean(dmspols_zhang_6, na.rm=T),
            far_addis = max(far_addis, na.rm=T)) %>%
  ungroup() 

dmspols_zhang_1996_median <- data_w$dmspols_zhang_1996[data_w$dmspols_zhang_1996 > 0] %>% median(na.rm=T) 
data_w$dmspols_zhang_1996_group <- 1
data_w$dmspols_zhang_1996_group[data_w$dmspols_zhang_1996 > 0] <- 2
data_w$dmspols_zhang_1996_group[data_w$dmspols_zhang_1996 >= dmspols_zhang_1996_median] <- 3
data_w$dmspols_zhang_1996_group <- data_w$dmspols_zhang_1996_group %>% as.factor()

for(var in names(data_w)){
  data_w[[var]][data_w[[var]] %in% c(Inf, -Inf)] <- NA
}

# Export Results ---------------------------------------------------------------
if(F){
  dv <- "globcover_urban"
  addis_distance <- "All"
  unit <- "cell"
}

for(dv in c("dmspols_zhang_ihs", "dmspols_zhang_6", "globcover_urban", "globcover_cropland")){
  for(addis_distance in c("All", "Far")){
    for(unit in c("cell", "woreda")){
        
      data$dv <- data[[dv]]
      data_w$dv <- data_w[[dv]]
      
      if(addis_distance %in% "Far"){
        data_temp <- data[data$far_addis %in% 1,]
        data_w_temp <- data_w[data_w$far_addis %in% 1,]
      } else{
        data_temp <- data
        data_w_temp <- data_w
      }
      
      if(dv %in% "dmspols_zhang_ihs") dep_var_label <- "DMSP OLS (Log)"
      if(dv %in% "dmspols_zhang_6") dep_var_label <- "Above Median DMSP OLS"
      if(dv %in% "globcover_urban") dep_var_label <- "Globcover: Urban"
      if(dv %in% "globcover_cropland") dep_var_label <- "Globcover: Cropland"
      
      if(unit %in% "woreda"){
        lm <- felm(dv ~ post_improvedroad | GADM_ID_3 + year | 0 | 0, data=data_w_temp)
        lm_baselineNTL <- felm(dv ~ post_improvedroad*dmspols_zhang_1996_group - dmspols_zhang_1996_group | GADM_ID_3 + year | 0 | 0, data=data_w_temp)
        
        lm_50aboveafter <- felm(dv ~ post_improvedroad_50aboveafter | GADM_ID_3 + year | 0 | 0, data=data_w_temp)
        lm_50aboveafter_baselineNTL <- felm(dv ~ post_improvedroad_50aboveafter*dmspols_zhang_1996_group - dmspols_zhang_1996_group | GADM_ID_3 + year | 0 | 0, data=data_w_temp)
        
        lm_below50after <- felm(dv ~ post_improvedroad_below50after | GADM_ID_3 + year | 0 | 0, data=data_w_temp)
        lm_below50after_baselineNTL <- felm(dv ~ post_improvedroad_below50after*dmspols_zhang_1996_group - dmspols_zhang_1996_group | GADM_ID_3 + year | 0 | 0, data=data_w_temp)
        
      } else{
        # NO INTERACTION
        lm <- felm(dv ~ post_improvedroad | cell_id + year | 0 | woreda_hdx_w_uid, data=data_temp)
        lm_50aboveafter <- felm(dv ~ post_improvedroad_50aboveafter | cell_id + year | 0 | woreda_hdx_w_uid, data=data_temp)
        lm_below50after <- felm(dv ~ post_improvedroad_below50after | cell_id + year | 0 | woreda_hdx_w_uid, data=data_temp)
        
        # INTERACT BASELINE NIGHTTIME LIGHTS
        lm_baselineNTL <- felm(dv ~ post_improvedroad*dmspols_zhang_1996_group - dmspols_zhang_1996_group | cell_id + year | 0 | woreda_hdx_w_uid, data=data_temp)
        lm_50aboveafter_baselineNTL <- felm(dv ~ post_improvedroad_50aboveafter*dmspols_zhang_1996_group - dmspols_zhang_1996_group | cell_id + year | 0 | woreda_hdx_w_uid, data=data_temp)
        lm_below50after_baselineNTL <- felm(dv ~ post_improvedroad_below50after*dmspols_zhang_1996_group - dmspols_zhang_1996_group | cell_id + year | 0 | woreda_hdx_w_uid, data=data_temp)
        
        # INTERACT REGION GROUP
        lm_region <- felm(dv ~ post_improvedroad*region_type - region_type | cell_id + year | 0 | woreda_hdx_w_uid, data=data_temp)
        lm_50aboveafter_region <- felm(dv ~ post_improvedroad_50aboveafter*region_type - region_type | cell_id + year | 0 | woreda_hdx_w_uid, data=data_temp)
        lm_below50after_region <- felm(dv ~ post_improvedroad_below50after*region_type - region_type | cell_id + year | 0 | woreda_hdx_w_uid, data=data_temp)
      }
      
  
      stargazer(lm,
                lm_50aboveafter,
                lm_below50after,
                
                lm_baselineNTL,
                lm_50aboveafter_baselineNTL,
                lm_below50after_baselineNTL,
                
                lm_region,
                lm_50aboveafter_region,
                lm_below50after_region,
                
                dep.var.labels.include = T,
                dep.var.labels = c(dep_var_label),
                dep.var.caption = "",
                covariate.labels = c("Near Improved Rd.",
                                     "Near Improved Rd. $>=$50km/hr",
                                     "Near Improved Rd. $<$50km/hr",
                                     
                                     "Near Improved Rd. X DMSP Low",
                                     "Near Improved Rd. X DMSP High",
                                     
                                     "Near Improved Rd. $>=$50km/hr X DMSP Low",
                                     "Near Improved Rd. $>=$50km/hr X DMSP High",
                                     
                                     "Near Improved Rd. $<$50km/hr X DMSP Low",
                                     "Near Improved Rd. $<$50km/hr X DMSP High",
                                     
                                     "Near Improved Rd. X Dense Region",
                                     "Near Improved Rd. $>=$50km/hr X Dense Region",
                                     "Near Improved Rd. $<$50km/hr X Dense Region"),
                omit.stat = c("f","ser"),
                align=TRUE,
                no.space=TRUE,
                float=FALSE,
                column.sep.width="-15pt",
                digits=2,
                add.lines = list(
                  c("Cell FE", rep("Y", 9)),
                  c("Year FE", rep("Y", 9))),
                out = file.path(tables_file_path, paste0("results_did_",dv,"_addisdistance",addis_distance,"_unit",unit,".tex")))
    }
  }
}



