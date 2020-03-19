# Exploratory Analysis

# APPROACH: Somewhat follow the Haiti paper. There it seems road improvement
# kinda random throughout their years. Here that's not the case, but maybe
# can assume that within an RDSP phase?

# OUTLINE
# 1. Overall impact of each phase.
# 2. Heterogeneity of impact, within each phase
#    2.1. Road type 
#    2.2. Baseline Dep Var (for ntl, num or divide into thirds a la aiddata?)
#    2.3. Distance to City (could break down by city pop)

# DEPENDENT VARIABLES
# 1. dmspols_zhang_ihs
# 2. globcover_urban
# 3. Cropland? NDVI?

# PRESENT RESULTS
# 1. Post-treatment
# 2. Coef-plots. In same plot. All cells and (for hetro), below/above cutoffs (median / quartiles). Super important to see pre-trends.

#geojson_filepath <- "C:/Users/wb521633/Dropbox/geojsons"
#files_all <- list.files(geojson_filepath, full.names=T, pattern="*.geojson")
#routes_all_df <- lapply(files_all, function(file_i) st_read(file_i)) %>% do.call(what="rbind")

# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "merged_datasets", "grid_data_clean.Rds"))

data$far_addis <- as.numeric(data$distance_city_addisababa >= 100*1000)

# Export Results ---------------------------------------------------------------
if(F){
  dv <- "globcover_urban"
  addis_distance <- "All"
  unit <- "cell"
  cluster_var <- "woreda_hdx_w_uid"
}

# woreda_hdx_z_code
for(dv in c("dmspols_zhang_ihs", "dmspols_zhang_6", "globcover_urban", "globcover_cropland")){
  for(addis_distance in c("All", "Far")){
    for(unit in c("cell")){ # "cell", "woreda"
      for(cluster_var in c("woreda_hdx_w_uid", "woreda_hdx_z_code")){
      
        gc()
        print(paste(dv, addis_distance, unit, cluster_var, "-----------------"))
          
        data$dv <- data[[dv]]
        data$cluster_var <- data[[cluster_var]]
  
        if(addis_distance %in% "Far"){
          data_temp <- data[data$far_addis %in% 1,]
          #data_w_temp <- data_w[data_w$far_addis %in% 1,]
        } else{
          data_temp <- data
          #data_w_temp <- data_w
        }
        
        if(dv %in% "dmspols_zhang_ihs") dep_var_label <- "DMSP OLS (Log)"
        if(dv %in% "dmspols_zhang_6") dep_var_label <- "Above Median DMSP OLS"
        if(dv %in% "globcover_urban") dep_var_label <- "Globcover: Urban"
        if(dv %in% "globcover_cropland") dep_var_label <- "Globcover: Cropland"
        
        if(dv %in% "globcover_cropland"){
          control_vars <- "+ temp_avg + precipitation"
        } else{
          control_vars <- ""
        }

        if(unit %in% "woreda"){
          lm <- felm(dv ~ post_improvedroad | GADM_ID_3 + year | 0 | 0, data=data_w_temp)
          lm_baselineNTL <- felm(dv ~ post_improvedroad*dmspols_zhang_1996_group - dmspols_zhang_1996_group | GADM_ID_3 + year | 0 | 0, data=data_w_temp)
          
          lm_50aboveafter <- felm(dv ~ post_improvedroad_50aboveafter | GADM_ID_3 + year | 0 | 0, data=data_w_temp)
          lm_50aboveafter_baselineNTL <- felm(dv ~ post_improvedroad_50aboveafter*dmspols_zhang_1996_group - dmspols_zhang_1996_group | GADM_ID_3 + year | 0 | 0, data=data_w_temp)
          
          lm_below50after <- felm(dv ~ post_improvedroad_below50after | GADM_ID_3 + year | 0 | 0, data=data_w_temp)
          lm_below50after_baselineNTL <- felm(dv ~ post_improvedroad_below50after*dmspols_zhang_1996_group - dmspols_zhang_1996_group | GADM_ID_3 + year | 0 | 0, data=data_w_temp)
          
        } else{
          # NO INTERACTION
          lm <- felm(as.formula(paste0("dv ~ post_improvedroad ",control_vars," | cell_id + year | 0 | cluster_var")), data=data_temp)
          lm_50aboveafter <- felm(as.formula(paste0("dv ~ post_improvedroad_50aboveafter ",control_vars," | cell_id + year | 0 | cluster_var")), data=data_temp)
          lm_below50after <- felm(as.formula(paste0("dv ~ post_improvedroad_below50after ",control_vars," | cell_id + year | 0 | cluster_var")), data=data_temp)
          
          # INTERACT BASELINE NIGHTTIME LIGHTS
          lm_baselineNTL <- felm(as.formula(paste0("dv ~ post_improvedroad*dmspols_zhang_1996_group_woreda - dmspols_zhang_1996_group_woreda ",control_vars," | cell_id + year | 0 | cluster_var")), data=data_temp)
          lm_50aboveafter_baselineNTL <- felm(as.formula(paste0("dv ~ post_improvedroad_50aboveafter*dmspols_zhang_1996_group_woreda - dmspols_zhang_1996_group_woreda ",control_vars," | cell_id + year | 0 | cluster_var")), data=data_temp)
          lm_below50after_baselineNTL <- felm(as.formula(paste0("dv ~ post_improvedroad_below50after*dmspols_zhang_1996_group_woreda - dmspols_zhang_1996_group_woreda ",control_vars," | cell_id + year | 0 | cluster_var")), data=data_temp)
          
          # INTERACT REGION GROUP
          lm_region <- felm(as.formula(paste0("dv ~ post_improvedroad*region_type - region_type ",control_vars," | cell_id + year | 0 | cluster_var")), data=data_temp)
          lm_50aboveafter_region <- felm(as.formula(paste0("dv ~ post_improvedroad_50aboveafter*region_type - region_type ",control_vars," | cell_id + year | 0 | cluster_var")), data=data_temp)
          lm_below50after_region <- felm(as.formula(paste0("dv ~ post_improvedroad_below50after*region_type - region_type ",control_vars," | cell_id + year | 0 | cluster_var")), data=data_temp)
        }
        
        stargazer(lm,
                  lm_baselineNTL,
                  lm_region,
                  lm_50,
                  lm_50_baselineNTL,
                  lm_50_region,
                  dep.var.labels.include = T,
                  dep.var.labels = c(dep_var_label),
                  dep.var.caption = "",
                  keep = c("post_improvedroad",
                           "post_improvedroad:dmspols_zhang_1996_group_woreda",
                           "post_improvedroad:region_type",
                           "post_improvedroad_below50after",
                           "post_improvedroad_below50after:dmspols_zhang_1996_group_woreda",
                           "post_improvedroad_below50after:region_type",
                           "post_improvedroad_50aboveafter",
                           "post_improvedroad_50aboveafter:dmspols_zhang_1996_group_woreda",
                           "post_improvedroad_50aboveafter:region_type"),
                  covariate.labels =   c("Near Improved Rd.",
                                         "Near Improved Rd. X DMSP Low",
                                         "Near Improved Rd. X DMSP High",
                                         "Near Improved Rd. X Dense Region",
                                         
                                         "Near Improved Rd. $<$50km/hr",
                                         "Near Improved Rd. $<$50km/hr X DMSP Low",
                                         "Near Improved Rd. $<$50km/hr X DMSP High",
                                         "Near Improved Rd. $<$50km/hr X Dense Region",
                                         
                                         "Near Improved Rd. $>=$50km/hr",
                                         "Near Improved Rd. $>=$50km/hr X DMSP Low",
                                         "Near Improved Rd. $>=$50km/hr X DMSP High",
                                         "Near Improved Rd. $>=$50km/hr X Dense Region"),
                  omit.stat = c("f","ser"),
                  align=TRUE,
                  no.space=TRUE,
                  float=FALSE,
                  column.sep.width="-15pt",
                  digits=2,
                  add.lines = list(
                    c("Cell FE", rep("Y", 6)),
                    c("Year FE", rep("Y", 6))),
                  out = file.path(tables_file_path, paste0("results_did_grouped_",dv,"_addisdistance",addis_distance,"_clustervar",cluster_var,"_unit",unit,".tex")))
      
        stargazer(lm,
                  lm_50aboveafter,
                  lm_below50after,
                  
                  lm_baselineNTL,
                  lm_50aboveafter_baselineNTL,
                  lm_below50after_baselineNTL,
                  
                  lm_region,
                  lm_50aboveafter_region,
                  lm_below50after_region,
                  
                  keep = c("post_improvedroad"),
                  
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
                  out = file.path(tables_file_path, paste0("results_did_",dv,"_addisdistance",addis_distance,"_clustervar",cluster_var,"_unit",unit,".tex")))
      }
    }
  }
}



