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

# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "merged_datasets", "grid_data_clean.Rds"))

data$far_addis <- as.numeric(data$distance_city_addisababa >= 100*1000)

data$post_improvedroad_50aboveafter[is.na(data$post_improvedroad_50aboveafter) & !is.na(data$post_improvedroad)] <- 0
data$post_improvedroad_below50after[is.na(data$post_improvedroad_below50after) & !is.na(data$post_improvedroad)] <- 0

data <- data %>%
  dplyr::rename("TempAvg" = "temp_avg",
                "TempMin" = "temp_min",
                "TempMax" = "temp_max")

# Export Results ---------------------------------------------------------------
if(F){
  dv <- "ndvi"
  addis_distance <- "All"
  unit <- "cell"
  cluster_var <- "woreda_hdx_z_code"
}

# All Phases Together ----------------------------------------------------------
for(dv in c("ndvi_cropland", "ndvi", "dmspols_zhang_ihs", "dmspols_zhang_6", "globcover_urban", "globcover_cropland")){ 
  for(addis_distance in c("All", "Far")){
    for(cluster_var in c("woreda_hdx_w_uid", "woreda_hdx_z_code")){

      Sys.sleep(.1)
      for(i in 1:5) gc()
      Sys.sleep(.1)
      
      print(paste(dv, addis_distance,cluster_var, "-----------------"))
      
      #### If DATASET_TYPE is woreda, skip select units
      if(DATASET_TYPE %in% "woreda_panel_hdx_csa"){
        #if(cluster_var %in% "woreda_hdx_w_uid") cluster_var <- "uid"
        #if(cluster_var %in% "woreda_hdx_z_code") cluster_var <- "Z_CODE"
        unit <- "woreda"
      } else{
        unit <- "cell"
      }
      
      #### Define Dependent Variable and Cluster Variable
      data$dv <- data[[dv]]
      data$cluster_var <- data[[cluster_var]]

      #### Subset by Addis Distance
      if(addis_distance %in% "Far"){
        data_temp <- data[data$far_addis %in% 1,]
        #data_w_temp <- data_w[data_w$far_addis %in% 1,]
      } else{
        data_temp <- data
        #data_w_temp <- data_w
      }
      
      #### Create Dependent Variable Label
      if(dv %in% "dmspols_zhang_ihs") dep_var_label <- "DMSP-OLS: IHS"
      if(dv %in% "dmspols_zhang_6") dep_var_label <- "DMSP-OLS: Above Median"
      if(dv %in% "globcover_urban") dep_var_label <- "Globcover: Urban"
      if(dv %in% "globcover_cropland") dep_var_label <- "Globcover: Cropland"
      if(dv %in% "ndvi") dep_var_label <- "NDVI"
      if(dv %in% "ndvi_cropland") dep_var_label <- "NDVI in Cropland Areas"
      
      #### Models
      if(grepl("ndvi", dv)){
        
        lm <- felm(dv ~ post_improvedroad + TempAvg + TempMin + TempMax + precipitation | cell_id + year | 0 | cluster_var, data=data_temp)
        lm_baselineNTL <- felm(dv ~ post_improvedroad + post_improvedroad*dmspols_zhang_1996_group_woreda - dmspols_zhang_1996_group_woreda + TempAvg + TempMin + TempMax + precipitation | cell_id + year | 0 | cluster_var, data=data_temp)
        lm_region <- felm(dv ~ post_improvedroad + post_improvedroad*region_type - region_type + TempAvg + TempMin + TempMax + precipitation | cell_id + year | 0 | cluster_var, data=data_temp)
        
        lm_50 <- felm(dv ~ post_improvedroad_below50after + post_improvedroad_50aboveafter + TempAvg + TempMin + TempMax + precipitation | cell_id + year | 0 | cluster_var, data=data_temp)
        
        lm_50_baselineNTL <- felm(dv ~ post_improvedroad_below50after + post_improvedroad_50aboveafter +
                                    post_improvedroad_below50after*dmspols_zhang_1996_group_woreda + 
                                    post_improvedroad_50aboveafter*dmspols_zhang_1996_group_woreda -
                                    dmspols_zhang_1996_group_woreda +
                                    TempAvg + TempMin + TempMax + precipitation| cell_id + year | 0 | cluster_var, data=data_temp)
        
        lm_50_region <- felm(dv ~ post_improvedroad_below50after + post_improvedroad_50aboveafter +
                               post_improvedroad_below50after*region_type +
                               post_improvedroad_50aboveafter*region_type -
                               region_type +
                               TempAvg + TempMin + TempMax + precipitation| cell_id + year | 0 | cluster_var, data=data_temp)
      
      } else{
        lm <- felm(dv ~ post_improvedroad | cell_id + year | 0 | cluster_var, data=data_temp)
        lm_baselineNTL <- felm(dv ~ post_improvedroad + post_improvedroad*dmspols_zhang_1996_group_woreda - dmspols_zhang_1996_group_woreda | cell_id + year | 0 | cluster_var, data=data_temp)
        lm_region <- felm(dv ~ post_improvedroad + post_improvedroad*region_type - region_type | cell_id + year | 0 | cluster_var, data=data_temp)
        
        lm_50 <- felm(dv ~ post_improvedroad_below50after + post_improvedroad_50aboveafter | cell_id + year | 0 | cluster_var, data=data_temp)
        
        lm_50_baselineNTL <- felm(dv ~ post_improvedroad_below50after + post_improvedroad_50aboveafter +
                                    post_improvedroad_below50after*dmspols_zhang_1996_group_woreda + 
                                    post_improvedroad_50aboveafter*dmspols_zhang_1996_group_woreda -
                                    dmspols_zhang_1996_group_woreda | cell_id + year | 0 | cluster_var, data=data_temp)
        
        lm_50_region <- felm(dv ~ post_improvedroad_below50after + post_improvedroad_50aboveafter +
                               post_improvedroad_below50after*region_type +
                               post_improvedroad_50aboveafter*region_type -
                               region_type | cell_id + year | 0 | cluster_var, data=data_temp)
      }
      
      rm(data_temp)

      stargazer(lm,
                lm_baselineNTL,
                lm_region,
                lm_50,
                lm_50_baselineNTL,
                lm_50_region,
                dep.var.labels.include = T,
                dep.var.labels = c(dep_var_label),
                dep.var.caption = "",
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
                keep = c("post_improvedroad",
                         "post_improvedroad:dmspols_zhang_1996_group_woreda",
                         "post_improvedroad:region_type",
                         "post_improvedroad_below50after",
                         "post_improvedroad_below50after:dmspols_zhang_1996_group_woreda",
                         "post_improvedroad_below50after:region_type",
                         "post_improvedroad_50aboveafter",
                         "post_improvedroad_50aboveafter:dmspols_zhang_1996_group_woreda",
                         "post_improvedroad_50aboveafter:region_type"),
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
      
    }
  }
}

# By Phase ---------------------------------------------------------------------



