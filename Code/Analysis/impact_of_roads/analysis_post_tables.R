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

data$dmspols_zhang_2 <- data$dmspols_zhang >= 2
data$far_addis <- as.numeric(data$distance_city_addisababa >= 100*1000)

data_w <- data %>% 
  group_by(GADM_ID_3, year) %>%
  summarise(dmspols_zhang_ihs = mean(dmspols_zhang_ihs, na.rm=T),
            globcover_urban = mean(globcover_urban, na.rm=T),
            post_improvedroad = max(post_improvedroad, na.rm=T),
            post_improvedroad_50aboveafter = max(post_improvedroad_50aboveafter, na.rm=T),
            post_improvedroad_below50after = max(post_improvedroad_below50after, na.rm=T),
            dmspols_zhang_1996 = mean(dmspols_zhang_1996, na.rm=T),
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
for(dv in c("dmspols_zhang_ihs", "globcover_urban")){
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
      if(dv %in% "globcover_urban") dep_var_label <- "Globcover: Urban"
      
      if(unit %in% "woreda"){
        lm <- felm(dv ~ post_improvedroad | GADM_ID_3 + year | 0 | 0, data=data_w_temp)
        lm_baselineNTL <- felm(dv ~ post_improvedroad*dmspols_zhang_1996_group - dmspols_zhang_1996_group | GADM_ID_3 + year | 0 | 0, data=data_w_temp)
        
        lm_50aboveafter <- felm(dv ~ post_improvedroad_50aboveafter | GADM_ID_3 + year | 0 | 0, data=data_w_temp)
        lm_50aboveafter_baselineNTL <- felm(dv ~ post_improvedroad_50aboveafter*dmspols_zhang_1996_group - dmspols_zhang_1996_group | GADM_ID_3 + year | 0 | 0, data=data_w_temp)
        
        lm_below50after <- felm(dv ~ post_improvedroad_below50after | GADM_ID_3 + year | 0 | 0, data=data_w_temp)
        lm_below50after_baselineNTL <- felm(dv ~ post_improvedroad_below50after*dmspols_zhang_1996_group - dmspols_zhang_1996_group | GADM_ID_3 + year | 0 | 0, data=data_w_temp)
        
      } else{
        lm <- felm(dv ~ post_improvedroad | cell_id + year | 0 | 0, data=data_temp)
        lm_baselineNTL <- felm(dv ~ post_improvedroad*dmspols_zhang_1996_group - dmspols_zhang_1996_group | cell_id + year | 0 | 0, data=data_temp)
        
        lm_50aboveafter <- felm(dv ~ post_improvedroad_50aboveafter | cell_id + year | 0 | 0, data=data_temp)
        lm_50aboveafter_baselineNTL <- felm(dv ~ post_improvedroad_50aboveafter*dmspols_zhang_1996_group - dmspols_zhang_1996_group | cell_id + year | 0 | 0, data=data_temp)
        
        lm_below50after <- felm(dv ~ post_improvedroad_below50after | cell_id + year | 0 | 0, data=data_temp)
        lm_below50after_baselineNTL <- felm(dv ~ post_improvedroad_below50after*dmspols_zhang_1996_group - dmspols_zhang_1996_group | cell_id + year | 0 | 0, data=data_temp)
      }
      
  
      stargazer(lm,
                lm_baselineNTL,
                lm_50aboveafter,
                lm_50aboveafter_baselineNTL,
                lm_below50after,
                lm_below50after_baselineNTL,
                dep.var.labels.include = T,
                dep.var.labels = c(dep_var_label),
                dep.var.caption = "",
                covariate.labels = c("Near Improved Rd.",
                                     "Near Improved Rd. X DMSP Low",
                                     "Near Improved Rd. X DMSP High",
                                     "Near Improved Rd. $>=$50km/hr",
                                     "Near Improved Rd. $>=$50km/hr X DMSP Low",
                                     "Near Improved Rd. $>=$50km/hr X DMSP High",
                                     "Near Improved Rd. $<$50km/hr",
                                     "Near Improved Rd. $<$50km/hr X DMSP Low",
                                     "Near Improved Rd. $<$50km/hr X DMSP High"),
                omit.stat = c("f","ser"),
                align=TRUE,
                no.space=TRUE,
                float=FALSE,
                column.sep.width="-15pt",
                digits=2,
                add.lines = list(
                  c("Cell FE", rep("Y", 6)),
                  c("Year FE", rep("Y", 6))),
                out = file.path(tables_file_path, "results_202001", paste0("results_did_",dv,"_addisdistance",addis_distance,"_unit",unit,".tex")))
    }
  }
}



