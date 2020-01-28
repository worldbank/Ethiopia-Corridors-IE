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
                dep.var.labels = c("DMSP-OLS (Log)"),
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







# Functions --------------------------------------------------------------------
lm_confint_tidy <- function(lm, varremove){
  lm_confint <- confint(lm) %>% 
    as.data.frame
  names(lm_confint) <- c("p025", "p975")
  lm_confint$b <- (lm_confint$p025 + lm_confint$p975)/2
  lm_confint$variable <- row.names(lm_confint)
  
  lm_confint <- lm_confint[!grepl("cluster_id)|year)|Intercept)", lm_confint$variable),]
  lm_confint$years_since_improved <- gsub(varremove, "", lm_confint$variable) %>% as.numeric
  
  return(lm_confint)
}


# Overal Results ---------------------------------------------------------------
globcover_urban_df <- data.frame(NULL)
globcover_cropland_df <- data.frame(NULL)
ndvi_df <- data.frame(NULL)
dmspols_zhang_ihs_df <- data.frame(NULL)
dmspols_zhang_2_df <- data.frame(NULL)

for(region_type in c("All", "Dense", "Sparse")){
  print(region_type)
  if(region_type %in% c("Dense", "Sparse")){
    data_temp <- data[data$region_type %in% region_type,]
  } else{
    data_temp <- data
  }
  
  globcover_urban_df_temp <- bind_rows(
    felm(globcover_urban ~ years_since_improvedroad | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
      lm_confint_tidy("years_since_improvedroad") %>% mutate(var = "All"),
    
    felm(globcover_urban ~ years_since_improvedroad_50aboveafter | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
      lm_confint_tidy("years_since_improvedroad_50aboveafter") %>% mutate(var = "50 Above"),
    
    felm(globcover_urban ~ years_since_improvedroad_below50after | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
      lm_confint_tidy("years_since_improvedroad_below50after") %>% mutate(var = "Below 50")
    ) %>% mutate(region = region_type)
  
  globcover_cropland_df_temp <- bind_rows(
    felm(globcover_cropland ~ years_since_improvedroad | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
      lm_confint_tidy("years_since_improvedroad") %>% mutate(var = "All"),
    
    felm(globcover_cropland ~ years_since_improvedroad_50aboveafter | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
      lm_confint_tidy("years_since_improvedroad_50aboveafter") %>% mutate(var = "50 Above"),
    
    felm(globcover_cropland ~ years_since_improvedroad_below50after | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
      lm_confint_tidy("years_since_improvedroad_below50after") %>% mutate(var = "Below 50")
  ) %>% mutate(region = region_type)
  
  ndvi_df_temp <- bind_rows(
    felm(ndvi ~ years_since_improvedroad | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
      lm_confint_tidy("years_since_improvedroad") %>% mutate(var = "All"),
    
    felm(ndvi ~ years_since_improvedroad_50aboveafter | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
      lm_confint_tidy("years_since_improvedroad_50aboveafter") %>% mutate(var = "50 Above"),
    
    felm(ndvi ~ years_since_improvedroad_below50after | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
      lm_confint_tidy("years_since_improvedroad_below50after") %>% mutate(var = "Below 50")
  ) %>% mutate(region = region_type)
  
  dmspols_zhang_ihs_df_temp <- bind_rows(
    felm(dmspols_zhang_ihs ~ years_since_improvedroad | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
      lm_confint_tidy("years_since_improvedroad") %>% mutate(var = "All"),
    
    felm(dmspols_zhang_ihs ~ years_since_improvedroad_50aboveafter | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
      lm_confint_tidy("years_since_improvedroad_50aboveafter") %>% mutate(var = "50 Above"),
    
    felm(dmspols_zhang_ihs ~ years_since_improvedroad_below50after | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
      lm_confint_tidy("years_since_improvedroad_below50after") %>% mutate(var = "Below 50")
  ) %>% mutate(region = region_type)
  
  dmspols_zhang_2_df_temp <- bind_rows(
    felm(dmspols_zhang_2 ~ years_since_improvedroad | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
      lm_confint_tidy("years_since_improvedroad") %>% mutate(var = "All"),
    
    felm(dmspols_zhang_2 ~ years_since_improvedroad_50aboveafter | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
      lm_confint_tidy("years_since_improvedroad_50aboveafter") %>% mutate(var = "50 Above"),
    
    felm(dmspols_zhang_2 ~ years_since_improvedroad_below50after | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
      lm_confint_tidy("years_since_improvedroad_below50after") %>% mutate(var = "Below 50")
  ) %>% mutate(region = region_type)
  
  globcover_urban_df <- bind_rows(globcover_urban_df_temp, globcover_urban_df)
  globcover_cropland_df <- bind_rows(globcover_cropland_df_temp, globcover_cropland_df)
  dmspols_zhang_ihs_df <- bind_rows(dmspols_zhang_ihs_df_temp, dmspols_zhang_ihs_df)
  dmspols_zhang_2_df <- bind_rows(dmspols_zhang_2_df_temp, dmspols_zhang_2_df)
  ndvi_df <- bind_rows(ndvi_df_temp, ndvi_df)
}

# Figures ----------------------------------------------------------------------
p_dodge_width <- .5

for(type in c("globcover_urban","dmspols_zhang_ihs", "globcover_cropland", "ndvi")){
  
  print(type)
  
  if(type %in% "globcover_urban") df <- globcover_urban_df
  if(type %in% "dmspols_zhang_ihs") df <- dmspols_zhang_ihs_df
  if(type %in% "globcover_cropland") df <- globcover_cropland_df
  if(type %in% "ndvi") df <- ndvi_df
  
  p <- ggplot(data=df, aes(x=years_since_improved, y=b, ymin=p025, ymax=p975,
                                                                group = var, color = var)) + 
    geom_vline(xintercept=0,size=3,alpha=0.15) +
    geom_hline(yintercept=0,size=1,alpha=0.15) +
    geom_point(position = position_dodge(width = p_dodge_width),size=1.5) + 
    geom_linerange(position = position_dodge(width = p_dodge_width),size=1) +
    labs(x="Years Since Improved Road Constructed",
         y="Coefficient",
         color="Road\nType") +
    theme_minimal() +
    facet_wrap(~region, scales="free", nrow=3)
  ggsave(p, filename = file.path(figures_file_path, "results_202001", paste0("regressions_eachyear_",type,".png")),
         height = 7, width =7)
  
}




