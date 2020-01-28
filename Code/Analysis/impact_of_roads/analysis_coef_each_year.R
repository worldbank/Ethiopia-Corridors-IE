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
  for(addis_distance in c("All", "Far")){
    print(paste(region_type, addis_distance))
    
    if(region_type %in% c("Dense", "Sparse")){
      data_temp <- data[data$region_type %in% region_type,]
      
    } else{
      data_temp <- data
    }
    
    if(addis_distance %in% "Far") data_temp <- data_temp[data_temp$distance_city_addisababa >= 100*1000,]
    
    #data_temp <- data_temp[data_temp$year_improvedroad %in% 1997:2012,]
    
    globcover_urban_df_temp <- bind_rows(
      felm(globcover_urban ~ years_since_improvedroad | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
        lm_confint_tidy("years_since_improvedroad") %>% mutate(var = "All"),
      
      felm(globcover_urban ~ years_since_improvedroad_50aboveafter | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
        lm_confint_tidy("years_since_improvedroad_50aboveafter") %>% mutate(var = "50 Above"),
      
      felm(globcover_urban ~ years_since_improvedroad_below50after | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
        lm_confint_tidy("years_since_improvedroad_below50after") %>% mutate(var = "Below 50")
      ) %>% mutate(region = region_type,
                   addis_distance = addis_distance)
    
    globcover_cropland_df_temp <- bind_rows(
      felm(globcover_cropland ~ years_since_improvedroad | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
        lm_confint_tidy("years_since_improvedroad") %>% mutate(var = "All"),
      
      felm(globcover_cropland ~ years_since_improvedroad_50aboveafter | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
        lm_confint_tidy("years_since_improvedroad_50aboveafter") %>% mutate(var = "50 Above"),
      
      felm(globcover_cropland ~ years_since_improvedroad_below50after | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
        lm_confint_tidy("years_since_improvedroad_below50after") %>% mutate(var = "Below 50")
    ) %>% mutate(region = region_type,
                 addis_distance = addis_distance)
    
    ndvi_df_temp <- bind_rows(
      felm(ndvi ~ years_since_improvedroad | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
        lm_confint_tidy("years_since_improvedroad") %>% mutate(var = "All"),
      
      felm(ndvi ~ years_since_improvedroad_50aboveafter | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
        lm_confint_tidy("years_since_improvedroad_50aboveafter") %>% mutate(var = "50 Above"),
      
      felm(ndvi ~ years_since_improvedroad_below50after | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
        lm_confint_tidy("years_since_improvedroad_below50after") %>% mutate(var = "Below 50")
    ) %>% mutate(region = region_type,
                 addis_distance = addis_distance)
    
    dmspols_zhang_ihs_df_temp <- bind_rows(
      felm(dmspols_zhang_ihs ~ years_since_improvedroad | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
        lm_confint_tidy("years_since_improvedroad") %>% mutate(var = "All"),
      
      felm(dmspols_zhang_ihs ~ years_since_improvedroad_50aboveafter | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
        lm_confint_tidy("years_since_improvedroad_50aboveafter") %>% mutate(var = "50 Above"),
      
      felm(dmspols_zhang_ihs ~ years_since_improvedroad_below50after | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
        lm_confint_tidy("years_since_improvedroad_below50after") %>% mutate(var = "Below 50")
    ) %>% mutate(region = region_type,
                 addis_distance = addis_distance)
    
    dmspols_zhang_2_df_temp <- bind_rows(
      felm(dmspols_zhang_2 ~ years_since_improvedroad | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
        lm_confint_tidy("years_since_improvedroad") %>% mutate(var = "All"),
      
      felm(dmspols_zhang_2 ~ years_since_improvedroad_50aboveafter | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
        lm_confint_tidy("years_since_improvedroad_50aboveafter") %>% mutate(var = "50 Above"),
      
      felm(dmspols_zhang_2 ~ years_since_improvedroad_below50after | year + cell_id | 0 | GADM_ID_3, data=data_temp) %>%
        lm_confint_tidy("years_since_improvedroad_below50after") %>% mutate(var = "Below 50")
    ) %>% mutate(region = region_type,
                 addis_distance = addis_distance)
    
    globcover_urban_df <- bind_rows(globcover_urban_df_temp, globcover_urban_df)
    globcover_cropland_df <- bind_rows(globcover_cropland_df_temp, globcover_cropland_df)
    dmspols_zhang_ihs_df <- bind_rows(dmspols_zhang_ihs_df_temp, dmspols_zhang_ihs_df)
    dmspols_zhang_2_df <- bind_rows(dmspols_zhang_2_df_temp, dmspols_zhang_2_df)
    ndvi_df <- bind_rows(ndvi_df_temp, ndvi_df)
    
  }
}

# Figures ----------------------------------------------------------------------
p_dodge_width <- .5

for(type in c("globcover_urban","dmspols_zhang_ihs", "globcover_cropland", "ndvi")){
  for(addis_distance in c("All", "Far")){
  
  print(type)
  
  if(type %in% "globcover_urban") df <- globcover_urban_df
  if(type %in% "dmspols_zhang_ihs") df <- dmspols_zhang_ihs_df
  if(type %in% "globcover_cropland") df <- globcover_cropland_df
  if(type %in% "ndvi") df <- ndvi_df
  
  p <- ggplot(data=df[df$addis_distance %in% addis_distance,], aes(x=years_since_improved, y=b, ymin=p025, ymax=p975,
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
  ggsave(p, filename = file.path(figures_file_path, "results_202001", paste0("regressions_eachyear_",type,"_addis",addis_distance,".png")),
         height = 7, width =7)
  
  }
}




