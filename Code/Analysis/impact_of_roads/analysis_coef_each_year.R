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

# Functions --------------------------------------------------------------------
lm_confint_tidy <- function(lm, years_since_variable){
  lm_confint <- confint(lm) %>% 
    as.data.frame
  names(lm_confint) <- c("p025", "p975")
  lm_confint$b <- (lm_confint$p025 + lm_confint$p975)/2
  lm_confint$variable <- row.names(lm_confint)
  
  lm_confint <- lm_confint[!grepl("cluster_id)|year)|Intercept)", lm_confint$variable),]
  lm_confint$years_since_improved <- gsub(years_since_variable, "", lm_confint$variable) %>% as.numeric
  
  return(lm_confint)
}

# Overal Results ---------------------------------------------------------------
region_type <- "All"
addis_distance <- "All"
phase <- "all"
dv <- "globcover_urban"

results_df <- data.frame(NULL)

for(region_type in c("All", "Dense", "Sparse")){
  for(addis_distance in c("All", "Far")){
    for(phase in c("phase_all", "phase_1", "phase_2", "phase_3", "phase_4")){
      for(dv in c("globcover_urban","globcover_cropland", "ndvi", "dmspols_ihs", "dmspols_zhang_ihs", "dmspols_zhang_2", "dmspols_zhang_6")){
        for(ntl_group in c("All", "1", "2", "3")){
          
          # Printing so know where we be!
          print(paste(region_type, addis_distance, phase, dv))
          
          data_temp <- data
          
          #### Subset by region type
          if(region_type %in% c("Dense", "Sparse")) data_temp <- data_temp[data_temp$region_type %in% region_type,]
          
          #### Subset by baseline nighttime lights
          if(region_type %in% c("1", "2", "3")) data_temp <- data_temp[data_temp$dmspols_zhang_1996_group %in% region_type %>% as.numeric(),]
          
          #### Subset by All or Far from Addis
          if(addis_distance %in% "Far") data_temp <- data_temp[data_temp$distance_city_addisababa >= 100*1000,]
          
          #### Add dependent variable as temp variable
          data_temp$dv <- data_temp[[dv]]
          
          #### Subsetting by Phase
          if(phase %in% "all")     phase_years <- 1997:2016
          if(phase %in% "phase_1") phase_years <- 1997:2002
          if(phase %in% "phase_2") phase_years <- 2003:2007
          if(phase %in% "phase_3") phase_years <- 2008:2010
          if(phase %in% "phase_4") phase_years <- 2011:2016
    
          data_temp_improvedroad              <- data_temp[data_temp$year_improvedroad              %in% phase_years,]
          data_temp_improvedroad_50aboveafter <- data_temp[data_temp$year_improvedroad_50aboveafter %in% phase_years,]
          data_temp_improvedroad_below50after <- data_temp[data_temp$year_improvedroad_below50after %in% phase_years,]
          
          #### Estimate Models
          
          results_df_temp <- bind_rows(
            felm(dv ~ years_since_improvedroad | year + cell_id | 0 | GADM_ID_3, data=data_temp_improvedroad) %>%
              lm_confint_tidy("years_since_improvedroad") %>% mutate(var = "All"),
            
            felm(dv ~ years_since_improvedroad_50aboveafter | year + cell_id | 0 | GADM_ID_3, data=data_temp_improvedroad_50aboveafter) %>%
              lm_confint_tidy("years_since_improvedroad_50aboveafter") %>% mutate(var = "50 Above"),
            
            felm(dv ~ years_since_improvedroad_below50after | year + cell_id | 0 | GADM_ID_3, data=data_temp_improvedroad_below50after) %>%
              lm_confint_tidy("years_since_improvedroad_below50after") %>% mutate(var = "Below 50")
            ) %>% mutate(region = region_type,
                         addis_distance = addis_distance,
                         phase = phase,
                         dv = dv,
                         ntl_group = ntl_group)
          
          results_df <- bind_rows(results_df, results_df_temp)
          
        }
      }
    }
  }
}

# Export Results ---------------------------------------------------------------
saveRDS(results_df, file.path(finaldata_file_path, DATASET_TYPE, "results", "results_coef_each_year.Rds"))

# Load Data --------------------------------------------------------------------
results_df <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "results", "results_coef_each_year.Rds"))

# Figures ----------------------------------------------------------------------
p_dodge_width <- .5

for(dv in c("globcover_urban", "dmspols_ihs", "dmspols_zhang_ihs", "globcover_cropland", "ndvi",
              "dmspols_zhang_2", "dmspols_zhang_6")){
  for(addis_distance in c("All", "Far")){
    for(phase in c("phase_all",  "phase_1", "phase_2", "phase_3", "phase_4")){
      for(ntl_group in c("All", "1", "2", "3")){
        
        print(paste(dv, addis_distance, phase))
      
        if(dv == "globcover_urban")    dv_title <- "Globcover: Urban"
        if(dv == "globcover_cropland") dv_title <- "Globcover: Cropland"
        if(dv == "ndvi")               dv_title <- "NDVI"
        if(dv == "dmspols_ihs")        dv_title <- "NTL - DMSPOLS (Log)"
        if(dv == "dmspols_zhang_ihs")  dv_title <- "NTL - DMSPOLS (Log)"
        if(dv == "dmspols_zhang_2")    dv_title <- "DMSPOLS >= 2"
        if(dv == "dmspols_zhang_6")    dv_title <- "DMSPOLS >= 6"
        
        p <- ggplot(data = results_df[(results_df$dv %in% dv) & 
                                      (results_df$addis_distance %in% addis_distance) & 
                                      (results_df$phase %in% phase) & 
                                      (results_df$ntl_group %in% ntl_group),], 
                    aes(x=years_since_improved, y=b, ymin=p025, ymax=p975,
                        group = var, color = var)) + 
          geom_vline(xintercept=0,size=3,alpha=0.15) +
          geom_hline(yintercept=0,size=1,alpha=0.15) +
          geom_point(position = position_dodge(width = p_dodge_width),size=1.5) + 
          geom_linerange(position = position_dodge(width = p_dodge_width),size=1) +
          labs(x="Years Since Improved Road Constructed",
               y="Coefficient",
               color="Road\nType",
               title = dv_title) +
          theme_minimal() +
          theme(plot.title = element_text(face="bold", hjust=.5)) +
          facet_wrap(~region, scales="fixed", nrow=1)
        ggsave(p, filename = file.path(figures_file_path, paste0("regressions_eachyear_",dv,"_addis",addis_distance,"_",phase,"_ntl",ntl_group,".png")),
               height = 3.5, width =11)
        
        
      }
    }
  }
}




