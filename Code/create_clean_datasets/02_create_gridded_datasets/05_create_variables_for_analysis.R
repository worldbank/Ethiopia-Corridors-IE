# Create Variables for Analysis

# Run woreda level first as merge in some of those variables to grid level.

# All distances measured in meters.

if(grepl("grid", DATASET_TYPE)){
  NEAR_CUTOFF <- 5 * 1000
} else{
  NEAR_CUTOFF <- 0
}

# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "merged_datasets", "grid_data.Rds"))

if(DATASET_TYPE %in% "woreda_panel_hdx_csa") data$cell_id <- data$uid

# Distance to aggregate road categories ----------------------------------------
# We calculate distance to roads by speed limit. Here we calculate distance
# to any road, road 50 km/hr and above and roads less than 50 km/hr

# If all NA, then return NA; if one isn't NA, return a value
min_NAifAllNA <- function(x){
  
  if(sum(!is.na(x)) > 0){
    return(min(x, na.rm=T))
  } else{
    return(NA)
  }

}

data$distance_road <- apply(data[,paste0("distance_road_speed_",c(20,25,30,35,45,50,70,120))], 1, FUN = min_NAifAllNA)
data$distance_road_50above <- apply(data[,paste0("distance_road_speed_",c(50,70,120))], 1, FUN = min_NAifAllNA)
data$distance_road_below50 <- apply(data[,paste0("distance_road_speed_",c(20,25,30,35,45))], 1, FUN = min_NAifAllNA)

data$distance_improvedroad <- apply(data[,paste0("distance_improvedroad_speedafter_",c(20,25,30,35,45,50,70,120))], 1, FUN = min_NAifAllNA)
data$distance_improvedroad_50aboveafter <- apply(data[,paste0("distance_improvedroad_speedafter_",c(50,70,120))], 1, FUN = min_NAifAllNA)
data$distance_improvedroad_below50after <- apply(data[,paste0("distance_improvedroad_speedafter_",c(20,25,30,35,45))], 1, FUN = min_NAifAllNA)
data$distance_improvedroad_50abovebefore <- apply(data[,paste0("distance_improvedroad_speedbefore_",c(50))], 1, FUN = min_NAifAllNA)
data$distance_improvedroad_below50before <- apply(data[,paste0("distance_improvedroad_speedbefore_",c(20,25,30,35,45))], 1, FUN = min_NAifAllNA)
if(DATASET_TYPE %in% "woreda_panel_hdx_csa") data$distance_improvedroad_50abovebefore <- data$distance_improvedroad_below50before

data$distance_improvedroad_below45after <- apply(data[,paste0("distance_improvedroad_speedafter_",c(20,25,30,35))], 1, FUN = min_NAifAllNA)
data$distance_improvedroad_below35after <- apply(data[,paste0("distance_improvedroad_speedafter_",c(20,25,30))], 1, FUN = min_NAifAllNA)

# Near Roads -------------------------------------------------------------------
for(var in c("distance_road", "distance_road_50above", "distance_road_below50",
             "distance_road_speed_50")){
  data[[str_replace_all(var, "distance_", "near_")]] <- data[[var]] < NEAR_CUTOFF
}

# Years Since / Post Improved Variables ----------------------------------------
generate_road_improved_variables <- function(road_var, data){
  print(road_var)
  
  road_type <- road_var %>% str_replace_all("distance_", "")
  data$distance_roadTEMP <- data[[road_var]]
  
  data <- data %>%
    
    # Whether near improved road
    mutate(near_roadTEMP = distance_roadTEMP <= NEAR_CUTOFF) %>%
    
    # Year road improved (if any). Only consider earliest improved road. If cell near
    # area where another road was improved, we don't consider this. So:
    # 0 0 0 0 2007 0 0 2010 0 0 0 --> would yield 2007, while all zeros returns NA
    mutate(near_roadTEMP_X_year = near_roadTEMP * year) %>%
    mutate(near_roadTEMP_X_year = na_if(near_roadTEMP_X_year, 0)) %>%
    mutate(near_roadTEMP_X_year = near_roadTEMP_X_year %>% as.numeric())
  
  # Variable for each cell of first year became near an improved road
  data_dt <- as.data.table(data)
  data <- data_dt[, year_roadTEMP:=min(near_roadTEMP_X_year,na.rm=T), by=list(cell_id)] %>% as.data.frame()
  data$year_roadTEMP[data$year_roadTEMP %in% Inf] <- NA
  
  data$years_since_roadTEMP <- data$year - data$year_roadTEMP
  data$post_roadTEMP <- data$years_since_roadTEMP >= 0
  
  # Subset variables and rename
  data <- data %>%
    dplyr::select(year_roadTEMP, years_since_roadTEMP, post_roadTEMP)
  
  data$years_since_roadTEMP <- data$years_since_roadTEMP %>% as.factor() %>% relevel("-1")
  data$post_roadTEMP <- data$post_roadTEMP %>% as.numeric()
  
  names(data) <- names(data) %>% str_replace_all("roadTEMP", road_type)
  
  return(data)
}

roadimproved_df <- lapply(c("distance_improvedroad", 
                            "distance_improvedroad_50aboveafter", "distance_improvedroad_below50after",
                            "distance_improvedroad_50abovebefore", "distance_improvedroad_below50before",
                            "distance_improvedroad_speedbefore_50",
                            "distance_improvedroad_below45after",
                            "distance_improvedroad_below35after"),
       generate_road_improved_variables, data) %>% bind_cols()

data <- bind_cols(data, roadimproved_df)

# Dependent Variable Transformations -------------------------------------------
# Inverse Hyperbolic Since Transformation 
# This is used by Mitnik et. al. due to lots of zeros in DMSP-OLS 
calc_ihs <- function(x) log(x + sqrt(x^2 + 1))

data <- data %>%
  
  group_by(cell_id) %>%
  
  # Baseline variables
  mutate(dmspols_1996 = dmspols[year == 1996],
         dmspols_zhang_1996 = dmspols_zhang[year == 1996],
         globcover_cropland_1996 = globcover_cropland[year == 1996],
         globcover_urban_1996 = globcover_urban[year == 1996],
         near_road_speed_50_1996 = near_road_speed_50[year == 1996]) %>%
  
  ungroup() %>%
  
  mutate(dmspols_ihs = calc_ihs(dmspols),
         dmspols_zhang_ihs = calc_ihs(dmspols_zhang),
         dmspols_1996_ihs = calc_ihs(dmspols_1996),
         dmspols_zhang_1996_ihs = calc_ihs(dmspols_zhang_1996))

# Baseline NTL quantiles
dmspols_1996_median <- data$dmspols_1996[data$dmspols_1996 > 0] %>% median(na.rm=T) 
data$dmspols_1996_group <- 1
data$dmspols_1996_group[data$dmspols_1996 > 0] <- 2
data$dmspols_1996_group[data$dmspols_1996 >= dmspols_1996_median] <- 3

dmspols_zhang_1996_median <- data$dmspols_zhang_1996[data$dmspols_zhang_1996 > 0] %>% median(na.rm=T) 
data$dmspols_zhang_1996_group <- 1
data$dmspols_zhang_1996_group[data$dmspols_zhang_1996 > 0] <- 2
data$dmspols_zhang_1996_group[data$dmspols_zhang_1996 >= dmspols_zhang_1996_median] <- 3

data$dmspols_1996_group <- data$dmspols_1996_group %>% as.factor()
data$dmspols_zhang_1996_group <- data$dmspols_zhang_1996_group %>% as.factor()

# Geographic Regions -----------------------------------------------------------
data$region_type <- ifelse(data$GADM_ID_1 %in% c("Afar", "Benshangul-Gumaz", "Somali"), "Sparse", "Dense") %>% factor(levels=c("Sparse", "Dense"))

if(DATASET_TYPE %in% "woreda_panel_hdx_csa"){
  data$R_NAME <- data$R_NAME %>% as.character()
  data$region_type <- ifelse(data$R_NAME %in% c("Afar", "Benishangul Gumuz", "SOMALI REGION"), "Sparse", "Dense") %>% factor(levels=c("Sparse", "Dense"))
} 

# Create Other Variables -------------------------------------------------------
data$dmspols_zhang_2 <- data$dmspols_zhang >= 2
data$dmspols_zhang_6 <- data$dmspols_zhang >= 6

# Log market access ------------------------------------------------------------
if(DATASET_TYPE %in% "woreda_panel_hdx_csa"){
  
  for(var in names(data)[grepl("^MA_", names(data))]){
    data[[paste0(var,"_log")]] <- log(data[[var]])
  }
  
}

# For grid dataset, merge and prep select woreda-level variables ---------------
if(grepl("grid", DATASET_TYPE)){
  woreda_data <- readRDS(file.path(finaldata_file_path, "woreda_panel_hdx_csa", "merged_datasets", "grid_data_clean.Rds"))
  
  woreda_data <- woreda_data %>%
    dplyr::select(uid, year, dmspols_zhang_1996_group, dmspols_1996_group) %>%
    dplyr::rename(woreda_hdx_w_uid = uid)
  
  names(woreda_data)[!(names(woreda_data) %in% c("woreda_hdx_w_uid", "year"))] <-
    paste0(names(woreda_data)[!(names(woreda_data) %in% c("woreda_hdx_w_uid", "year"))], "_woreda")
  
  data <- merge(data, woreda_data, by=c("woreda_hdx_w_uid", "year"), all=T)

} else{
  # Add same variable names to woreda to make analysis script work for both
  data$dmspols_zhang_1996_group_woreda <- data$dmspols_zhang_1996
  data$dmspols_1996_group_woreda       <- data$dmspols_1996_group
  
}

# Export -----------------------------------------------------------------------
saveRDS(data, file.path(finaldata_file_path, DATASET_TYPE, "merged_datasets", "grid_data_clean.Rds"))

