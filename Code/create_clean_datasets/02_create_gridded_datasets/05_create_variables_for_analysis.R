# Create Variables for Analysis

# All distances measured in meters.

NEAR_CUTOFF <- 5 * 1000

# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "merged_datasets", "grid_data.Rds"))

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
                            "distance_improvedroad_speedbefore_50"),
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

# Post Improved Road -----------------------------------------------------------
data$post_improved <- (data$years_since_improved >= 0) %>% as.numeric
data$post_improved_50aboveafter <- (data$years_since_improved_50aboveafter >= 0) %>% as.numeric
data$post_improved_below50after <- (data$years_since_improved_below50after >= 0) %>% as.numeric

# Years Since Improved: Left out factor (-1) -----------------------------------
data$years_since_improved <- data$years_since_improved %>% as.factor() %>% relevel(ref="-1")
data$years_since_improved_speedbefore_50 <- data$years_since_improved_speedbefore_50 %>% as.factor() %>% relevel(ref="-1")
data$years_since_improved_50aboveafter <- data$years_since_improved_50aboveafter %>% as.factor() %>% relevel(ref="-1")
data$years_since_improved_below50after <- data$years_since_improved_below50after %>% as.factor() %>% relevel(ref="-1")

# Factor to Numeric for Select Variables ---------------------------------------
data$near_improvedroad <- data$near_improvedroad %>% as.numeric()
data$near_improvedroad_50aboveafter <- data$near_improvedroad_50aboveafter %>% as.numeric()
data$near_improvedroad_below50after <- data$near_improvedroad_below50after %>% as.numeric()

data$near_road <- data$near_road %>% as.numeric()
data$near_road_50aboveafter <- data$near_road_50above %>% as.numeric()
data$near_road_below50after <- data$near_road_below50 %>% as.numeric()

# Geographic Regions -----------------------------------------------------------
data$region_type <- ifelse(data$GADM_ID_1 %in% c("Afar", "Benshangul-Gumaz", "Somali"), "Sparse", "Dense") %>% as.factor()

# Export -----------------------------------------------------------------------
saveRDS(data, file.path(finaldata_file_path, DATASET_TYPE, "merged_datasets", "grid_data_clean.Rds"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# PURGATORY --------------------------------------------------------------------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 




















































# Years Since Improved Variables -----------------------------------------------
#### Distance to Roads
#data$near_all <- as.numeric(apply(data[,paste0("distance_road_speed_",c(20,25,30,35,45,50,70,120),"")], 1, FUN = min, na.rm=T) <= DIST_THRESH*1000)
#data$near_50above <- as.numeric(apply(data[,paste0("distance_road_speed_",c(50,70,120),"")], 1, FUN = min, na.rm=T) <= DIST_THRESH*1000)
#data$near_below50 <- as.numeric(apply(data[,paste0("distance_road_speed_",c(20,25,30,35,45),"")], 1, FUN = min, na.rm=T) <= DIST_THRESH*1000)

#### Years Since Improvement
data$near_improved_all <- as.numeric(apply(data[,paste0("distance_road_speed_",c(20,25,30,35,45,50,70,120),"_improved")], 1, FUN = min, na.rm=T) <= DIST_THRESH*1000)
data$year_improved_all <- data$near_improved_all * data$year
data$year_improved_all[data$year_improved_all == 0] <- NA

data$near_improved_50above <- as.numeric(apply(data[,paste0("distance_road_speed_",c(50,70,120),"_improved")], 1,FUN = min, na.rm=T) <= DIST_THRESH*1000)
data$year_improved_50above <- data$near_improved_50above * data$year
data$year_improved_50above[data$year_improved_50above == 0] <- NA

data$near_improved_below50 <- as.numeric(apply(data[,paste0("distance_road_speed_",c(20,25,30,35,45),"_improved")], 1,FUN = min, na.rm=T) <= DIST_THRESH*1000)
data$year_improved_below50 <- data$near_improved_below50 * data$year
data$year_improved_below50[data$near_improved_below50 == 0] <- NA

data <- data %>%
  dplyr::group_by(cell_id) %>%
  
  dplyr::mutate(year_improved_all = min(year_improved_all, na.rm=T)) %>%
  dplyr::mutate(year_improved_50above = min(year_improved_50above, na.rm=T)) %>%
  dplyr::mutate(year_improved_below50 = min(year_improved_below50, na.rm=T)) %>%
  
  dplyr::mutate(near_improved_all_2016 = near_improved_all[year == 2016]) %>%
  dplyr::mutate(near_improved_50above_2016 = near_improved_50above[year == 2016]) %>%
  dplyr::mutate(near_improved_below50_2016 = near_improved_below50[year == 2016]) %>%
  
  dplyr::mutate(dmspols_1997 = dmspols[year == 1997]) %>%
  dplyr::mutate(globcover_cropland_2015 = globcover_cropland[year == 2015]) %>%
  
  ungroup()

#### Treatment Variables
data$year_improved_all[data$year_improved_all == Inf] <- NA
data$year_improved_50above[data$year_improved_50above == Inf] <- NA
data$year_improved_below50[data$year_improved_below50 == Inf] <- NA

data$years_since_improved_all <- data$year - data$year_improved_all
data$years_since_improved_50above <- data$year - data$year_improved_50above
data$years_since_improved_below50 <- data$year - data$year_improved_below50

# Binary Treatment Variables
data$years_since_improved_all_bin <- (data$years_since_improved_all >= 0) %>% as.numeric

# Road Sections -- Make NA only when all roads is NA
data$years_since_improved_below50_bin <- (data$years_since_improved_below50 >= 0) %>% as.numeric
data$years_since_improved_50above_bin <- (data$years_since_improved_50above >= 0) %>% as.numeric

data$years_since_improved_below50_bin[is.na(data$years_since_improved_below50_bin)] <- 0
data$years_since_improved_50above_bin[is.na(data$years_since_improved_50above_bin)] <- 0

data$years_since_improved_below50_bin[is.na(data$years_since_improved_all_bin)] <- NA
data$years_since_improved_50above_bin[is.na(data$years_since_improved_all_bin)] <- NA

#### Placebo treatment
data$years_since_improved_all_placebo <- data$years_since_improved_all + 5
data$years_since_improved_50above_placebo <- data$years_since_improved_50above + 5
data$years_since_improved_below50_placebo <- data$years_since_improved_below50 + 5

data$years_since_improved_all_placebo_bin <- as.numeric(data$years_since_improved_all_placebo >= 0)
data$years_since_improved_50above_placebo_bin <- as.numeric(data$years_since_improved_50above_placebo >= 0)
data$years_since_improved_below50_placebo_bin <- as.numeric(data$years_since_improved_below50_placebo >= 0)

### Years Since Treatment - Groups
for(subset in c("all", "50above", "below50")){
  data[[paste0("years_since_improved_",subset,"_group")]] <- NA
  data[[paste0("years_since_improved_",subset,"_group")]][!is.na(data[[paste0("years_since_improved_all")]])] <- "No Treat" # all year regardless of subset
  data[[paste0("years_since_improved_",subset,"_group")]][data[[paste0("years_since_improved_",subset)]] == 0] <- "T: 0"
  data[[paste0("years_since_improved_",subset,"_group")]][data[[paste0("years_since_improved_",subset)]] == 1] <- "T: 1"
  data[[paste0("years_since_improved_",subset,"_group")]][data[[paste0("years_since_improved_",subset)]] == 2] <- "T: 2"
  data[[paste0("years_since_improved_",subset,"_group")]][data[[paste0("years_since_improved_",subset)]] == 3] <- "T: 3"
  data[[paste0("years_since_improved_",subset,"_group")]][data[[paste0("years_since_improved_",subset)]] >= 4] <- "T: 4"
  data[[paste0("years_since_improved_",subset,"_group")]] <- as.factor(data[[paste0("years_since_improved_",subset,"_group")]])
}

### Years Since Treatment - Groups - Placebo
for(subset in c("all", "50above", "below50")){
  data[[paste0("years_since_improved_",subset,"_group_placebo")]] <- NA
  data[[paste0("years_since_improved_",subset,"_group_placebo")]][!is.na(data[[paste0("years_since_improved_all")]])] <- "No Treat" # all year regardless of subset
  data[[paste0("years_since_improved_",subset,"_group_placebo")]][data[[paste0("years_since_improved_",subset)]] >= 0] <- "Treated"
  data[[paste0("years_since_improved_",subset,"_group_placebo")]][data[[paste0("years_since_improved_",subset)]] == -1] <- "T: -1"
  data[[paste0("years_since_improved_",subset,"_group_placebo")]][data[[paste0("years_since_improved_",subset)]] == -2] <- "T: -2"
  data[[paste0("years_since_improved_",subset,"_group_placebo")]][data[[paste0("years_since_improved_",subset)]] == -3] <- "T: -3"
  data[[paste0("years_since_improved_",subset,"_group_placebo")]][data[[paste0("years_since_improved_",subset)]] <= -4] <- "T: -4"
  data[[paste0("years_since_improved_",subset,"_group_placebo")]] <- as.factor(data[[paste0("years_since_improved_",subset,"_group_placebo")]])
}

# Variable Subsets -------------------------------------------------------------
data$dmspols_1997_bin <- as.numeric(data$dmspols_1997 > 0)

# quantile(data$dmspols_1997[data$dmspols_1997 > 0], c(0.5)) 
data$dmspols_1997_group <- 0
data$dmspols_1997_group[data$dmspols_1997 == 0] <- 1
data$dmspols_1997_group[data$dmspols_1997 > 0 & data$dmspols_1997 <= 4.5] <- 2
data$dmspols_1997_group[data$dmspols_1997 > 4.5] <- 3
data$dmspols_1997_group <- factor(data$dmspols_1997_group)

# Adjust DVs -------------------------------------------------------------------
# Inverse hyperbolic sine transformation
data$dmspols_ihs <- log(data$dmspols + sqrt(data$dmspols^2 + 1))
data$dmspols_zhang_ihs <- log(data$dmspols_zhang + sqrt(data$dmspols_zhang^2 + 1))

# DMSP-OLS Binary
data$dmspols_1 <- as.numeric(data$dmspols > 0)
data$dmspols_5 <- as.numeric(data$dmspols >= 5)

data$dmspols_zhang_1 <- as.numeric(data$dmspols_zhang > 0)
data$dmspols_zhang_5 <- as.numeric(data$dmspols_zhang >= 5)

# NDVI
data$ndvi_cropland[is.na(data$ndvi_cropland)] <- 0
data$ndvi_cropland[!(data$year %in% 2000:2015)] <- NA

data$ndvi_nocropland <- data$ndvi
data$ndvi_nocropland[!(data$globcover_cropland %in% 0)] <- 0
data$ndvi_nocropland[!(data$year %in% 2000:2015)] <- NA

# Adjust Order of Factor Variables ---------------------------------------------
# For years since treatment, make "-1" the left out group.
for(subset in c("all", "50above", "below50")){
  data[[paste0("years_since_improved_",subset)]] <- factor(data[[paste0("years_since_improved_",subset)]])
  data[[paste0("years_since_improved_",subset)]] <- relevel(data[[paste0("years_since_improved_",subset)]], "-1")
  
  data[[paste0("years_since_improved_",subset,"_placebo")]] <- factor(data[[paste0("years_since_improved_",subset,"_placebo")]])
  data[[paste0("years_since_improved_",subset,"_placebo")]] <- relevel(data[[paste0("years_since_improved_",subset,"_placebo")]], "-1")
  
}

# Export -----------------------------------------------------------------------
saveRDS(data, file.path(project_file_path, "Data", "FinalData", "dmspols_grid_dataset", paste0(filename,"_analysisvars.Rds")))




