# Create Varibles for Analysis

# DESCRIPTION: Creates a clean dataset to be used for analysis. Takes the dataset
# with raw variables merged together. This code uses the raw variables to create
# the variables needed for analysis, and pairs down the variables in the
# final dataset to only those needed. (Limited the final variables resulting
# from the dataset is important to reduce dataset size, particularly among
# the grid level variables).

# NOTES:
# (1) First run on woreda level before grid level. Some variables from the woreda
#     level dataset are merged into the grid level dataset. For example, woreda
#     level nighttime lights and market access
# (2) All distance variables are in meteres.

# Parameters -------------------------------------------------------------------
# Distance threshold to be used to be counted as treated. Differs for grid level
# versus the woreda level.

# Determines whether a grid dataset or not (eg, vs woreda level). Some different
# cleaning steps depending on whether grid or now.
GRID_DATASET <- grepl("grid", DATASET_TYPE)

if(GRID_DATASET){
  NEAR_CUTOFF <- 5 * 1000
  ALL_YEARS_IMPROVED_VAR <- T
} else{
  NEAR_CUTOFF <- 0
  ALL_YEARS_IMPROVED_VAR <- T # creates variable showing all years road was built, 
  # in addition to a variable indicating minimum
  # year.
}

# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "merged_datasets", "grid_data.Rds"))

data$distance_rsdp_phase1 <- NULL
data$distance_rsdp_phase2 <- NULL
data$distance_rsdp_phase3 <- NULL
data$distance_rsdp_phase4 <- NULL

if(DATASET_TYPE %in% "woreda_panel_hdx_csa") data$cell_id <- data$uid

gc(); Sys.sleep(.5); gc(); Sys.sleep(.5)
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

data$distance_improvedroad <- apply(data[,paste0("distance_improvedroad_speedafter_",c(20,25,30,35,45,50,70,120))], 1, FUN = min_NAifAllNA)
data$distance_improvedroad_50aboveafter <- apply(data[,paste0("distance_improvedroad_speedafter_",c(50,70,120))], 1, FUN = min_NAifAllNA)
data$distance_improvedroad_below50after <- apply(data[,paste0("distance_improvedroad_speedafter_",c(20,25,30,35,45))], 1, FUN = min_NAifAllNA)

data$distance_improvedroad_speedafter_20 <- NULL
data$distance_improvedroad_speedafter_25 <- NULL
data$distance_improvedroad_speedafter_30 <- NULL
data$distance_improvedroad_speedafter_35 <- NULL
data$distance_improvedroad_speedafter_45 <- NULL
data$distance_improvedroad_speedafter_50 <- NULL
data$distance_improvedroad_speedafter_70 <- NULL
data$distance_improvedroad_speedafter_120 <- NULL

gc(); Sys.sleep(.5); gc(); Sys.sleep(.5) # TODO: create "rest" function, with GRID as input (Where no rest for woredas)
# Remove cells not in analysis -------------------------------------------------

#### Include unit if near an improved road at some point during the analysis
# 1. Create a temporary improved road variable and make NA values very large as a
# dummy indicator of the road being super far award.
# 2. Check whether minimum distance of road within a cell is within threshold value.
# 3. Them remove temporary variables

data$distance_improvedroad_TEMP <- data$distance_improvedroad
data$distance_improvedroad_TEMP[is.na(data$distance_improvedroad_TEMP)] <- 9999*1000

data <- data %>%
  group_by(cell_id) %>%
  mutate(distance_improvedroad_TEMP_min = min(distance_improvedroad_TEMP)) %>%
  ungroup()
data <- data[data$distance_improvedroad_TEMP_min <= NEAR_CUTOFF,]

data$distance_improvedroad_TEMP <- NULL
data$distance_improvedroad_TEMP_min <- NULL

#### Remove Cells intersect road at any point
# Don't need "distance_anyroad2016" any more; remove to save memory
if(GRID_DATASET){
  data <- data[data$distance_anyroad2016 >= 1000,]
  data$distance_anyroad2016 <- NULL
}

gc(); Sys.sleep(.5); gc()
# Years Since / Post Improved Variables ----------------------------------------
generate_road_improved_variables <- function(road_var, 
                                             data,
                                             all_years_improved_var){
  # DESCRIPTION: Creates variables indicating years since road improved,
  # and first year road was improved.
  
  # INPUT:
  # road_var: name of road variable that captures distance to road in meters
  # data: dataset
  # all_years_improved_var: T/F, whether to add a variable indicating all
  # years near an improved road
  
  print(road_var)
  
  final_vars <- c("year_roadTEMP", "years_since_roadTEMP", "post_roadTEMP")
  
  
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
  
  # Create variable indicating all years road improved: e.g., 2007;2010
  if(all_years_improved_var){
    data <- data %>%
      group_by(cell_id) %>%
      mutate(near_roadTEMP_all_years = paste(near_roadTEMP_X_year, collapse=";") %>% str_replace_all("NA;|;NA", "")) 
    
    final_vars <- c(final_vars, "near_roadTEMP_all_years")
  }
  
  # Variable for each cell of first year became near an improved road
  data_dt <- as.data.table(data)
  data <- data_dt[, year_roadTEMP:=min(near_roadTEMP_X_year,na.rm=T), by=list(cell_id)] %>% as.data.frame()
  data$year_roadTEMP[data$year_roadTEMP %in% Inf] <- NA
  
  # Years since road improved and binary 1/0 road improved variable
  data$years_since_roadTEMP <- data$year - data$year_roadTEMP
  data$post_roadTEMP <- data$years_since_roadTEMP >= 0
  data$post_roadTEMP[is.na(data$post_roadTEMP)] <- 0
  
  # +/- 10 years aggregate
  data$years_since_roadTEMP[data$years_since_roadTEMP >= 10] <- 10
  data$years_since_roadTEMP[data$years_since_roadTEMP <= -10] <- -10
  
  # Subset variables and rename
  data <- data %>%
    dplyr::select(all_of(final_vars))
  
  # Prep variables
  data$years_since_roadTEMP <- data$years_since_roadTEMP %>% as.factor() %>% relevel("-1")
  data$post_roadTEMP <- data$post_roadTEMP %>% as.numeric()
  
  names(data) <- names(data) %>% str_replace_all("roadTEMP", road_type)
  
  return(data)
}

roadimproved_df <- lapply(c("distance_improvedroad", 
                            "distance_improvedroad_50aboveafter", 
                            "distance_improvedroad_below50after"),
                          generate_road_improved_variables, 
                          data, ALL_YEARS_IMPROVED_VAR) %>% bind_cols()

data <- bind_cols(data, roadimproved_df)

gc(); Sys.sleep(.5); gc(); Sys.sleep(.5)
# Lagged treatment -------------------------------------------------------------
data$pre_improvedroad_neg2_5 <- as.numeric(data$years_since_improvedroad %in% -2:-5) %>% as.numeric()
data$pre_improvedroad_neg6_10 <- as.numeric(data$years_since_improvedroad %in% -6:-10) %>% as.numeric()

data$pre_improvedroad_50aboveafter_neg2_5 <- as.numeric(data$years_since_improvedroad_50aboveafter %in% -2:-5) %>% as.numeric()
data$pre_improvedroad_50aboveafter_neg6_10 <- as.numeric(data$years_since_improvedroad_50aboveafter %in% -6:-10) %>% as.numeric()

data$pre_improvedroad_below50after_neg2_5 <- as.numeric(data$years_since_improvedroad_below50after %in% -2:-5) %>% as.numeric()
data$pre_improvedroad_below50after_neg6_10 <- as.numeric(data$years_since_improvedroad_below50after %in% -6:-10) %>% as.numeric()

gc(); Sys.sleep(.5); gc(); Sys.sleep(.5)
# Variables for treated time 2, 3, etc -----------------------------------------
# TODO: This should go in year group (06_) script
if(!GRID_DATASET){
  data <- data %>%
    dplyr::mutate(near_improvedroad_all_years_t1 = near_improvedroad_all_years %>% substring(1,4) %>% as.numeric(),
                  near_improvedroad_all_years_t2 = near_improvedroad_all_years %>% substring(6,9) %>% as.numeric(),
                  near_improvedroad_all_years_t3 = near_improvedroad_all_years %>% substring(11,14) %>% as.numeric(),
                  
                  near_improvedroad_50aboveafter_all_years_t1 = near_improvedroad_50aboveafter_all_years %>% substring(1,4) %>% as.numeric(),
                  near_improvedroad_50aboveafter_all_years_t2 = near_improvedroad_50aboveafter_all_years %>% substring(6,9) %>% as.numeric(),
                  near_improvedroad_50aboveafter_all_years_t3 = near_improvedroad_50aboveafter_all_years %>% substring(11,14) %>% as.numeric(),
                  
                  near_improvedroad_below50after_all_years_t1 = near_improvedroad_below50after_all_years %>% substring(1,4) %>% as.numeric(),
                  near_improvedroad_below50after_all_years_t2 = near_improvedroad_below50after_all_years %>% substring(6,9) %>% as.numeric(),
                  near_improvedroad_below50after_all_years_t3 = near_improvedroad_below50after_all_years %>% substring(11,14) %>% as.numeric()) %>%
    
      dplyr::mutate(post_improvedroad_t1 = as.numeric((near_improvedroad_all_years_t1) - year >= 0),
                  post_improvedroad_t2 = as.numeric((near_improvedroad_all_years_t2) - year >= 0),
                  post_improvedroad_t3 = as.numeric((near_improvedroad_all_years_t3) - year >= 0),
                  
                  post_improvedroad_50aboveafter_t1 = as.numeric((near_improvedroad_50aboveafter_all_years_t1) - year >= 0),
                  post_improvedroad_50aboveafter_t2 = as.numeric((near_improvedroad_50aboveafter_all_years_t2) - year >= 0),
                  post_improvedroad_50aboveafter_t3 = as.numeric((near_improvedroad_50aboveafter_all_years_t3) - year >= 0),
                  
                  post_improvedroad_below50after_t1 = as.numeric((near_improvedroad_below50after_all_years_t1) - year >= 0),
                  post_improvedroad_below50after_t2 = as.numeric((near_improvedroad_below50after_all_years_t2) - year >= 0),
                  post_improvedroad_below50after_t3 = as.numeric((near_improvedroad_below50after_all_years_t3) - year >= 0))
  
  # Replace NAs with 0
  post_t_vars <- names(data)[grepl("_t1$|_t2$|_t3$", names(data)) & grepl("^post", names(data))]
  
  for(var in post_t_vars){
    data[[var]][is.na(data[[var]])] <- 0
  }

}

gc(); Sys.sleep(.5); gc(); Sys.sleep(.5)
# Dependent Variable Transformations -------------------------------------------
# Inverse Hyperbolic Since Transformation 
# This is used by Mitnik et. al. due to lots of zeros in DMSP-OLS 
calc_ihs <- function(x) log(x + sqrt(x^2 + 1))

data <- data %>%
  
  group_by(cell_id) %>%
  
  # Baseline variables
  mutate(dmspols_1996 = dmspols[year == 1996],
         dmspols_zhang_1996 = dmspols_zhang[year == 1996]) %>%
  
  ungroup() %>%
  
  # IHS
  mutate(dmspols_ihs = calc_ihs(dmspols),
         viirs_mean_ihs = calc_ihs(viirs_mean),
         viirs_median_ihs = calc_ihs(viirs_median),
         viirs_max_ihs = calc_ihs(viirs_max),
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

# Binary variables for above NTL threshold
data$dmspols_zhang_2 <- data$dmspols_zhang >= 2
data$dmspols_zhang_6 <- data$dmspols_zhang >= 6

data$viirs_mean_2 <- data$viirs_mean >= 2
data$viirs_mean_6 <- data$viirs_mean >= 6

gc(); Sys.sleep(.5); gc(); Sys.sleep(.5)
# Other variable transformations -----------------------------------------------
data$far_addis <- as.numeric(data$distance_city_addisababa >= 100*1000)
data$distance_city_addisababa <- NULL

gc(); Sys.sleep(.5); gc(); Sys.sleep(.5)
# Geographic Regions -----------------------------------------------------------
data$region_type <- ifelse(data$GADM_ID_1 %in% c("Afar", "Benshangul-Gumaz", "Somali"), "Sparse", "Dense") %>% factor(levels=c("Sparse", "Dense"))
data$GADM_ID_1 <- NULL

if(DATASET_TYPE %in% "woreda_panel_hdx_csa"){
  data$R_NAME <- data$R_NAME %>% as.character()
  data$region_type <- ifelse(data$R_NAME %in% c("Afar", "Benishangul Gumuz", "SOMALI REGION"), "Sparse", "Dense") %>% factor(levels=c("Sparse", "Dense"))
} 

gc(); Sys.sleep(.5); gc(); Sys.sleep(.5)
# Log market access ------------------------------------------------------------
if(DATASET_TYPE %in% "woreda_panel_hdx_csa"){
  
  for(var in names(data)[grepl("^MA_", names(data))]){
    data[[paste0(var,"_log")]] <- log(data[[var]])
  }
  
}

gc(); Sys.sleep(.5); gc(); Sys.sleep(.5)
# Road length & density --------------------------------------------------------
if(!GRID_DATASET){
  
  #### Variables for (1) road length var and (2) road speed numbers
  road_length_vars <- names(data)[grepl("road_length_", names(data))] 
  
  road_lengths <- road_length_vars %>% 
    str_replace_all("road_length_", "") %>% 
    as.numeric() %>% 
    sort()
  road_lengths <- road_lengths[road_lengths>0]
  
  #### Replace NAs with 0 road length
  for(var in road_length_vars){
    data[[var]][is.na(data[[var]])] <- 0
  }
  
  #### Create road_length_[speed]over: length of road of speed and above
  for(speed_i in road_lengths){
    
    road_lengths_speedi_over <- road_lengths[road_lengths >= speed_i]
    
    data[[paste0("road_length_", speed_i, "over")]] <- 
      apply(data[,paste0("road_length_",road_lengths_speedi_over)], 1, FUN = sum)
  }
  
  
}

gc(); Sys.sleep(.5); gc(); Sys.sleep(.5)
# For grid dataset, merge and prep select woreda-level variables ---------------
if(GRID_DATASET){
  woreda_data <- readRDS(file.path(finaldata_file_path, "woreda_panel_hdx_csa", "merged_datasets", "grid_data_clean.Rds"))
  
  woreda_data <- woreda_data %>%
    dplyr::select(uid, year, dmspols_zhang_1996_group, dmspols_1996_group) %>%
    dplyr::rename(woreda_hdx_w_uid = uid)
  
  names(woreda_data)[!(names(woreda_data) %in% c("woreda_hdx_w_uid", "year"))] <-
    paste0(names(woreda_data)[!(names(woreda_data) %in% c("woreda_hdx_w_uid", "year"))], "_woreda")
  
  data <- merge(data, woreda_data, by=c("woreda_hdx_w_uid", "year"), all=T)
  
} else{
  # Add same variable names to woreda to make analysis script work for both
  data$dmspols_zhang_1996_group_woreda <- data$dmspols_zhang_1996_group
  data$dmspols_1996_group_woreda       <- data$dmspols_1996_group
  
  data$woreda_hdx_w_uid       <- data$uid
  data$woreda_hdx_z_code       <- data$Z_CODE
  
}

gc(); Sys.sleep(.5); gc(); Sys.sleep(.5)
# Remove Stuff Don't Need ------------------------------------------------------
# Reduces dataset size if grid dataset where need to trim size of dataset
if(GRID_DATASET){
  data$distance_city_popsize_3groups_g1 <- NULL
  data$distance_city_popsize_3groups_g2 <- NULL
  data$distance_city_popsize_3groups_g3 <- NULL
  data$distance_city_all <- NULL
  
  data$distance_improvedroad_speedafter_20 <- NULL
  data$distance_improvedroad_speedafter_25 <- NULL
  data$distance_improvedroad_speedafter_30 <- NULL
  data$distance_improvedroad_speedafter_35 <- NULL
  data$distance_improvedroad_speedafter_45 <- NULL
  data$distance_improvedroad_speedafter_50 <- NULL
  data$distance_improvedroad_speedafter_70 <- NULL
  data$distance_improvedroad_speedafter_120 <- NULL
  
  data$globcover_cropland_rainfed <- NULL
  data$globcover_cropland_irrigated <- NULL
  data$globcover_cropland_mosaic <- NULL
}

gc(); Sys.sleep(.5); gc(); Sys.sleep(.5)
# Export -----------------------------------------------------------------------
saveRDS(data, file.path(finaldata_file_path, DATASET_TYPE, "merged_datasets", "grid_data_clean_all.Rds"))



