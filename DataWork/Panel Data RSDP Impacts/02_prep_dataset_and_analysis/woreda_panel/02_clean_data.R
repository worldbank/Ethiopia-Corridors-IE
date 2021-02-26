# Merge Datasets Together

NEAR_CUTOFF <- 5 * 1000
ALL_YEARS_IMPROVED_VAR <- F

# Load Data / Create Dataset Lists ---------------------------------------------
data <- readRDS(file.path(panel_rsdp_imp_data_file_path, "woreda", "merged_datasets", "panel_data.Rds"))

# Distance improved road -------------------------------------------------------
data$distance_improvedroad <- apply(data[,paste0("distance_improvedroad_speedafter_",c(20,25,30,35,45,50,70,120))], 1, FUN = min_na)
data$distance_improvedroad_50aboveafter <- apply(data[,paste0("distance_improvedroad_speedafter_",c(50,70,120))], 1, FUN = min_na)
data$distance_improvedroad_below50after <- apply(data[,paste0("distance_improvedroad_speedafter_",c(20,25,30,35,45))], 1, FUN = min_na)

# Years Since / Post Improved Variables --------------------------------------
generate_road_improved_variables <- function(road_var, 
                                             data,
                                             all_years_improved_var,
                                             NEAR_CUTOFF){
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
  
  ## Variable for year of first improvement
  data <- data %>%
    
    # Whether near improved road
    mutate(near_roadTEMP = distance_roadTEMP <= NEAR_CUTOFF) %>%
    
    # Year road improved (if any). Only consider earliest improved road. If cell near
    # area where another road was improved, we don't consider this. So:
    # 0 0 0 0 2007 0 0 2010 0 0 0 --> would yield 2007, while all zeros returns NA
    mutate(near_roadTEMP_X_year = near_roadTEMP * year) %>%
    mutate(near_roadTEMP_X_year = na_if(near_roadTEMP_X_year, 0) %>% as.numeric())
  
  # Create variable indicating all years road improved: e.g., 2007;2010
  if(all_years_improved_var){
    data <- data %>%
      group_by(cell_id) %>%
      mutate(near_roadTEMP_all_years = paste(near_roadTEMP_X_year, collapse=";") %>% str_replace_all("NA;|;NA", "")) 
    
    final_vars <- c(final_vars, "near_roadTEMP_all_years")
  }
  
  ## Variable for each cell of first year became near an improved road
  data_dt <- as.data.table(data)
  data <- data_dt[, year_roadTEMP:=min(near_roadTEMP_X_year,na.rm=T), by=list(cell_id)] %>% as.data.frame()
  data$year_roadTEMP[data$year_roadTEMP %in% Inf] <- NA
  
  ## Years since road improved and binary 1/0 road improved variable
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
                          data, 
                          ALL_YEARS_IMPROVED_VAR,
                          NEAR_CUTOFF) %>% bind_cols()
data <- bind_cols(data, roadimproved_df)

# Road Variables ---------------------------------------------------------------
# Variables for road length above Xkm in each year

#### Remove speed distacne 0 variables
# Speed distnace of 0 means the road didn't exist then, so we remove
data$road_length_0 <- NULL
data$distance_improvedroad_speedbefore_0 <- NULL

#### Road speeds
speeds_vec <- names(data) %>% 
  str_subset("road_length_") %>% 
  str_replace_all("road_length_", "") %>% 
  as.numeric() %>% 
  sort()

#### Create road variables, using speed limits of X and above
for(speed_i in speeds_vec){
  
  ### Road length above speed
  # Length of road with speed limit of X and above
  vars_road_length <- paste0("road_length_", speeds_vec[speeds_vec >= speed_i])
  
  data[[paste0("road_length_",speed_i,"above")]] <- data %>%
    dplyr::select(all_of(vars_road_length)) %>% 
    apply(1, sum_na)
  
  var_label(data[[paste0("road_length_",speed_i,"above")]]) <- 
    paste0("Road length of  ", speed_i, "km/hr and above")

  ### Minumum distance to road
  # Minimum distance to road with speed limit of X and above
  vars_distance_road <- paste0("distance_road_speed_", speeds_vec[speeds_vec >= speed_i])
  
  data[[paste0("distance_road_speed_",speed_i,"above")]] <- data %>%
    dplyr::select(all_of(vars_distance_road)) %>% 
    apply(1, min, na.rm=T)
  
  var_label(data[[paste0("distance_road_speed_",speed_i,"above")]]) <- 
    paste0("Min distance to road of ", speed_i, "km/hr and above")
}

# Log Variables ----------------------------------------------------------------
calc_ihs <- function(x) log(x + sqrt(x^2 + 1))

ma_var <- data %>% names() %>% str_subset("^MA_")
for(var in ma_var) data[[paste0(var, "_log")]] <- data[[var]] %>% log()

ntl_var <- data %>% names() %>% str_subset("dmspols|globcover")
for(var in ntl_var) data[[paste0(var, "_log")]] <- log(data[[var]] + 1)
for(var in ntl_var) data[[paste0(var, "_ihs")]] <- calc_ihs(data[[var]])

# Dummy Urban/Rural Woredas ----------------------------------------------------
data$dmspols_2bin  <- as.numeric(data$dmspols_2 > 0)
data$dmspols_6bin  <- as.numeric(data$dmspols_6 > 0)
data$dmspols_33bin <- as.numeric(data$dmspols_33 > 0)

# Baseline Variables -----------------------------------------------------------
data <- data %>%
  group_by(cell_id) %>%
  mutate(globcover_urban_1996        = globcover_urban[year == 1996],
         globcover_urban_sum_1996    = globcover_urban_sum[year == 1996],
         globcover_urban_sum_ihs_1996    = globcover_urban_sum_ihs[year == 1996],
         globcover_cropland_1996     = globcover_cropland[year == 1996],
         globcover_cropland_sum_1996 = globcover_cropland_sum[year == 1996],
         ndvi_1996              = ndvi[year == 1996],
         ndvi_cropland_1996     = ndvi_cropland[year == 1996],
         dmspols_zhang_sum2_ihs_1996 = dmspols_zhang_sum2_ihs[year == 1996],
         dmspols_zhang_sum6_ihs_1996 = dmspols_zhang_sum6_ihs[year == 1996],
         dmspols_harmon_sum2_ihs_1996 = dmspols_harmon_sum2_ihs[year == 1996],
         dmspols_harmon_sum6_ihs_1996 = dmspols_harmon_sum6_ihs[year == 1996],
         dmspols_1996           = dmspols[year == 1996],
         dmspols_sum_1996       = dmspols_sum[year == 1996],
         dmspols_log_1996       = dmspols_log[year == 1996],
         dmspols_2bin_1996      = dmspols_2bin[year == 1996],
         dmspols_6bin_1996      = dmspols_6bin[year == 1996],
         dmspols_33bin_1996     = dmspols_33bin[year == 1996],
         dmspols_sum2_1996      = dmspols_sum2[year == 1996],
         dmspols_sum6_1996      = dmspols_sum6[year == 1996],
         dmspols_sum10_1996      = dmspols_sum10[year == 1996],
         dmspols_sum15_1996      = dmspols_sum15[year == 1996],
         dmspols_sum20_1996      = dmspols_sum20[year == 1996],
         dmspols_sum25_1996      = dmspols_sum25[year == 1996],
         dmspols_sum30_1996      = dmspols_sum30[year == 1996],
         dmspols_sum33_1996     = dmspols_sum33[year == 1996],
         dmspols_ihs_1996       = dmspols_ihs[year == 1996],
         dmspols_zhang_log_1996 = dmspols_zhang_log[year == 1996],
         dmspols_zhang_ihs_1996 = dmspols_zhang_ihs[year == 1996],
         dmspols_harmon_log_1996 = dmspols_harmon_log[year == 1996],
         dmspols_harmon_ihs_1996 = dmspols_harmon_ihs[year == 1996]) %>%
  ungroup()

## bin3
data$dmspols_1996_bin3 <- NA
data$dmspols_1996_bin3[data$dmspols_sum2_1996 %in% 0] <- 1
data$dmspols_1996_bin3[data$dmspols_sum2_1996 > 0]    <- 2
data$dmspols_1996_bin3[data$dmspols_sum6_1996 > 0]    <- 3

data$dmspols_1996_bin3_1 <-  as.numeric(data$dmspols_1996_bin3 == 1)
data$dmspols_1996_bin3_2 <-  as.numeric(data$dmspols_1996_bin3 == 2)
data$dmspols_1996_bin3_3 <-  as.numeric(data$dmspols_1996_bin3 == 3)

## bin4
data$dmspols_1996_bin4 <- NA
data$dmspols_1996_bin4[data$dmspols_sum2_1996 %in% 0] <- 1
data$dmspols_1996_bin4[data$dmspols_sum2_1996 > 0]    <- 2
data$dmspols_1996_bin4[data$dmspols_sum6_1996 > 0]    <- 3
data$dmspols_1996_bin4[data$dmspols_sum10_1996 > 0]    <- 4

data$dmspols_1996_bin4_1 <-  as.numeric(data$dmspols_1996_bin4 == 1)
data$dmspols_1996_bin4_2 <-  as.numeric(data$dmspols_1996_bin4 == 2)
data$dmspols_1996_bin4_3 <-  as.numeric(data$dmspols_1996_bin4 == 3)
data$dmspols_1996_bin4_4 <-  as.numeric(data$dmspols_1996_bin4 == 4)

# m <- quantile(data$dmspols_sum_1996[data$dmspols_sum_1996 > 0], c(0.25, .5, 0.75)) %>% as.numeric()
# 
# data$dmspols_1996_bin4_1 <- 0
# data$dmspols_1996_bin4_2 <- 0
# data$dmspols_1996_bin4_3 <- 0
# data$dmspols_1996_bin4_4 <- 0
# 
# data$dmspols_1996_bin4_1[data$dmspols_sum_1996                                <= m[1]] <- 1
# data$dmspols_1996_bin4_2[data$dmspols_sum_1996 > m[1] & data$dmspols_sum_1996 <= m[2]] <- 2
# data$dmspols_1996_bin4_3[data$dmspols_sum_1996 > m[2] & data$dmspols_sum_1996 <= m[3]] <- 3
# data$dmspols_1996_bin4_4[data$dmspols_sum_1996 > m[3]]                                 <- 4

## bin4v2
# data$dmspols_1996_bin42 <- NA
# data$dmspols_1996_bin42[data$dmspols_sum2_1996 %in% 0] <- 1
# data$dmspols_1996_bin42[data$dmspols_sum2_1996 > 0]    <- 2
# data$dmspols_1996_bin42[data$dmspols_sum6_1996 > 0]    <- 3
# data$dmspols_1996_bin42[data$dmspols_sum15_1996 > 0]    <- 4
# 
# data$dmspols_1996_bin42_1 <-  as.numeric(data$dmspols_1996_bin42 == 1)
# data$dmspols_1996_bin42_2 <-  as.numeric(data$dmspols_1996_bin42 == 2)
# data$dmspols_1996_bin42_3 <-  as.numeric(data$dmspols_1996_bin42 == 3)
# data$dmspols_1996_bin42_4 <-  as.numeric(data$dmspols_1996_bin42 == 4)

# Baseline NTL quantiles
dmspols_1996_median <- data$dmspols_1996[data$dmspols_1996 > 0] %>% median(na.rm=T) 
data$dmspols_1996_group[data$dmspols_1996 < dmspols_1996_median] <- "1"
data$dmspols_1996_group[data$dmspols_1996 >= dmspols_1996_median] <- "2"
data$ntl_group <- data$dmspols_1996_group

# Baseline Variables - MA ------------------------------------------------------
MA_vars <- names(data) %>% str_subset("^MA_")

data_MA_vars <- data[data$year %in% 1996, c("cell_id", MA_vars)]
data_MA_vars <- data_MA_vars %>% rename_at(vars(-cell_id), ~ paste0(., '_1996'))

data <- merge(data, data_MA_vars, by = "cell_id")

# Pretrends Variables ----------------------------------------------------------
data <- data %>%
  group_by(cell_id) %>%
  mutate(globcover_urban_sum_pretnd96_92     = globcover_urban_sum[year == 1996]      - globcover_urban_sum[year == 1992],
         globcover_urban_sum_ihs_pretnd96_92 = globcover_urban_sum_ihs[year == 1996]  - globcover_urban_sum_ihs[year == 1992],
         globcover_urban_pretnd96_92     = globcover_urban[year == 1996]   - globcover_urban[year == 1992], 
         dmspols_pretnd96_92             = dmspols[year == 1996]           - dmspols[year == 1992],
         dmspols_log_pretnd96_92         = dmspols_log[year == 1996]       - dmspols_log[year == 1992],
         dmspols_ihs_pretnd96_92         = dmspols_ihs[year == 1996]       - dmspols_ihs[year == 1992],
         dmspols_zhang_log_pretnd96_92   = dmspols_zhang_log[year == 1996] - dmspols_zhang_log[year == 1992],
         dmspols_zhang_ihs_pretnd96_92   = dmspols_zhang_ihs[year == 1996] - dmspols_zhang_ihs[year == 1992],
         dmspols_harmon_log_pretnd96_92   = dmspols_harmon_log[year == 1996] - dmspols_harmon_log[year == 1992],
         dmspols_harmon_ihs_pretnd96_92   = dmspols_harmon_ihs[year == 1996] - dmspols_harmon_ihs[year == 1992]) %>%
  ungroup()

# Other ------------------------------------------------------------------------
data$far_addis <- as.numeric(data$distance_city_addisababa >= 100*1000)

# Select Relevant Variables ----------------------------------------------------
id_vars <- c("cell_id", "year")
adm_vars <- c("W_CODE", "R_CODE", "Z_CODE", "woreda_id")
road_length_vars <- names(data) %>% str_subset("road_length_") %>% str_subset("above")
road_distance_vars <- c("distance_mst")
dist_road_speed_vars <- names(data) %>% str_subset("distance_road_speed_") %>% str_subset("above")
years_since_improved_vars <- names(data) %>% str_subset("years_since_|distance_improvedroad_speedafter_")
satellite_vars <- names(data) %>% str_subset("viirs|dmspols|ndvi|globcover|temp|precipitation")
MA_vars <- names(data) %>% str_subset("MA_")
other_vars <- c("gpw2000", "area_polygon", "distance_city_addisababa", "Pop2007", "ntl_group", "far_addis")

vars_all <- c(id_vars,
              adm_vars,
              road_length_vars,
              road_distance_vars,
              dist_road_speed_vars,
              satellite_vars,
              MA_vars,
              other_vars,
              years_since_improved_vars)

data <- data %>%
  dplyr::select(all_of(vars_all))

# Label variables --------------------------------------------------------------
var_label(data$cell_id) <- "Unique unit ID"
var_label(data$year) <- "year"
var_label(data$dmspols) <- "Average NTL: DMSP-OLS"
var_label(data$dmspols_ihs) <- "Average NTL, IHS: DMSP-OLS"
var_label(data$dmspols_2) <- "Proportion of pixels with NTL > 2"
var_label(data$dmspols_6) <- "Proportion of pixels with NTL > 6"
var_label(data$dmspols_zhang) <- "Average NTL: DMSP-OLS, Intercalibrated"
var_label(data$dmspols_zhang_ihs) <- "Average NTL, IHS: DMSP-OLS, Intercalibrated"
var_label(data$dmspols_zhang_2) <- "Proportion of pixels with NTL > 2 (DMSP-OLS Intercalibrated)"
var_label(data$dmspols_zhang_6) <- "Proportion of pixels with NTL > 6 (DMSP-OLS Intercalibrated)"
var_label(data$viirs_median) <- "Median NTL Across Months: VIIRS"
var_label(data$viirs_mean) <- "Aveage NTL Across Months: VIIRS"
var_label(data$viirs_max) <- "Max NTL Across Months: VIIRS"
var_label(data$viirs_mean_2) <- "Proportion of pixels with average NTL > 2"
var_label(data$viirs_mean_6) <- "Proportion of pixels with average NTL > 6"
var_label(data$temp_min) <- "Daily minimum temperature, average"
var_label(data$temp_max) <- "Daily maximum temperature, average"
var_label(data$temp_avg) <- "Daily average temperature, average"
var_label(data$precipitation) <- "Average precipitation"
var_label(data$ndvi) <- "NDVI"
var_label(data$ndvi_cropland) <- "NDVI in Cropland areas"
var_label(data$globcover_cropland) <- "Proportion of cells classified as cropland"
var_label(data$globcover_cropland_sum) <- "N cells classified as cropland"
var_label(data$globcover_urban) <- "Proportion of cells classified as urban"
var_label(data$globcover_urban_sum) <- "N cells classified as urban"
var_label(data$MA_pop2000_theta1) <- "Market Access"
var_label(data$MA_pop2000_theta2) <- "Market Access"
var_label(data$MA_pop2000_theta5) <- "Market Access"
var_label(data$MA_pop2000_theta8) <- "Market Access"
var_label(data$MA_pop2000_theta1_exclude100km) <- "Market Access, excluding woredas within 100km"
var_label(data$MA_pop2000_theta2_exclude100km) <- "Market Access, excluding woredas within 100km"
var_label(data$MA_pop2000_theta5_exclude100km) <- "Market Access, excluding woredas within 100km"
var_label(data$MA_pop2000_theta8_exclude100km) <- "Market Access, excluding woredas within 100km"
var_label(data$distance_mst) <- "Distance to hypothetical network: min spanning tree (meters)"
var_label(data$dmspols_1996) <- "Average NTL in 1996 (DMSP-OLS)"
var_label(data$woreda_id) <- "Unique Woreda ID"
var_label(data$gpw2000) <- "Population in 2000 (Gridded Population of the World)"

# Export -----------------------------------------------------------------------
saveRDS(data, file.path(panel_rsdp_imp_data_file_path, "woreda", "merged_datasets", "panel_data_clean.Rds"))
#write_dta(data, file.path(panel_rsdp_imp_data_file_path, "woreda", "merged_datasets", "panel_data_clean.dta"))

