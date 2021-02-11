# Merge Datasets Together

# Load Data / Create Dataset Lists ---------------------------------------------
data <- readRDS(file.path(panel_rsdp_imp_data_file_path, "woreda", "merged_datasets", "panel_data.Rds"))

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
         dmspols_1996           = dmspols[year == 1996],
         dmspols_log_1996       = dmspols_log[year == 1996],
         dmspols_2bin_1996      = dmspols_2bin[year == 1996],
         dmspols_6bin_1996      = dmspols_6bin[year == 1996],
         dmspols_33bin_1996     = dmspols_33bin[year == 1996],
         dmspols_ihs_1996       = dmspols_ihs[year == 1996],
         dmspols_zhang_log_1996 = dmspols_zhang_log[year == 1996],
         dmspols_zhang_ihs_1996 = dmspols_zhang_ihs[year == 1996]) %>%
  ungroup()

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
         dmspols_zhang_ihs_pretnd96_92   = dmspols_zhang_ihs[year == 1996] - dmspols_zhang_ihs[year == 1992]) %>%
  ungroup()

# Select Relevant Variables ----------------------------------------------------
id_vars <- c("cell_id", "year")
adm_vars <- c("W_CODE", "R_CODE", "Z_CODE", "woreda_id")
road_length_vars <- names(data) %>% str_subset("road_length_") %>% str_subset("above")
road_distance_vars <- c("distance_mst")
dist_road_speed_vars <- names(data) %>% str_subset("distance_road_speed_") %>% str_subset("above")
satellite_vars <- names(data) %>% str_subset("viirs|dmspols|ndvi|globcover|temp|precipitation")
MA_vars <- names(data) %>% str_subset("MA_")
other_vars <- c("gpw2000", "area_polygon", "distance_city_addisababa", "Pop2007")

vars_all <- c(id_vars,
              adm_vars,
              road_length_vars,
              road_distance_vars,
              dist_road_speed_vars,
              satellite_vars,
              MA_vars,
              other_vars)

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

