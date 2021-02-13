# Create Varibles for Analysis

# The code implements the following cleaning steps
# 1. Distance to road categories (e.g., min distance to improved road >50km/hr)
# 2. Create (1) years since improved and (2) binary improved variables
# 3. Add grouped, lagged treatment (dummy for 2-5 & 6-10 years before treatment)
# 4. Dependent variable transformations
# 5. Other variable transformations
# 6. Woreda level stats (within woreda + near road; not getting full woreda value)
# 7. Remove variables don't need

# The code is memory intensive. To prevent from crashing, break the datasets into
# different chunks, implementing the code on each chunk, the append together.
# Chunk done by woreda and cell, so that an entire woreda must be contained
# fully within a chunk

#### Parameters
NEAR_CUTOFF <- 5 * 1000     # meters distance for "close to road"
ALL_YEARS_IMPROVED_VAR <- F # add variables indicate 2nd and 3rd year of treatment
CHUNK_SIZE <- 200           # number of woredas in each chunk

# Load Data --------------------------------------------------------------------
data_all <- readRDS(file.path(panel_rsdp_imp_data_file_path, "dmspols_grid_nearroad", "merged_datasets", "panel_data.Rds"))

# Set Up Loop Over Chunks ------------------------------------------------------

## Delete previous temp files
file.path(panel_rsdp_imp_data_file_path, "dmspols_grid_nearroad", "merged_datasets", "temp_datasets") %>%
  list.files(full.names = T) %>%
  lapply(function(file) file.remove(file))

## Determine chunks
cell_id_df <- data_all %>%
  distinct(cell_id, woreda_id)

woreda_ids <- unique(cell_id_df$woreda_id)

start_ids <- seq(from = 1, to = length(woreda_ids), by=CHUNK_SIZE)

for(start_i in start_ids){
  print(paste(start_i, "-", length(start_ids), "-----------------------------"))
  
  ## Subset Data
  end_i <- min(start_i + CHUNK_SIZE - 1, length(woreda_ids))
  woreda_ids_i <- woreda_ids[start_i:end_i]
  
  data <- data_all[data_all$woreda_id %in% woreda_ids_i,]
  
  # Distance to aggregate road categories --------------------------------------
  # We calculate distance to roads by speed limit. Here we calculate distance
  # to any road, road 50 km/hr and above and roads less than 50 km/hr
  
  ## Distance improved road
  data$distance_improvedroad <- apply(data[,paste0("distance_improvedroad_speedafter_",c(20,25,30,35,45,50,70,120))], 1, FUN = min_na)
  data$distance_improvedroad_50aboveafter <- apply(data[,paste0("distance_improvedroad_speedafter_",c(50,70,120))], 1, FUN = min_na)
  data$distance_improvedroad_below50after <- apply(data[,paste0("distance_improvedroad_speedafter_",c(20,25,30,35,45))], 1, FUN = min_na)
  
  data$distance_improvedroad_speedafter_20 <- NULL
  data$distance_improvedroad_speedafter_25 <- NULL
  data$distance_improvedroad_speedafter_30 <- NULL
  data$distance_improvedroad_speedafter_35 <- NULL
  data$distance_improvedroad_speedafter_45 <- NULL
  data$distance_improvedroad_speedafter_50 <- NULL
  data$distance_improvedroad_speedafter_70 <- NULL
  data$distance_improvedroad_speedafter_120 <- NULL
  
  ## Distance road
  data$distance_road <- apply(data[,paste0("distance_road_speed_",c(10,15,20,25,30,35,45,50,70,120))], 1, FUN = min_na)
  data$distance_road_50above <- apply(data[,paste0("distance_road_speed_",c(50,70,120))], 1, FUN = min_na)
  data$distance_road_below50 <- apply(data[,paste0("distance_road_speed_",c(10,15,20,25,30,35,45))], 1, FUN = min_na)
  
  data$distance_road_speed_10 <- NULL
  data$distance_road_speed_15 <- NULL
  data$distance_road_speed_20 <- NULL
  data$distance_road_speed_25 <- NULL
  data$distance_road_speed_30 <- NULL
  data$distance_road_speed_35 <- NULL
  data$distance_road_speed_45 <- NULL
  data$distance_road_speed_50 <- NULL
  data$distance_road_speed_70 <- NULL
  data$distance_road_speed_120 <- NULL
  
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
                              "distance_improvedroad_below50after",
                              "distance_road", 
                              "distance_road_50above", 
                              "distance_road_below50"),
                            generate_road_improved_variables, 
                            data, 
                            ALL_YEARS_IMPROVED_VAR,
                            NEAR_CUTOFF) %>% bind_cols()
  data <- bind_cols(data, roadimproved_df)
  
  # Lagged treatment -----------------------------------------------------------
  data$pre_improvedroad_neg2_5 <- as.numeric(data$years_since_improvedroad %in% -2:-5) %>% as.numeric()
  data$pre_improvedroad_neg6_10 <- as.numeric(data$years_since_improvedroad %in% -6:-10) %>% as.numeric()
  
  data$pre_improvedroad_50aboveafter_neg2_5 <- as.numeric(data$years_since_improvedroad_50aboveafter %in% -2:-5) %>% as.numeric()
  data$pre_improvedroad_50aboveafter_neg6_10 <- as.numeric(data$years_since_improvedroad_50aboveafter %in% -6:-10) %>% as.numeric()
  
  data$pre_improvedroad_below50after_neg2_5 <- as.numeric(data$years_since_improvedroad_below50after %in% -2:-5) %>% as.numeric()
  data$pre_improvedroad_below50after_neg6_10 <- as.numeric(data$years_since_improvedroad_below50after %in% -6:-10) %>% as.numeric()
  
  gc()
  # Variables for treated time 2, 3, etc ---------------------------------------
  if(ALL_YEARS_IMPROVED_VAR){
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
  
  gc()
  
  # Log Variables ----------------------------------------------------------------
  calc_ihs <- function(x) log(x + sqrt(x^2 + 1))

  ntl_var <- data %>% names() %>% str_subset("dmspols|globcover")
  for(var in ntl_var) data[[paste0(var, "_log")]] <- log(data[[var]] + 1)
  for(var in ntl_var) data[[paste0(var, "_ihs")]] <- calc_ihs(data[[var]])
  
  # Dependent Variable Transformations -----------------------------------------
  # Inverse Hyperbolic Since Transformation 
  # This is used by Mitnik et. al. due to lots of zeros in DMSP-OLS 
  calc_ihs <- function(x) log(x + sqrt(x^2 + 1))
  
  data <- data %>%
    
    group_by(cell_id) %>%
    
    # Baseline variables
    mutate(dmspols_1996 = dmspols[year == 1996],
           dmspols_zhang_1996 = dmspols_zhang[year == 1996],
           globcover_urban_1996 = globcover_urban[year == 1996],
           dmspols_zhang_ihs_1996 = dmspols_zhang_ihs[year == 1996]) %>%
    
    ungroup() 
  
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
  
  #### Binary variables for above NTL threshold
  # For woreda/non-grid dataset, this is done in the extraction phase, so this
  # represents the proportion of cells. 
  
  data$dmspols_zhang_2 <- data$dmspols_zhang >= 2
  data$dmspols_zhang_6 <- data$dmspols_zhang >= 6
  
  data$viirs_mean_2 <- data$viirs_mean >= 2
  data$viirs_mean_6 <- data$viirs_mean >= 6
  
  # Other variable transformations ---------------------------------------------
  data$far_addis <- as.numeric(data$distance_city_addisababa >= 100*1000)
  data$distance_city_addisababa <- NULL
  
  ## Check Median NTL Value
  data$dmspols_zhang[data$dmspols_zhang > 0 & data$year %in% 1996] %>% median(na.rm=T)
  
  ## NTL lit at baseline
  data$dmspols_zhang_base0na <- data$dmspols_zhang
  data$dmspols_zhang_base0na[data$dmspols_zhang_1996 %in% 0] <- NA
  
  data$dmspols_zhang_ihs_base0na <- data$dmspols_zhang_ihs
  data$dmspols_zhang_ihs_base0na[data$dmspols_zhang_1996 %in% 0] <- NA
  
  # Geographic Regions ---------------------------------------------------------
  # data$region_type <- ifelse(data$GADM_ID_1 %in% c("Afar", "Benshangul-Gumaz", "Somali"), "Sparse", "Dense") %>% factor(levels=c("Sparse", "Dense"))
  # data$GADM_ID_1 <- NULL
  # 
  # if(grepl("woreda_panel_hdx_csa", DATASET_TYPE)){
  #   data$R_NAME <- data$R_NAME %>% as.character()
  #   data$region_type <- ifelse(data$R_NAME %in% c("Afar", "Benishangul Gumuz", "SOMALI REGION"), "Sparse", "Dense") %>% factor(levels=c("Sparse", "Dense"))
  # } 
  # 
  # gc(); Sys.sleep(.5); gc(); Sys.sleep(.5)
  
  # Woreda stats ---------------------------------------------------------------
  # Taking cells near improved road by woreda
  data <- data %>%
    group_by(woreda_id) %>%
    mutate(dmspols_1996_woreda = mean(dmspols_1996, na.rm = T)) %>%
    ungroup()
  
  ## Nighttime lights groups
  ntl_non0_med <- data$dmspols_1996_woreda[data$dmspols_1996_woreda > 0] %>% median(na.rm=T)
  data$ntl_group <- NA
  data$ntl_group[data$dmspols_1996_woreda <= ntl_non0_med] <- "1"
  data$ntl_group[data$dmspols_1996_woreda > ntl_non0_med] <- "2"
  
  # Remove Stuff Don't Need ----------------------------------------------------
  # Reduces dataset size if grid dataset where need to trim size of dataset
  data$distance_city_popsize_3groups_g1 <- NULL
  data$globcover_urban_log <- NULL
  data$globcover_cropland_log <- NULL
  data$dmspols_log <- NULL
  data$dmspols_zhang <- NULL
  data$viirs_max <- NULL
  data$viirs_mean <- NULL
  data$viirs_mean_2 <- NULL
  data$viirs_mean_6 <- NULL
  data$viirs_median <- NULL
  
  # Export Tmp Data ------------------------------------------------------------
  saveRDS(data, file.path(panel_rsdp_imp_data_file_path, "dmspols_grid_nearroad", "merged_datasets", "temp_datasets", paste0("grid_data_clean_",start_i,".Rds")))
}

# Append Together --------------------------------------------------------------
rm(data)
rm(data_all)
gc(); gc(); gc()

data_append <- file.path(panel_rsdp_imp_data_file_path, "dmspols_grid_nearroad", "merged_datasets", "temp_datasets") %>%
  list.files(full.names = T) %>%
  lapply(function(fpath){
    print(fpath)
    data_i <- readRDS(fpath) %>% data.table()
  }) %>%
  bind_rows() %>%
  as.data.frame()

# Export -----------------------------------------------------------------------
saveRDS(data_append, file.path(panel_rsdp_imp_data_file_path, "dmspols_grid_nearroad", "merged_datasets", "grid_data_clean.Rds"))

