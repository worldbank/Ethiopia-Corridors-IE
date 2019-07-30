# Create Variables for Analysis

dataset <- "points_5percent"

# Load Data --------------------------------------------------------------------
if(dataset == "cluster_all"){
  filename <- "urban_cluster_data"
  data <- readRDS(file.path(project_file_path, "Data", "FinalData", "dmspols_grid_dataset", paste0(filename,".Rds")))
  data$cell_id <- data$cluster_id
}

if(dataset == "points"){
  filename <- "dmspols_level_dataset"
  data <- readRDS(file.path(project_file_path, "Data", "FinalData", "dmspols_grid_dataset", paste0(filename,".Rds")))
}

if(dataset == "points_5percent"){
  filename <- "dmspols_level_dataset_5percentsample"
  data <- readRDS(file.path(project_file_path, "Data", "FinalData", "dmspols_grid_dataset", paste0(filename,".Rds")))
  data$gc_urban_mean <- data$globcover_urban
}

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




