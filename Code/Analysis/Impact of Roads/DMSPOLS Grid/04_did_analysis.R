# Impact of Expressway Expansion
# Ethiopia IE

# Setup ------------------------------------------------------------------------
if(Sys.info()[["user"]] == "r521633") project_file_path <- "/home/wb521633/IEs/Ethiopia IE"
if(Sys.info()[["user"]] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE"
if(Sys.info()[["user"]] == "robmarty") project_file_path <- "~/Dropbox/World Bank/IEs/Ethiopia IE"

tables_file_path <- file.path(project_file_path, "Results", "Tables")
figures_file_path <- file.path(project_file_path, "Results", "Figures")

DIST_THRESH <- 2 #km to be considered near a road

dataset <- "points_5percent"

# Load Data --------------------------------------------------------------------
if(dataset == "cluster_all"){
  data <- readRDS(file.path(project_file_path, "Data", "FinalData", "urban_cluster_dataset", "urban_cluster_data.Rds"))
  data$cell_id <- data$cluster_id
}

if(dataset == "points"){
  data <- readRDS(file.path(project_file_path, "Data", "FinalData", "dmspols_grid_dataset", "dmspols_level_dataset.Rds"))
}

if(dataset == "points_5percent"){
  data <- readRDS(file.path(project_file_path, "Data", "FinalData", "dmspols_grid_dataset", "dmspols_level_dataset_5percentsample.Rds"))
  data$gc_urban_mean <- data$globcover_urban
}

data <- data[(data$year >= 1996) & (data$year <= 2016),]

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
  
  #dplyr::mutate(near_all_1997 = near_all[year == 1997]) %>%
  #dplyr::mutate(near_50above_1997 = near_50above[year == 1997]) %>%
  #dplyr::mutate(near_below50_1997 = near_below50[year == 1997]) %>%
  
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
data$dmspols_zhang_1 <- as.numeric(data$dmspols_zhang > 0)
data$dmspols_zhang_5 <- as.numeric(data$dmspols_zhang >= 5)

# NDVI
data$ndvi_cropland[is.na(data$ndvi_cropland)] <- 0
data$ndvi_cropland[!(data$year %in% 2000:2015)] <- NA
 
data$ndvi_nocropland <- data$ndvi
data$ndvi_nocropland[!(data$globcover_cropland %in% 0)] <- 0
data$ndvi_nocropland[!(data$year %in% 2000:2015)] <- NA


# TESTING
#parallel_trends <- felm(dmspols_zhang ~ years_since_improved_all_bin + years_since_improved_all_group_placebo | cell_id + year | 0 | GADM_ID_3, data=data[data$year %in% 1996:2012,])
#parallel_trends <- felm(dmspols_zhang ~ years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data[data$year %in% 1996:2012,])
#parallel_trends <- felm(dmspols_zhang ~ as.factor(years_since_improved_all) - 1 | cell_id +  year | 0 | GADM_ID_3, data=data[data$year %in% 1996:2012,])
#summary(parallel_trends)

#data$years_since_improved_all_group_placebo %>% table
#data$years_since_improved_all_bin %>% table

# Regressions: Binary Treatment ------------------------------------------------
covariate.labels <- c("Imp. Rd, Any",
                      "Imp. Rd $<$ 50km/hr",
                      "Imp. Rd $\\geq$ 50km/hr",
                      
                      "Imp. Rd, Any $\\times$ Base NTL Zero",
                      "Imp. Rd, Any $\\times$ Base NTL Low",
                      "Imp. Rd, Any $\\times$ Base NTL High",
                      
                      "Imp. Rd $<$ 50km/hr $\\times$ Base NTL Zero",
                      "Imp. Rd $<$ 50km/hr $\\times$ Base NTL Low",
                      "Imp. Rd $<$ 50km/hr $\\times$ Base NTL High",
                      
                      "Imp. Rd $\\geq$ 50km/hr $\\times$ Base NTL Zero",
                      "Imp. Rd $\\geq$ 50km/hr $\\times$ Base NTL Low",
                      "Imp. Rd $\\geq$ 50km/hr $\\times$ Base NTL High")

if(dataset %in% "cluster_belowmedianclustersize"){
  covariate.labels <- c("Imp. Rd, Any",
                        "Imp. Rd $<$ 50km/hr",
                        "Imp. Rd $\\geq$ 50km/hr",
                        
                        "Imp. Rd, Any $\\times$ Base NTL Zero",
                        "Imp. Rd, Any $\\times$ Base NTL Low",
                        
                        "Imp. Rd $<$ 50km/hr $\\times$ Base NTL Zero",
                        "Imp. Rd $<$ 50km/hr $\\times$ Base NTL Low",
                        
                        "Imp. Rd $\\geq$ 50km/hr $\\times$ Base NTL Zero",
                        "Imp. Rd $\\geq$ 50km/hr $\\times$ Base NTL Low")
}

# Points -----------------------------------------------------------------------
if(grepl("points", dataset)){
  
  # Placebo
  felm_allroads_placebo_dmspols_yearfe <- felm(dmspols_zhang ~ years_since_improved_all_placebo_bin | cell_id + year | 0 | GADM_ID_3, data=data)
  felm_allroads_placebo_dmspols_yearlinear <- felm(dmspols_zhang ~ years_since_improved_all_placebo_bin + year | cell_id | 0 | GADM_ID_3, data=data)
  
  felm_allroads_placebo_dmspols1_yearfe <- felm(dmspols_zhang_1 ~ years_since_improved_all_placebo_bin | cell_id + year | 0 | GADM_ID_3, data=data)
  felm_allroads_placebo_dmspols1_yearlinear <- felm(dmspols_zhang_1 ~ years_since_improved_all_placebo_bin + year | cell_id | 0 | GADM_ID_3, data=data)
  
  felm_allroads_placebo_dmspols5_yearfe <- felm(dmspols_zhang_5 ~ years_since_improved_all_placebo_bin | cell_id + year | 0 | GADM_ID_3, data=data)
  felm_allroads_placebo_dmspols5_yearlinear <- felm(dmspols_zhang_5 ~ years_since_improved_all_placebo_bin + year | cell_id | 0 | GADM_ID_3, data=data)
  
  felm_allroads_placebo_urban_yearfe <- felm(gc_urban_mean ~ years_since_improved_all_placebo_bin | cell_id + year | 0 | GADM_ID_3, data=data)
  felm_allroads_placebo_urban_yearlinear <- felm(gc_urban_mean ~ years_since_improved_all_placebo_bin + year | cell_id | 0 | GADM_ID_3, data=data)
  
  felm_allroads_placebo_cropland_yearfe <- felm(globcover_cropland ~ years_since_improved_all_placebo_bin | cell_id + year | 0 | GADM_ID_3, data=data)
  felm_allroads_placebo_cropland_yearlinear <- felm(globcover_cropland ~ years_since_improved_all_placebo_bin + year | cell_id | 0 | GADM_ID_3, data=data)
  
  felm_allroads_placebo_ndvi_yearfe <- felm(ndvi ~ years_since_improved_all_placebo_bin | cell_id + year | 0 | GADM_ID_3, data=data)
  felm_allroads_placebo_ndvi_yearlinear <- felm(ndvi ~ years_since_improved_all_placebo_bin + year | cell_id | 0 | GADM_ID_3, data=data)
  
  felm_allroads_placebo_ndvi_cropland_yearfe <- felm(ndvi_cropland ~ years_since_improved_all_placebo_bin | cell_id + year | 0 | GADM_ID_3, data=data[data$globcover_cropland_2015 > 0,])
  felm_allroads_placebo_ndvi_cropland_yearlinear <- felm(ndvi_cropland ~ years_since_improved_all_placebo_bin + year | cell_id | 0 | GADM_ID_3, data=data[data$globcover_cropland_2015 > 0,])
  
  felm_allroads_placebo_ndvi_nocropland_yearfe <- felm(ndvi_nocropland ~ years_since_improved_all_placebo_bin | cell_id + year | 0 | GADM_ID_3, data=data[data$globcover_cropland_2015 %in% 0,])
  felm_allroads_placebo_ndvi_nocropland_yearlinear <- felm(ndvi_nocropland ~ years_since_improved_all_placebo_bin + year | cell_id | 0 | GADM_ID_3, data=data[data$globcover_cropland_2015 %in% 0,])
  
  # DMSP-OLS Value 
  felm_allroads <- felm(dmspols_zhang ~ years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data)
  felm_allroads_ntlbase <- felm(dmspols_zhang ~ years_since_improved_all_bin*factor(dmspols_1997_group)-factor(dmspols_1997_group)-years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data) 
  
  felm_blabv50roads <- felm(dmspols_zhang ~ years_since_improved_below50_bin + years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin),])
  felm_blabv50roads_ntlbase <- felm(dmspols_zhang ~ years_since_improved_below50_bin*factor(dmspols_1997_group) + 
                                      years_since_improved_50above_bin*factor(dmspols_1997_group) -
                                      factor(dmspols_1997_group) -
                                      years_since_improved_below50_bin - 
                                      years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin),])  
  
  # DMSP-OLS > 1
  felm_allroads_dmspols1 <- felm(dmspols_zhang_1 ~ years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data)
  felm_allroads_ntlbase_dmspols1 <- felm(dmspols_zhang_1 ~ years_since_improved_all_bin*factor(dmspols_1997_group)-factor(dmspols_1997_group)-years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data) 
  
  felm_blabv50roads_dmspols1 <- felm(dmspols_zhang_1 ~ years_since_improved_below50_bin + years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin),])
  felm_blabv50roads_ntlbase_dmspols1 <- felm(dmspols_zhang_1 ~ years_since_improved_below50_bin*factor(dmspols_1997_group) + 
                                            years_since_improved_50above_bin*factor(dmspols_1997_group) -
                                            factor(dmspols_1997_group) -
                                            years_since_improved_below50_bin - 
                                            years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin),])  
  
  # DMSP-OLS > 5
  felm_allroads_dmspols5 <- felm(dmspols_zhang_5 ~ years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data)
  felm_allroads_ntlbase_dmspols5 <- felm(dmspols_zhang_5 ~ years_since_improved_all_bin*factor(dmspols_1997_group)-factor(dmspols_1997_group)-years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data) 
  
  felm_blabv50roads_dmspols5 <- felm(dmspols_zhang_5 ~ years_since_improved_below50_bin + years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin),])
  felm_blabv50roads_ntlbase_dmspols5 <- felm(dmspols_zhang_5 ~ years_since_improved_below50_bin*factor(dmspols_1997_group) + 
                                               years_since_improved_50above_bin*factor(dmspols_1997_group) -
                                               factor(dmspols_1997_group) -
                                               years_since_improved_below50_bin - 
                                               years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin),])  
  
  # Urban
  felm_allroads_urban <- felm(gc_urban_mean ~ years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data)
  felm_allroads_ntlbase_urban <- felm(gc_urban_mean ~ years_since_improved_all_bin*factor(dmspols_1997_group)-factor(dmspols_1997_group)-years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data) 
  
  felm_blabv50roads_urban <- felm(gc_urban_mean ~ years_since_improved_below50_bin + years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin),])
  felm_blabv50roads_ntlbase_urban <- felm(gc_urban_mean ~ years_since_improved_below50_bin*factor(dmspols_1997_group) + 
                                           years_since_improved_50above_bin*factor(dmspols_1997_group) -
                                           factor(dmspols_1997_group) -
                                           years_since_improved_below50_bin - 
                                           years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin),])  
  
  # Cropland
  felm_allroads_cropland <- felm(globcover_cropland ~ years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data)
  felm_allroads_ntlbase_cropland <- felm(globcover_cropland ~ years_since_improved_all_bin*factor(dmspols_1997_group)-factor(dmspols_1997_group)-years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data) 
  
  felm_blabv50roads_cropland <- felm(globcover_cropland ~ years_since_improved_below50_bin + years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin),])
  felm_blabv50roads_ntlbase_cropland <- felm(globcover_cropland ~ years_since_improved_below50_bin*factor(dmspols_1997_group) + 
                                            years_since_improved_50above_bin*factor(dmspols_1997_group) -
                                            factor(dmspols_1997_group) -
                                            years_since_improved_below50_bin - 
                                            years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin),])  

  # Cropland - Irrigates
  felm_allroads_cropland_irrigated <- felm(globcover_cropland_irrigated ~ years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data)
  felm_allroads_ntlbase_cropland_irrigated <- felm(globcover_cropland_irrigated ~ years_since_improved_all_bin*factor(dmspols_1997_group)-factor(dmspols_1997_group)-years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data) 
  
  felm_blabv50roads_cropland_irrigated <- felm(globcover_cropland_irrigated ~ years_since_improved_below50_bin + years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin),])
  felm_blabv50roads_ntlbase_cropland_irrigated <- felm(globcover_cropland_irrigated ~ years_since_improved_below50_bin*factor(dmspols_1997_group) + 
                                               years_since_improved_50above_bin*factor(dmspols_1997_group) -
                                               factor(dmspols_1997_group) -
                                               years_since_improved_below50_bin - 
                                               years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin),])  
  
  # Cropland - Rainfed
  felm_allroads_cropland_rainfed <- felm(globcover_cropland_rainfed ~ years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data)
  felm_allroads_ntlbase_cropland_rainfed <- felm(globcover_cropland_rainfed ~ years_since_improved_all_bin*factor(dmspols_1997_group)-factor(dmspols_1997_group)-years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data) 
  
  felm_blabv50roads_cropland_rainfed <- felm(globcover_cropland_rainfed ~ years_since_improved_below50_bin + years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin),])
  felm_blabv50roads_ntlbase_cropland_rainfed <- felm(globcover_cropland_rainfed ~ years_since_improved_below50_bin*factor(dmspols_1997_group) + 
                                                         years_since_improved_50above_bin*factor(dmspols_1997_group) -
                                                         factor(dmspols_1997_group) -
                                                         years_since_improved_below50_bin - 
                                                         years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin),])  
  
  # Cropland - Mosaic
  felm_allroads_cropland_mosaic <- felm(globcover_cropland_mosaic ~ years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data)
  felm_allroads_ntlbase_cropland_mosaic <- felm(globcover_cropland_mosaic ~ years_since_improved_all_bin*factor(dmspols_1997_group)-factor(dmspols_1997_group)-years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data) 
  
  felm_blabv50roads_cropland_mosaic <- felm(globcover_cropland_mosaic ~ years_since_improved_below50_bin + years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin),])
  felm_blabv50roads_ntlbase_cropland_mosaic <- felm(globcover_cropland_mosaic ~ years_since_improved_below50_bin*factor(dmspols_1997_group) + 
                                                       years_since_improved_50above_bin*factor(dmspols_1997_group) -
                                                       factor(dmspols_1997_group) -
                                                       years_since_improved_below50_bin - 
                                                       years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin),])  
  
  # NDVI
  felm_allroads_ndvi <- felm(ndvi ~ years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data)
  felm_allroads_ntlbase_ndvi <- felm(ndvi ~ years_since_improved_all_bin*factor(dmspols_1997_group)-factor(dmspols_1997_group)-years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data) 
  
  felm_blabv50roads_ndvi <- felm(ndvi ~ years_since_improved_below50_bin + years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin),])
  felm_blabv50roads_ntlbase_ndvi <- felm(ndvi ~ years_since_improved_below50_bin*factor(dmspols_1997_group) + 
                                                      years_since_improved_50above_bin*factor(dmspols_1997_group) -
                                                      factor(dmspols_1997_group) -
                                                      years_since_improved_below50_bin - 
                                                      years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin),])  
  
  # NDVI - Cropland
  felm_allroads_ndvi_cropland <- felm(ndvi_cropland ~ years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data[data$globcover_cropland_2015 > 0,])
  felm_allroads_ntlbase_ndvi_cropland <- felm(ndvi_cropland ~ years_since_improved_all_bin*factor(dmspols_1997_group)-factor(dmspols_1997_group)-years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data[data$globcover_cropland_2015 > 0,]) 
  
  felm_blabv50roads_ndvi_cropland <- felm(ndvi_cropland ~ years_since_improved_below50_bin + years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin) & data$globcover_cropland_2015 > 0,])
  felm_blabv50roads_ntlbase_ndvi_cropland <- felm(ndvi_cropland ~ years_since_improved_below50_bin*factor(dmspols_1997_group) + 
                                                      years_since_improved_50above_bin*factor(dmspols_1997_group) -
                                                      factor(dmspols_1997_group) -
                                                      years_since_improved_below50_bin - 
                                                      years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin) & data$globcover_cropland_2015 > 0,])  
  
  # NDVI - No Cropland
  felm_allroads_ndvi_nocropland <- felm(ndvi_nocropland ~ years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data[data$globcover_cropland_2015 %in% 0,])
  felm_allroads_ntlbase_ndvi_nocropland <- felm(ndvi_nocropland ~ years_since_improved_all_bin*factor(dmspols_1997_group)-factor(dmspols_1997_group)-years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data[data$globcover_cropland_2015 %in% 0,]) 
  
  felm_blabv50roads_ndvi_nocropland <- felm(ndvi_nocropland ~ years_since_improved_below50_bin + years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin) & data$globcover_cropland_2015 %in% 0,])
  felm_blabv50roads_ntlbase_ndvi_nocropland <- felm(ndvi_nocropland ~ years_since_improved_below50_bin*factor(dmspols_1997_group) + 
                                                    years_since_improved_50above_bin*factor(dmspols_1997_group) -
                                                    factor(dmspols_1997_group) -
                                                    years_since_improved_below50_bin - 
                                                    years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin) & data$globcover_cropland_2015 %in% 0,])  
  
  
  
  ## Time Since Treatment
  felm_allroads_yearssince_dmspols <- felm(dmspols_zhang ~ factor(years_since_improved_all_group) | cell_id + year | 0 | GADM_ID_3, data=data)
  felm_allroads_blabv50roads_dmspols <- felm(dmspols_zhang ~ factor(years_since_improved_below50_group) + factor(years_since_improved_50above_group) | cell_id + year | 0 | GADM_ID_3, data=data)
  
  felm_allroads_yearssince_dmspols1 <- felm(dmspols_zhang_1 ~ factor(years_since_improved_all_group) | cell_id + year | 0 | GADM_ID_3, data=data)
  felm_allroads_blabv50roads_dmspols1 <- felm(dmspols_zhang_1 ~ factor(years_since_improved_below50_group) + factor(years_since_improved_50above_group) | cell_id + year | 0 | GADM_ID_3, data=data)
  
  felm_allroads_yearssince_dmspols5 <- felm(dmspols_zhang_5 ~ factor(years_since_improved_all_group) | cell_id + year | 0 | GADM_ID_3, data=data)
  felm_allroads_blabv50roads_dmspols5 <- felm(dmspols_zhang_5 ~ factor(years_since_improved_below50_group) + factor(years_since_improved_50above_group) | cell_id + year | 0 | GADM_ID_3, data=data)
  
  felm_allroads_yearssince_urban <- felm(gc_urban_mean ~ factor(years_since_improved_all_group) | cell_id + year | 0 | GADM_ID_3, data=data)
  felm_allroads_blabv50roads_urban <- felm(gc_urban_mean ~ factor(years_since_improved_below50_group) + factor(years_since_improved_50above_group) | cell_id + year | 0 | GADM_ID_3, data=data)

  felm_allroads_yearssince_cropland <- felm(globcover_cropland ~ factor(years_since_improved_all_group) | cell_id + year | 0 | GADM_ID_3, data=data)
  felm_allroads_blabv50roads_cropland <- felm(globcover_cropland ~ factor(years_since_improved_below50_group) + factor(years_since_improved_50above_group) | cell_id + year | 0 | GADM_ID_3, data=data)
  
  felm_allroads_yearssince_cropland_irrigated <- felm(globcover_cropland_irrigated ~ factor(years_since_improved_all_group) | cell_id + year | 0 | GADM_ID_3, data=data)
  felm_allroads_blabv50roads_cropland_irrigated <- felm(globcover_cropland_irrigated ~ factor(years_since_improved_below50_group) + factor(years_since_improved_50above_group) | cell_id + year | 0 | GADM_ID_3, data=data)
  
  felm_allroads_yearssince_cropland_rainfed <- felm(globcover_cropland_rainfed ~ factor(years_since_improved_all_group) | cell_id + year | 0 | GADM_ID_3, data=data)
  felm_allroads_blabv50roads_cropland_rainfed <- felm(globcover_cropland_rainfed ~ factor(years_since_improved_below50_group) + factor(years_since_improved_50above_group) | cell_id + year | 0 | GADM_ID_3, data=data)
  
  felm_allroads_yearssince_cropland_mosaic <- felm(globcover_cropland_mosaic ~ factor(years_since_improved_all_group) | cell_id + year | 0 | GADM_ID_3, data=data)
  felm_allroads_blabv50roads_cropland_mosaic <- felm(globcover_cropland_mosaic ~ factor(years_since_improved_below50_group) + factor(years_since_improved_50above_group) | cell_id + year | 0 | GADM_ID_3, data=data)
  
  felm_allroads_yearssince_ndvi <- felm(ndvi ~ factor(years_since_improved_all_group) | cell_id + year | 0 | GADM_ID_3, data=data)
  felm_allroads_blabv50roads_ndvi <- felm(ndvi ~ factor(years_since_improved_below50_group) + factor(years_since_improved_50above_group) | cell_id + year | 0 | GADM_ID_3, data=data)
  
  felm_allroads_yearssince_ndvi_cropland <- felm(ndvi_cropland ~ factor(years_since_improved_all_group) | cell_id + year | 0 | GADM_ID_3, data=data[data$globcover_cropland_2015 > 0,])
  felm_allroads_blabv50roads_ndvi_cropland <- felm(ndvi_cropland ~ factor(years_since_improved_below50_group) + factor(years_since_improved_50above_group) | cell_id + year | 0 | GADM_ID_3, data=data[data$globcover_cropland_2015 > 0,])
  
  felm_allroads_yearssince_ndvi_nocropland <- felm(ndvi_nocropland ~ factor(years_since_improved_all_group) | cell_id + year | 0 | GADM_ID_3, data=data[data$globcover_cropland_2015 %in% 0,])
  felm_allroads_blabv50roads_ndvi_nocropland <- felm(ndvi_nocropland ~ factor(years_since_improved_below50_group) + factor(years_since_improved_50above_group) | cell_id + year | 0 | GADM_ID_3, data=data[data$globcover_cropland_2015 %in% 0,])
  
  stargazer(felm_allroads_cropland,
            felm_blabv50roads_cropland,
            felm_allroads_ntlbase_cropland,
            felm_blabv50roads_ntlbase_cropland,

            felm_allroads_cropland_rainfed,
            felm_blabv50roads_cropland_rainfed,
            felm_allroads_ntlbase_cropland_rainfed,
            felm_blabv50roads_ntlbase_cropland_rainfed,
            
            felm_allroads_cropland_irrigated,
            felm_blabv50roads_cropland_irrigated,
            felm_allroads_ntlbase_cropland_irrigated,
            felm_blabv50roads_ntlbase_cropland_irrigated,
            
            felm_allroads_cropland_mosaic,
            felm_blabv50roads_cropland_mosaic,
            felm_allroads_ntlbase_cropland_mosaic,
            felm_blabv50roads_ntlbase_cropland_mosaic,
            dep.var.labels.include = T,
            dep.var.labels = c("Crop","Crop: Irrig","Crop: Rain","Crop: Mosaic"),
            dep.var.caption = "",
            covariate.labels = covariate.labels,
            omit.stat = c("f","ser"),
            align=TRUE,
            no.space=TRUE,
            float=FALSE,
            column.sep.width="-15pt",
            digits=2,
            add.lines = list(
              c("Cell FE", rep("Y", 16)),
              c("Year FE", rep("Y", 16))
            ),
            out = file.path(tables_file_path, paste0("eventstudy_results_crop",
                                                     "_dataset",dataset,
                                                     "_distthresh",DIST_THRESH,
                                                     ".tex")))
  
  stargazer(felm_allroads_ndvi,
            felm_blabv50roads_ndvi,
            felm_allroads_ntlbase_ndvi,
            felm_blabv50roads_ntlbase_ndvi,
 
            felm_allroads_ndvi_cropland,
            felm_blabv50roads_ndvi_cropland,
            felm_allroads_ntlbase_ndvi_cropland,
            felm_blabv50roads_ntlbase_ndvi_cropland,
            
            felm_allroads_ndvi_nocropland,
            felm_blabv50roads_ndvi_nocropland,
            felm_allroads_ntlbase_ndvi_nocropland,
            felm_blabv50roads_ntlbase_ndvi_nocropland,

            dep.var.labels.include = T,
            dep.var.labels = c("NDVI","NDVI: Crop", "NDVI: No Crop"),
            dep.var.caption = "",
            covariate.labels = covariate.labels,
            omit.stat = c("f","ser"),
            align=TRUE,
            no.space=TRUE,
            float=FALSE,
            column.sep.width="-15pt",
            digits=2,
            add.lines = list(
              c("Cell FE", rep("Y", 12)),
              c("Year FE", rep("Y", 12))
            ),
            out = file.path(tables_file_path, paste0("eventstudy_results_ndvi",
                                                     "_dataset",dataset,
                                                     "_distthresh",DIST_THRESH,
                                                     ".tex")))
  

  stargazer(felm_allroads_placebo_dmspols_yearfe,
            felm_allroads_placebo_dmspols_yearlinear,
            felm_allroads_placebo_dmspols1_yearfe,
            felm_allroads_placebo_dmspols1_yearlinear,
            felm_allroads_placebo_dmspols5_yearfe,
            felm_allroads_placebo_dmspols5_yearlinear,
            felm_allroads_placebo_urban_yearfe,
            felm_allroads_placebo_urban_yearlinear,
            
            felm_allroads_placebo_cropland_yearfe,
            felm_allroads_placebo_cropland_yearlinear,
            
            felm_allroads_placebo_ndvi_yearfe,
            felm_allroads_placebo_ndvi_yearlinear,
            
            felm_allroads_placebo_ndvi_cropland_yearfe,
            felm_allroads_placebo_ndvi_cropland_yearlinear,
            
            dep.var.labels.include = T,
            dep.var.labels = c("DMSP-OLS","DMSP-OLS $>$ 0","DMSP-OLS $>$ 5","Proportion Urban",
                               "Crop", "NDVI", "NDVI: Crop"),
            dep.var.caption = "",
            covariate.labels = c("Imp. Rd, Any", "Year"),
            omit.stat = c("f","ser"),
            align=TRUE,
            no.space=TRUE,
            float=FALSE,
            column.sep.width="-1pt",
            digits=2,
            add.lines = list(
              c("Cell FE", rep("Y",14)),
              c("Year FE", rep(c("Y","N"), 7))
            ),
            out = file.path(tables_file_path, paste0("eventstudy_results_placebo",
                                                     "_dataset",dataset,
                                                     "_distthresh",DIST_THRESH,
                                                     ".tex")))


  stargazer(felm_allroads,
            felm_blabv50roads,
            felm_allroads_ntlbase,
            felm_blabv50roads_ntlbase,
            felm_allroads_dmspols1,
            felm_blabv50roads_dmspols1,
            felm_allroads_ntlbase_dmspols1,
            felm_blabv50roads_ntlbase_dmspols1,
            felm_allroads_dmspols5,
            felm_blabv50roads_dmspols5,
            felm_allroads_ntlbase_dmspols5,
            felm_blabv50roads_ntlbase_dmspols5,
            felm_allroads_urban,
            felm_blabv50roads_urban,
            felm_allroads_ntlbase_urban,
            felm_blabv50roads_ntlbase_urban,
            dep.var.labels.include = T,
            dep.var.labels = c("DMSP-OLS","DMSP-OLS $>$ 0","DMSP-OLS $>$ 5","Proportion Urban"),
            dep.var.caption = "",
            covariate.labels = covariate.labels,
            omit.stat = c("f","ser"),
            align=TRUE,
            no.space=TRUE,
            float=FALSE,
            column.sep.width="-15pt",
            digits=2,
            add.lines = list(
              c("Cell FE", rep("Y", 16)),
              c("Year FE", rep("Y", 16))
            ),
            out = file.path(tables_file_path, paste0("eventstudy_results",
                                                     "_dataset",dataset,
                                                     "_distthresh",DIST_THRESH,
                                                     ".tex")))
  
  
  stargazer(felm_allroads_yearssince_dmspols,
            felm_allroads_blabv50roads_dmspols,
            felm_allroads_yearssince_dmspols1,
            felm_allroads_blabv50roads_dmspols1,
            felm_allroads_yearssince_dmspols5,
            felm_allroads_blabv50roads_dmspols5,
            felm_allroads_yearssince_urban,
            felm_allroads_blabv50roads_urban,
            dep.var.labels.include = T,
            dep.var.labels = c("DMSP-OLS","DMSP-OLS $>$ 0","DMSP-OLS $>$ 5","Prop. Urban"),
            dep.var.caption = "",
            covariate.labels = c("Imp Rd, Any: Yr 0",
                                 "Imp Rd, Any: Yr 1",
                                 "Imp Rd, Any: Yr 2",
                                 "Imp Rd, Any: Yr 3",
                                 "Imp Rd, Any: Yr $\\geq$ 4",
                                 "Imp. Rd $<$ 50km/hr: Yr 0",
                                 "Imp. Rd $<$ 50km/hr: Yr 1",
                                 "Imp. Rd $<$ 50km/hr: Yr 2",
                                 "Imp. Rd $<$ 50km/hr: Yr 3",
                                 "Imp. Rd $<$ 50km/hr: Yr $\\geq$ 4",
                                 "Imp. Rd $\\geq$ 50km/hr: Yr 0",
                                 "Imp. Rd $\\geq$ 50km/hr: Yr 1",
                                 "Imp. Rd $\\geq$ 50km/hr: Yr 2",
                                 "Imp. Rd $\\geq$ 50km/hr: Yr 3",
                                 "Imp. Rd $\\geq$ 50km/hr: Yr $\\geq$ 4"),
            omit.stat = c("f","ser"),
            align=TRUE,
            no.space=TRUE,
            float=FALSE,
            column.sep.width="-1pt",
            digits=2,
            add.lines = list(
              c("Cell FE", rep("Y", 8)),
              c("Year FE", rep("Y", 8))
            ),
            out = file.path(tables_file_path, paste0("eventstudy_results_yearssince",
                                                     "_dataset",dataset,
                                                     "_distthresh",DIST_THRESH,
                                                     ".tex")))
  
  stargazer(felm_allroads_yearssince_cropland,
            felm_allroads_blabv50roads_cropland,
            felm_allroads_yearssince_cropland_irrigated,
            felm_allroads_blabv50roads_cropland_irrigated,
            felm_allroads_yearssince_cropland_rainfed,
            felm_allroads_blabv50roads_cropland_rainfed,
            felm_allroads_yearssince_cropland_mosaic,
            felm_allroads_blabv50roads_cropland_mosaic,
            felm_allroads_yearssince_ndvi,
            felm_allroads_blabv50roads_ndvi,
            felm_allroads_yearssince_ndvi_cropland,
            felm_allroads_blabv50roads_ndvi_cropland,
            felm_allroads_yearssince_ndvi_nocropland,
            felm_allroads_blabv50roads_ndvi_nocropland,
            dep.var.labels.include = T,
            dep.var.labels = c("Crop","Crop: Irrig","Crop: Rain","Crop: Mosaic", "NDVI", "NDVI: Crop", "NDVI: No Crop"),
            dep.var.caption = "",
            covariate.labels = c("Imp Rd, Any: Yr 0",
                                 "Imp Rd, Any: Yr 1",
                                 "Imp Rd, Any: Yr 2",
                                 "Imp Rd, Any: Yr 3",
                                 "Imp Rd, Any: Yr $\\geq$ 4",
                                 "Imp. Rd $<$ 50km/hr: Yr 0",
                                 "Imp. Rd $<$ 50km/hr: Yr 1",
                                 "Imp. Rd $<$ 50km/hr: Yr 2",
                                 "Imp. Rd $<$ 50km/hr: Yr 3",
                                 "Imp. Rd $<$ 50km/hr: Yr $\\geq$ 4",
                                 "Imp. Rd $\\geq$ 50km/hr: Yr 0",
                                 "Imp. Rd $\\geq$ 50km/hr: Yr 1",
                                 "Imp. Rd $\\geq$ 50km/hr: Yr 2",
                                 "Imp. Rd $\\geq$ 50km/hr: Yr 3",
                                 "Imp. Rd $\\geq$ 50km/hr: Yr $\\geq$ 4"),
            omit.stat = c("f","ser"),
            align=TRUE,
            no.space=TRUE,
            float=FALSE,
            column.sep.width="-1pt",
            digits=2,
            add.lines = list(
              c("Cell FE", rep("Y", 14)),
              c("Year FE", rep("Y", 14))
            ),
            out = file.path(tables_file_path, paste0("eventstudy_results_yearssince_crop",
                                                     "_dataset",dataset,
                                                     "_distthresh",DIST_THRESH,
                                                     ".tex")))
  
  
  
  
}

# Clusters ---------------------------------------------------------------------
if(grepl("cluster", dataset)){
  
  # Full Sample ----------------------------------------------------------------
  
  # DMSP-OLS: Mean 
  felm_allroads_dmsp_mean <- felm(dmspols_zhang ~ years_since_improved_all_bin | cell_id + year | 0 | 0, data=data)
  felm_allroads_ntlbase_dmsp_mean <- felm(dmspols_zhang ~ years_since_improved_all_bin*factor(dmspols_1997_group)-factor(dmspols_1997_group)-years_since_improved_all_bin | cell_id + year | 0 | 0, data=data) 
  
  felm_blabv50roads_dmsp_mean <- felm(dmspols_zhang ~ years_since_improved_below50_bin + years_since_improved_50above_bin | cell_id + year | 0 | 0, data=data[!is.na(data$years_since_improved_all_bin),])
  felm_blabv50roads_ntlbase_dmsp_mean <- felm(dmspols_zhang ~ years_since_improved_below50_bin*factor(dmspols_1997_group) + 
                                      years_since_improved_50above_bin*factor(dmspols_1997_group) -
                                      factor(dmspols_1997_group) -
                                      years_since_improved_below50_bin - 
                                      years_since_improved_50above_bin | cell_id + year | 0 | 0, data=data[!is.na(data$years_since_improved_all_bin),])  
  
  # DMSP-OLS: Max 
  felm_allroads_dmsp_max <- felm(dmspols_zhang_max ~ years_since_improved_all_bin | cell_id + year | 0 | 0, data=data)
  felm_allroads_ntlbase_dmsp_max <- felm(dmspols_zhang_max ~ years_since_improved_all_bin*factor(dmspols_1997_group)-factor(dmspols_1997_group)-years_since_improved_all_bin | cell_id + year | 0 | 0, data=data) 
  
  felm_blabv50roads_dmsp_max <- felm(dmspols_zhang_max ~ years_since_improved_below50_bin + years_since_improved_50above_bin | cell_id + year | 0 | 0, data=data[!is.na(data$years_since_improved_all_bin),])
  felm_blabv50roads_ntlbase_dmsp_max <- felm(dmspols_zhang_max ~ years_since_improved_below50_bin*factor(dmspols_1997_group) + 
                                      years_since_improved_50above_bin*factor(dmspols_1997_group) -
                                      factor(dmspols_1997_group) -
                                      years_since_improved_below50_bin - 
                                      years_since_improved_50above_bin | cell_id + year | 0 | 0, data=data[!is.na(data$years_since_improved_all_bin),])  
  
  # DMSP-OLS: > 0 
  felm_allroads_dmspols1 <- felm(dmspols_zhang_1_above ~ years_since_improved_all_bin | cell_id + year | 0 | 0, data=data)
  felm_allroads_ntlbase_dmspols1 <- felm(dmspols_zhang_1_above ~ years_since_improved_all_bin*factor(dmspols_1997_group)-factor(dmspols_1997_group)-years_since_improved_all_bin | cell_id + year | 0 | 0, data=data) 
  
  felm_blabv50roads_dmspols1 <- felm(dmspols_zhang_1_above ~ years_since_improved_below50_bin + years_since_improved_50above_bin | cell_id + year | 0 | 0, data=data[!is.na(data$years_since_improved_all_bin),])
  felm_blabv50roads_ntlbase_dmspols1 <- felm(dmspols_zhang_1_above ~ years_since_improved_below50_bin*factor(dmspols_1997_group) + 
                                                years_since_improved_50above_bin*factor(dmspols_1997_group) -
                                                factor(dmspols_1997_group) -
                                                years_since_improved_below50_bin - 
                                                years_since_improved_50above_bin | cell_id + year | 0 | 0, data=data[!is.na(data$years_since_improved_all_bin),])  
  
  # DMSP-OLS: > 5 
  felm_allroads_dmspols5 <- felm(dmspols_zhang_5_above ~ years_since_improved_all_bin | cell_id + year | 0 | 0, data=data)
  felm_allroads_ntlbase_dmspols5 <- felm(dmspols_zhang_5_above ~ years_since_improved_all_bin*factor(dmspols_1997_group)-factor(dmspols_1997_group)-years_since_improved_all_bin | cell_id + year | 0 | 0, data=data) 
  
  felm_blabv50roads_dmspols5 <- felm(dmspols_zhang_5_above ~ years_since_improved_below50_bin + years_since_improved_50above_bin | cell_id + year | 0 | 0, data=data[!is.na(data$years_since_improved_all_bin),])
  felm_blabv50roads_ntlbase_dmspols5 <- felm(dmspols_zhang_5_above ~ years_since_improved_below50_bin*factor(dmspols_1997_group) + 
                                               years_since_improved_50above_bin*factor(dmspols_1997_group) -
                                               factor(dmspols_1997_group) -
                                               years_since_improved_below50_bin - 
                                               years_since_improved_50above_bin | cell_id + year | 0 | 0, data=data[!is.na(data$years_since_improved_all_bin),])  
  
  
  # Urban
  felm_allroads_urban <- felm(gc_urban_mean ~ years_since_improved_all_bin | cell_id + year | 0 | 0, data=data)
  felm_allroads_ntlbase_urban <- felm(gc_urban_mean ~ years_since_improved_all_bin*factor(dmspols_1997_group)-factor(dmspols_1997_group)-years_since_improved_all_bin | cell_id + year | 0 | 0, data=data) 
  
  felm_blabv50roads_urban <- felm(gc_urban_mean ~ years_since_improved_below50_bin + years_since_improved_50above_bin | cell_id + year | 0 | 0, data=data[!is.na(data$years_since_improved_all_bin),])
  felm_blabv50roads_ntlbase_urban <- felm(gc_urban_mean ~ years_since_improved_below50_bin*factor(dmspols_1997_group) + 
                                            years_since_improved_50above_bin*factor(dmspols_1997_group) -
                                            factor(dmspols_1997_group) -
                                            years_since_improved_below50_bin - 
                                            years_since_improved_50above_bin | cell_id + year | 0 | 0, data=data[!is.na(data$years_since_improved_all_bin),])  
  
  # Small Clusters -------------------------------------------------------------
  
  # DMSP-OLS: Mean 
  felm_allroads_dmsp_mean_small <- felm(dmspols_zhang ~ years_since_improved_all_bin | cell_id + year | 0 | 0, data=data[data$cluster_n_cells <= 20,])
  felm_allroads_ntlbase_dmsp_mean_small <- felm(dmspols_zhang ~ years_since_improved_all_bin*factor(dmspols_1997_group)-factor(dmspols_1997_group)-years_since_improved_all_bin | cell_id + year | 0 | 0, data=data[data$cluster_n_cells <= 20,]) 
  
  felm_blabv50roads_dmsp_mean_small <- felm(dmspols_zhang ~ years_since_improved_below50_bin + years_since_improved_50above_bin | cell_id + year | 0 | 0, data=data[!is.na(data$years_since_improved_all_bin) & data$cluster_n_cells <= 20,])
  felm_blabv50roads_ntlbase_dmsp_mean_small <- felm(dmspols_zhang ~ years_since_improved_below50_bin*factor(dmspols_1997_group) + 
                                                years_since_improved_50above_bin*factor(dmspols_1997_group) -
                                                factor(dmspols_1997_group) -
                                                years_since_improved_below50_bin - 
                                                years_since_improved_50above_bin | cell_id + year | 0 | 0, data=data[!is.na(data$years_since_improved_all_bin) & data$cluster_n_cells <= 20,])  
  
  # DMSP-OLS: Max 
  felm_allroads_dmsp_max_small <- felm(dmspols_zhang_max ~ years_since_improved_all_bin | cell_id + year | 0 | 0, data=data[data$cluster_n_cells <= 20,])
  felm_allroads_ntlbase_dmsp_max_small <- felm(dmspols_zhang_max ~ years_since_improved_all_bin*factor(dmspols_1997_group)-factor(dmspols_1997_group)-years_since_improved_all_bin | cell_id + year | 0 | 0, data=data[data$cluster_n_cells <= 20,]) 
  
  felm_blabv50roads_dmsp_max_small <- felm(dmspols_zhang_max ~ years_since_improved_below50_bin + years_since_improved_50above_bin | cell_id + year | 0 | 0, data=data[!is.na(data$years_since_improved_all_bin) & data$cluster_n_cells <= 20,])
  felm_blabv50roads_ntlbase_dmsp_max_small <- felm(dmspols_zhang_max ~ years_since_improved_below50_bin*factor(dmspols_1997_group) + 
                                               years_since_improved_50above_bin*factor(dmspols_1997_group) -
                                               factor(dmspols_1997_group) -
                                               years_since_improved_below50_bin - 
                                               years_since_improved_50above_bin | cell_id + year | 0 | 0, data=data[!is.na(data$years_since_improved_all_bin) & data$cluster_n_cells <= 20,])  
  
  # DMSP-OLS: > 0 
  felm_allroads_dmspols1_small <- felm(dmspols_zhang_1_above ~ years_since_improved_all_bin | cell_id + year | 0 | 0, data=data[data$cluster_n_cells <= 20,])
  felm_allroads_ntlbase_dmspols1_small <- felm(dmspols_zhang_1_above ~ years_since_improved_all_bin*factor(dmspols_1997_group)-factor(dmspols_1997_group)-years_since_improved_all_bin | cell_id + year | 0 | 0, data=data[data$cluster_n_cells <= 20,]) 
  
  felm_blabv50roads_dmspols1_small <- felm(dmspols_zhang_1_above ~ years_since_improved_below50_bin + years_since_improved_50above_bin | cell_id + year | 0 | 0, data=data[!is.na(data$years_since_improved_all_bin) & data$cluster_n_cells <= 20,])
  felm_blabv50roads_ntlbase_dmspols1_small <- felm(dmspols_zhang_1_above ~ years_since_improved_below50_bin*factor(dmspols_1997_group) + 
                                               years_since_improved_50above_bin*factor(dmspols_1997_group) -
                                               factor(dmspols_1997_group) -
                                               years_since_improved_below50_bin - 
                                               years_since_improved_50above_bin | cell_id + year | 0 | 0, data=data[!is.na(data$years_since_improved_all_bin) & data$cluster_n_cells <= 20,])  
  
  # DMSP-OLS: > 5 
  felm_allroads_dmspols5_small <- felm(dmspols_zhang_5_above ~ years_since_improved_all_bin | cell_id + year | 0 | 0, data=data[data$cluster_n_cells <= 20,])
  felm_allroads_ntlbase_dmspols5_small <- felm(dmspols_zhang_5_above ~ years_since_improved_all_bin*factor(dmspols_1997_group)-factor(dmspols_1997_group)-years_since_improved_all_bin | cell_id + year | 0 | 0, data=data[data$cluster_n_cells <= 20,]) 
  
  felm_blabv50roads_dmspols5_small <- felm(dmspols_zhang_5_above ~ years_since_improved_below50_bin + years_since_improved_50above_bin | cell_id + year | 0 | 0, data=data[!is.na(data$years_since_improved_all_bin) & data$cluster_n_cells <= 20,])
  felm_blabv50roads_ntlbase_dmspols5_small <- felm(dmspols_zhang_5_above ~ years_since_improved_below50_bin*factor(dmspols_1997_group) + 
                                               years_since_improved_50above_bin*factor(dmspols_1997_group) -
                                               factor(dmspols_1997_group) -
                                               years_since_improved_below50_bin - 
                                               years_since_improved_50above_bin | cell_id + year | 0 | 0, data=data[!is.na(data$years_since_improved_all_bin) & data$cluster_n_cells <= 20,])  
  
  
  
  # Urban
  felm_allroads_urban_small <- felm(gc_urban_mean ~ years_since_improved_all_bin | cell_id + year | 0 | 0, data=data[data$cluster_n_cells <= 20,])
  felm_allroads_ntlbase_urban_small <- felm(gc_urban_mean ~ years_since_improved_all_bin*factor(dmspols_1997_group)-factor(dmspols_1997_group)-years_since_improved_all_bin | cell_id + year | 0 | 0, data=data[data$cluster_n_cells <= 20,]) 
  
  felm_blabv50roads_urban_small <- felm(gc_urban_mean ~ years_since_improved_below50_bin + years_since_improved_50above_bin | cell_id + year | 0 | 0, data=data[!is.na(data$years_since_improved_all_bin) & data$cluster_n_cells <= 20,])
  felm_blabv50roads_ntlbase_urban_small <- felm(gc_urban_mean ~ years_since_improved_below50_bin*factor(dmspols_1997_group) + 
                                            years_since_improved_50above_bin*factor(dmspols_1997_group) -
                                            factor(dmspols_1997_group) -
                                            years_since_improved_below50_bin - 
                                            years_since_improved_50above_bin | cell_id + year | 0 | 0, data=data[!is.na(data$years_since_improved_all_bin) & data$cluster_n_cells <= 20,])  
  
  
  stargazer(felm_allroads_dmsp_mean,
            felm_blabv50roads_dmsp_mean,
            felm_allroads_ntlbase_dmsp_mean,
            felm_blabv50roads_ntlbase_dmsp_mean,
            felm_allroads_dmsp_max,
            felm_blabv50roads_dmsp_max,
            felm_allroads_ntlbase_dmsp_max,
            felm_blabv50roads_ntlbase_dmsp_max,
            
            felm_allroads_dmsp_mean_small,
            felm_blabv50roads_dmsp_mean_small,
            felm_allroads_ntlbase_dmsp_mean_small,
            felm_blabv50roads_ntlbase_dmsp_mean_small,
            felm_allroads_dmsp_max_small,
            felm_blabv50roads_dmsp_max_small,
            felm_allroads_ntlbase_dmsp_max_small,
            felm_blabv50roads_ntlbase_dmsp_max_small,
            dep.var.labels.include = T,
            dep.var.labels = c("DMSP-OLS (Mean), Full","DMSP-OLS (Max), Full",
                               "DMSP-OLS (Mean), Small","DMSP-OLS (Max), Small"),
            dep.var.caption = "",
            covariate.labels = covariate.labels,
            omit.stat = c("f","ser"),
            align=TRUE,
            no.space=TRUE,
            float=FALSE,
            column.sep.width="-10pt",
            digits=2,
            add.lines = list(
              c("Cell FE", rep("Y", 16)),
              c("Year FE", rep("Y", 16))
            ),
            out = file.path(tables_file_path, paste0("eventstudy_results_dmspminmax",
                                                     "_dataset",dataset,
                                                     "_distthresh",DIST_THRESH,
                                                     ".tex")))
  
  stargazer(felm_allroads_dmspols1,
            felm_blabv50roads_dmspols1,
            felm_allroads_ntlbase_dmspols1,
            felm_blabv50roads_ntlbase_dmspols1,
            felm_allroads_dmspols5,
            felm_blabv50roads_dmspols5,
            felm_allroads_ntlbase_dmspols5,
            felm_blabv50roads_ntlbase_dmspols5,
            
            felm_allroads_dmspols1_small,
            felm_blabv50roads_dmspols1_small,
            felm_allroads_ntlbase_dmspols1_small,
            felm_blabv50roads_ntlbase_dmspols1_small,
            felm_allroads_dmspols5_small,
            felm_blabv50roads_dmspols5_small,
            felm_allroads_ntlbase_dmspols5_small,
            felm_blabv50roads_ntlbase_dmspols5_small,
            dep.var.labels.include = T,
            dep.var.labels = c("N DMSP-OLS $>$ 0, Full","N DMSP-OLS $>$ 5, Full",
                               "N DMSP-OLS $>$ 0, Small","N DMSP-OLS $>$ 5, Small"),
            dep.var.caption = "",
            covariate.labels = covariate.labels,
            omit.stat = c("f","ser"),
            align=TRUE,
            no.space=TRUE,
            float=FALSE,
            column.sep.width="-10pt",
            digits=2,
            add.lines = list(
              c("Cell FE", rep("Y", 16)),
              c("Year FE", rep("Y", 16))
            ),
            out = file.path(tables_file_path, paste0("eventstudy_results_dmspbin",
                                                     "_dataset",dataset,
                                                     "_distthresh",DIST_THRESH,
                                                     ".tex")))
  
  stargazer(felm_allroads_urban,
            felm_blabv50roads_urban,
            felm_allroads_ntlbase_urban,
            felm_blabv50roads_ntlbase_urban,
            felm_allroads_urban_small,
            felm_blabv50roads_urban_small,
            felm_allroads_ntlbase_urban_small,
            felm_blabv50roads_ntlbase_urban_small,
    
            dep.var.labels.include = T,
            dep.var.labels = c("Urban, Full","Urban, Small"),
            dep.var.caption = "",
            covariate.labels = covariate.labels,
            omit.stat = c("f","ser"),
            align=TRUE,
            no.space=TRUE,
            float=FALSE,
            column.sep.width="-10pt",
            digits=2,
            add.lines = list(
              c("Cell FE", rep("Y", 8)),
              c("Year FE", rep("Y", 8))
            ),
            out = file.path(tables_file_path, paste0("eventstudy_results_urban",
                                                     "_dataset",dataset,
                                                     "_distthresh",DIST_THRESH,
                                                     ".tex")))
  
}

































































if(dataset %in% c("cluster_all", "cluster_belowmedianclustersize")){
  felm_allroads <- felm(dmspols_zhang ~ years_since_improved_all_bin | cell_id + year | 0 | 0, data=data)
  felm_allroads_ntlbase <- felm(dmspols_zhang ~ years_since_improved_all_bin*factor(dmspols_1997_group)-factor(dmspols_1997_group)-years_since_improved_all_bin | cell_id + year | 0 | 0, data=data) 
  
  felm_blabv50roads <- felm(dmspols_zhang ~ years_since_improved_below50_bin + years_since_improved_50above_bin | cell_id + year | 0 | 0, data=data[!is.na(data$years_since_improved_all_bin),])
  felm_blabv50roads_ntlbase <- felm(dmspols_zhang ~ years_since_improved_below50_bin*factor(dmspols_1997_group) + 
         years_since_improved_50above_bin*factor(dmspols_1997_group) -
         factor(dmspols_1997_group) -
         years_since_improved_below50_bin - 
         years_since_improved_50above_bin | cell_id + year | 0 | 0, data=data[!is.na(data$years_since_improved_all_bin),])
} else{
  felm_allroads <- felm(dmspols_zhang ~ years_since_improved_all_bin | cell_id + year | 0 | 0, data=data)
  felm_allroads_ntlbase <- felm(dmspols_zhang ~ years_since_improved_all_bin*factor(dmspols_1997_group)-factor(dmspols_1997_group)-years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data) 
  
  felm_blabv50roads <- felm(dmspols_zhang ~ years_since_improved_below50_bin + years_since_improved_50above_bin | cell_id + year | 0 | 0, data=data[!is.na(data$years_since_improved_all_bin),])
  felm_blabv50roads_ntlbase <- felm(dmspols_zhang ~ years_since_improved_below50_bin*factor(dmspols_1997_group) + 
                                      years_since_improved_50above_bin*factor(dmspols_1997_group) -
                                      factor(dmspols_1997_group) -
                                      years_since_improved_below50_bin - 
                                      years_since_improved_50above_bin | cell_id + year | 0 | 0, data=data[!is.na(data$years_since_improved_all_bin),])  
}



stargazer(felm_allroads,
          felm_blabv50roads,
          felm_allroads_ntlbase,
          felm_blabv50roads_ntlbase,
          dep.var.labels.include = T,
          dep.var.labels = "DMSP-OLS",
          dep.var.caption = "",
          covariate.labels = covariate.labels,
          omit.stat = c("f","ser"),
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width="8pt",
          digits=2,
          add.lines = list(
            c("Cell FE", rep("Y", 4)),
            c("Year FE", rep("Y", 4))
          ),
          out = file.path(tables_file_path, paste0("eventstudy_results",
                                                   "_dataset",dataset,
                                                   "_distthresh",DIST_THRESH,
                                                   ".tex"))
)

# Cluster: Number of Lit Cells -------------------------------------------------
if(dataset %in% c("cluster_all", "cluster_belowmedianclustersize")){
  
  ## Number of Lit Cells
  felm_allroads <- felm(dmspols_1_above ~ years_since_improved_all_bin | cell_id + year | 0 | 0, data=data)
  felm_allroads_ntlbase <- felm(dmspols_1_above ~ years_since_improved_all_bin*factor(dmspols_1997_group)-factor(dmspols_1997_group)-years_since_improved_all_bin | cell_id + year | 0 | 0, data=data) 
  
  felm_blabv50roads <- felm(dmspols_1_above ~ years_since_improved_below50_bin + years_since_improved_50above_bin | cell_id + year | 0 | 0, data=data[!is.na(data$years_since_improved_all_bin),])
  felm_blabv50roads_ntlbase <- felm(dmspols_1_above ~ years_since_improved_below50_bin*factor(dmspols_1997_group) + 
                                      years_since_improved_50above_bin*factor(dmspols_1997_group) -
                                      factor(dmspols_1997_group) -
                                      years_since_improved_below50_bin - 
                                      years_since_improved_50above_bin | cell_id + year | 0 | 0, data=data[!is.na(data$years_since_improved_all_bin),])
  
  stargazer(felm_allroads,
            felm_blabv50roads,
            felm_allroads_ntlbase,
            felm_blabv50roads_ntlbase,
            dep.var.labels.include = T,
            dep.var.labels = "Number Lits Cells Greater than/Equal 1",
            dep.var.caption = "",
            covariate.labels = covariate.labels,
            omit.stat = c("f","ser"),
            align=TRUE,
            no.space=TRUE,
            float=FALSE,
            column.sep.width="8pt",
            digits=2,
            add.lines = list(
              c("Cell FE", rep("Y", 4)),
              c("Year FE", rep("Y", 4))
            ),
            out = file.path(tables_file_path, paste0("eventstudy_results_litcells1",
                                                     "_dataset",dataset,
                                                     "_distthresh",DIST_THRESH,
                                                     ".tex"))
  )
  
  
  ## Number of Lit Cells
  felm_allroads <- felm(dmspols_5_above ~ years_since_improved_all_bin | cell_id + year | 0 | 0, data=data)
  felm_allroads_ntlbase <- felm(dmspols_5_above ~ years_since_improved_all_bin*factor(dmspols_1997_group)-factor(dmspols_1997_group)-years_since_improved_all_bin | cell_id + year | 0 | 0, data=data) 
  
  felm_blabv50roads <- felm(dmspols_5_above ~ years_since_improved_below50_bin + years_since_improved_50above_bin | cell_id + year | 0 | 0, data=data[!is.na(data$years_since_improved_all_bin),])
  felm_blabv50roads_ntlbase <- felm(dmspols_5_above ~ years_since_improved_below50_bin*factor(dmspols_1997_group) + 
                                      years_since_improved_50above_bin*factor(dmspols_1997_group) -
                                      factor(dmspols_1997_group) -
                                      years_since_improved_below50_bin - 
                                      years_since_improved_50above_bin | cell_id + year | 0 | 0, data=data[!is.na(data$years_since_improved_all_bin),])
  
  stargazer(felm_allroads,
            felm_blabv50roads,
            felm_allroads_ntlbase,
            felm_blabv50roads_ntlbase,
            dep.var.labels.include = T,
            dep.var.labels = "Number Lits Cells Greater than/Equal 5",
            dep.var.caption = "",
            covariate.labels = covariate.labels,
            omit.stat = c("f","ser"),
            align=TRUE,
            no.space=TRUE,
            float=FALSE,
            column.sep.width="8pt",
            digits=2,
            add.lines = list(
              c("Cell FE", rep("Y", 4)),
              c("Year FE", rep("Y", 4))
            ),
            out = file.path(tables_file_path, paste0("eventstudy_results_litcells5",
                                                     "_dataset",dataset,
                                                     "_distthresh",DIST_THRESH,
                                                     ".tex"))
  )

}



# Regressions ------------------------------------------------------------------
lm_confint_tidy <- function(lm){
  lm_confint <- confint(lm) %>% 
    as.data.frame
  names(lm_confint) <- c("p025", "p975")
  lm_confint$b <- (lm_confint$p025 + lm_confint$p975)/2
  lm_confint$variable <- row.names(lm_confint)
  
  lm_confint <- lm_confint[!grepl("cluster_id)|year)|Intercept)", lm_confint$variable),]
  lm_confint$years_since_improved <- gsub(".*)", "", lm_confint$variable) %>% as.numeric
  
  return(lm_confint)
}

#### Constant sample, Without Year FE
DV <- "dmspols"
FE <- "cell_id"
dmspols_1997_bin_choice <- "low" 

results_all <- data.frame(NULL)

for(DV in c("dmspols", "dmspols_zhang","dmspols_ihs", "dmspols_zhang_ihs")){
  for(FE in c("cell_id", "cell_id + year")){
    for(dmspols_1997_bin_choice in c("low","high","all")){
      
      print(paste(DV, FE, dmspols_1997_bin_choice))
      
      if(dmspols_1997_bin_choice == "low"){
        dmspols_1997_bin <- 0
      } else if(dmspols_1997_bin_choice == "high"){
        dmspols_1997_bin <- 1
      } else if(dmspols_1997_bin_choice == "all"){
        dmspols_1997_bin <- c(0,1)
      }
      
      improved_all_formula <-     paste(DV, "~ factor(years_since_improved_all) | ", FE, " | 0 | 0") %>% as.formula
      improved_below50_formula <- paste(DV, "~ factor(years_since_improved_below50) | ", FE, " | 0 | 0") %>% as.formula
      improved_50above_formula <- paste(DV, "~ factor(years_since_improved_50above) | ", FE, " | 0 | 0") %>% as.formula
      
      #### Constant Sample
      lm_anyroad_constant <- felm(improved_all_formula, data=data_improved_all[data_improved_all$dmspols_1997_bin %in% dmspols_1997_bin,]) %>% 
        lm_confint_tidy %>% 
        dplyr::mutate(improved_road = "All") %>%
        dplyr::mutate(sample = "constant") %>%
        dplyr::mutate(DV = DV) %>%
        dplyr::mutate(FE = gsub("\\+| ","",FE)) %>%
        dplyr::mutate(dmspols_1997_bin = paste(dmspols_1997_bin, collapse=""))
      
      lm_below50_constant <- felm(improved_below50_formula, data=data_improved_below50[data_improved_below50$dmspols_1997_bin %in% dmspols_1997_bin,]) %>% 
        lm_confint_tidy %>% 
        dplyr::mutate(improved_road = "Speed Limit\nBelow\n50 km/hr\n ") %>%
        dplyr::mutate(sample = "constant") %>%
        dplyr::mutate(DV = DV) %>%
        dplyr::mutate(FE = gsub("\\+| ","",FE)) %>%
        dplyr::mutate(dmspols_1997_bin = paste(dmspols_1997_bin, collapse=""))
      
      lm_50above_constant <- felm(improved_50above_formula, data=data_improved_50above[data_improved_50above$dmspols_1997_bin %in% dmspols_1997_bin,]) %>% 
        lm_confint_tidy %>% 
        dplyr::mutate(improved_road = "Speed Limit\n50 km/hr\nand Above\n ") %>%
        dplyr::mutate(sample = "constant") %>%
        dplyr::mutate(DV = DV) %>%
        dplyr::mutate(FE = gsub("\\+| ","",FE)) %>%
        dplyr::mutate(dmspols_1997_bin = paste(dmspols_1997_bin, collapse="")) 
      
      #### Nonconstant Sample
      lm_anyroad_nonconstant <- felm(improved_all_formula, data=data[data$dmspols_1997_bin %in% dmspols_1997_bin,]) %>% 
        lm_confint_tidy %>% 
        dplyr::mutate(improved_road = "All") %>%
        dplyr::mutate(sample = "nonconstant") %>%
        dplyr::mutate(DV = DV) %>%
        dplyr::mutate(FE = gsub("\\+| ","",FE)) %>%
        dplyr::mutate(dmspols_1997_bin = paste(dmspols_1997_bin, collapse=""))
      
      lm_below50_nonconstant <- felm(improved_below50_formula, data=data[data$dmspols_1997_bin %in% dmspols_1997_bin,]) %>% 
        lm_confint_tidy %>% 
        dplyr::mutate(improved_road = "Speed Limit\nBelow\n50 km/hr\n ") %>%
        dplyr::mutate(sample = "nonconstant") %>%
        dplyr::mutate(DV = DV) %>%
        dplyr::mutate(FE = gsub("\\+| ","",FE)) %>%
        dplyr::mutate(dmspols_1997_bin = paste(dmspols_1997_bin, collapse=""))
      
      lm_50above_nonconstant <- felm(improved_50above_formula, data=data[data$dmspols_1997_bin %in% dmspols_1997_bin,]) %>% 
        lm_confint_tidy %>% 
        dplyr::mutate(improved_road = "Speed Limit\n50 km/hr\nand Above\n ") %>%
        dplyr::mutate(sample = "nonconstant") %>%
        dplyr::mutate(DV = DV) %>%
        dplyr::mutate(FE = gsub("\\+| ","",FE)) %>%
        dplyr::mutate(dmspols_1997_bin = paste(dmspols_1997_bin, collapse="")) 
      
      results_all_i <- bind_rows(lm_anyroad_constant, 
                                 lm_below50_constant,
                                 lm_50above_constant,
                                 lm_anyroad_nonconstant, 
                                 lm_below50_nonconstant,
                                 lm_50above_nonconstant)
      
      results_all <- bind_rows(results_all, results_all_i)
      
      
    }
  }
}

# Figures ----------------------------------------------------------------------
for(DV in c("dmspols", "dmspols_zhang","dmspols_ihs", "dmspols_zhang_ihs")){
  for(FE in c("cell_id", "cell_idyear")){
    for(dmspols_1997_bin in c("0","1","01")){
      for(sample in c("constant", "nonconstant")){
        
        p_dodge_width <- .6
        
        fig <- results_all[results_all$DV %in% DV &
                             results_all$sample %in% sample & 
                             results_all$FE == FE &
                             results_all$dmspols_1997_bin == dmspols_1997_bin,] %>%
          ggplot(aes(x=years_since_improved, y=b, ymin=p025, ymax=p975,
                     group=improved_road,
                     color=improved_road)) +
          geom_vline(xintercept=0,size=3,alpha=0.15) +
          geom_point(position = position_dodge(width = p_dodge_width),size=3) + 
          geom_linerange(position = position_dodge(width = p_dodge_width),size=1.25) +
          theme_minimal() +
          labs(x="Years Since Improved Road Constructed",
               y="Coefficient",
               color = "Improved\nRoad") +
          scale_x_continuous(breaks = seq(-30, 30, by = 2)) +
          theme(axis.text = element_text(size=14, color="black"),
                axis.title = element_text(size=15, color="black"),
                legend.text = element_text(size=14),
                legend.title = element_text(size=14))
        ggsave(fig, filename = file.path(figures_file_path, paste0("eventstudycoefs",
                                                                   "_DV",DV,
                                                                   "_FE",FE,
                                                                   "_ntlbase",dmspols_1997_bin,
                                                                   "_sample",sample,
                                                                   "_dataset",dataset,
                                                                   ".png")),
               height=4,width=10)
        
      }
    }
  }
}


DV <- "dmspols_zhang"
FE <- "cell_id"
improved_all_formula <-     paste(DV, "~ factor(years_since_improved_all) | ", FE, " | 0 | 0") %>% as.formula

#### Constant Sample
felm(improved_all_formula, data=data_improved_all[data_improved_all$cluster_n_cells <= 20,]) %>% summary
felm(improved_all_formula, data=data_improved_all[data_improved_all$cluster_n_cells > 20,]) %>% summary

























data$years_since_improved_var <- data$years_since_improved_all
lm_anyroad_nonconstant <-            felm(improved_all_formula, data=data[data$dmspols_1997 >= 0,])
lm_anyroad_nonconstant_baseNtl_0 <-  felm(improved_all_formula, data=data[data$dmspols_1997 == 0,])
lm_anyroad_nonconstant_baseNtl_g0 <- felm(improved_all_formula, data=data[data$dmspols_1997 > 0,])

data$years_since_improved_var <- data$years_since_improved_below50
lm_below50_nonconstant  <-           felm(improved_below50_formula, data=data[data$dmspols_1997 >= 0,])
lm_below50_nonconstant_baseNtl_0 <-  felm(improved_below50_formula, data=data[data$dmspols_1997 == 0,])
lm_below50_nonconstant_baseNtl_g0 <- felm(improved_below50_formula, data=data[data$dmspols_1997 > 0,])

data$years_since_improved_var <- data$years_since_improved_50above
lm_50above_nonconstant  <-           felm(improved_50above_formula, data=data[data$dmspols_1997 >= 0,])
lm_50above_nonconstant_baseNtl_0 <-  felm(improved_50above_formula, data=data[data$dmspols_1997 == 0,])
lm_50above_nonconstant_baseNtl_g0 <- felm(improved_50above_formula, data=data[data$dmspols_1997 > 0,])

stargazer(lm_anyroad_constant,
          lm_below50_constant,
          lm_50above_constant,
          lm_anyroad_constant_baseNtl_0,
          lm_below50_constant_baseNtl_0,
          lm_50above_constant_baseNtl_0,
          lm_anyroad_constant_baseNtl_g0,
          lm_below50_constant_baseNtl_g0,
          lm_50above_constant_baseNtl_g0,
          dep.var.labels.include = T,
          dep.var.labels = "DMSP-OLS",
          dep.var.caption = "",
          omit.stat = c("f","ser"),
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width="8pt",
          digits=2,
          add.lines = list(
            c("Baseline NTL", rep("Any",3), rep(0,3), rep("g0",3)),
            c("Cell FE", rep("Y",9)),
            c("Year FE", rep("N",9)),
            c("Constant Sample", rep("Y",9))
          ),
          out = file.path(tables_file_path, paste0("eventstudy_results_sampleconstant",
                                                   "_DV",DV,
                                                   "_FE",gsub("\\+| ","",FE),
                                                   ".tex"))
)




# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# PURGATORY --------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

paste0("DV",DV,
       "_dmspols_1997_bin",
       paste(dmspols_1997_bin, collapse=""))


#### Non-Constant sample, Without Year FE
lm_anyroad_nonconstant <- felm(dmspols_zhang ~ factor(years_since_improved_all) | cell_id  | 0 | 0, data=data[data$dmspols_1997 >= 0,])
lm_below50_nonconstant  <- felm(dmspols ~ factor(years_since_improved_below50) | cell_id | 0 | 0, data=data[data$dmspols_1997 >= 0,])
lm_50above_nonconstant  <- felm(dmspols ~ factor(years_since_improved_50above) | cell_id | 0 | 0, data=data[data$dmspols_1997 >= 0,])

lm_anyroad_nonconstant_baseNtl_0 <- felm(dmspols ~ factor(years_since_improved_all) | cell_id  | 0 | 0, data=data[data$dmspols_1997 == 0,])
lm_below50_nonconstant_baseNtl_0 <- felm(dmspols ~ factor(years_since_improved_below50) | cell_id | 0 | 0, data=data[data$dmspols_1997 == 0,])
lm_50above_nonconstant_baseNtl_0 <- felm(dmspols ~ factor(years_since_improved_50above) | cell_id | 0 | 0, data=data[data$dmspols_1997 == 0,])

lm_anyroad_nonconstant_baseNtl_g0 <- felm(dmspols ~ factor(years_since_improved_all) | cell_id  | 0 | 0, data=data[data$dmspols_1997 > 0,])
lm_below50_nonconstant_baseNtl_g0 <- felm(dmspols ~ factor(years_since_improved_below50) | cell_id | 0 | 0, data=data[data$dmspols_1997 > 0,])
lm_50above_nonconstant_baseNtl_g0 <- felm(dmspols ~ factor(years_since_improved_50above) | cell_id | 0 | 0, data=data[data$dmspols_1997 > 0,])

summary(lm_anyroad_constant)
summary(lm_anyroad_nonconstant)
summary(lm_below50)
summary(lm_50above)


# Regressions ------------------------------------------------------------------

lm_anyroad <- felm(dmspols ~ factor(years_since_improved_all) | cell_id + year | 0 | 0, data=data_improved_all[data_improved_all$dmspols_1997 >= 0,])


lm_below50 <- felm(dmspols_zhang ~ factor(years_since_improved_below50) | cell_id | 0 | 0, data=data_improved_below50[data_improved_below50$dmspols_1997 >= 0,])
lm_50above <- felm(dmspols_zhang ~ factor(years_since_improved_below50) + factor(years_since_improved_50above) | cell_id | 0 | 0, data=data_improved_50above[data_improved_50above$dmspols_1997 >= 0,])

summary(lm_anyroad)
summary(lm_below50)
summary(lm_50above)

lm_anyroad <- felm(dmspols ~ factor(years_since_improved_all) | 0 | 0 | 0, data=data_improved_all) %>% 
  lm_confint_tidy %>%
  mutate(subset = "anyroad")

lm_below50 <- felm(dmspols ~ factor(years_since_improved_below50) | 0 | 0 | 0, data=data_improved_below50) %>% 
  lm_confint_tidy %>%
  mutate(subset = "below50")

lm_50above <- felm(dmspols ~ factor(years_since_improved_50above) | 0 | 0 | 0, data=data_improved_50above) %>% 
  lm_confint_tidy %>%
  mutate(subset = "50above")

lm_all <- bind_rows(lm_anyroad, lm_50above, lm_below50)

ggplot(data=lm_all, aes(x=years_since_improved, y=b, ymin=p025, ymax=p975, group=subset, color=subset)) + 
  geom_line() +
  geom_point() +
  geom_linerange()


ggplot(data=lm_all[lm_all$subset %in% "anyroad",], aes(x=years_since_improved, y=b, ymin=p025, ymax=p975, group=subset, color=subset)) + 
  geom_line() +
  geom_point() +
  geom_linerange()














# Treatment variables
data$near_any_road <- apply(subset(data, select=c(distance_road_speed_10, distance_road_speed_15, distance_road_speed_20, distance_road_speed_25, distance_road_speed_30, distance_road_speed_35, distance_road_speed_45, distance_road_speed_50, distance_road_speed_70, distance_road_speed_120)), 1, function(x) min(x, na.rm=T)) < DIST_THRESH*1000
data$near_70kmhrmore_road <- apply(subset(data, select=c(distance_road_speed_70, distance_road_speed_120)), 1, function(x) min(x, na.rm=T)) < DIST_THRESH*1000
data$near_50kmhrmore_road <- apply(subset(data, select=c(distance_road_speed_50, distance_road_speed_70, distance_road_speed_120)), 1, function(x) min(x, na.rm=T)) < DIST_THRESH*1000
data$near_45kmhrmore_road <- apply(subset(data, select=c(distance_road_speed_45, distance_road_speed_50, distance_road_speed_70, distance_road_speed_120)), 1, function(x) min(x, na.rm=T)) < DIST_THRESH*1000
data$near_30kmhrmore_road <- apply(subset(data, select=c(distance_road_speed_30, distance_road_speed_35, distance_road_speed_45, distance_road_speed_50, distance_road_speed_70, distance_road_speed_120)), 1, function(x) min(x, na.rm=T)) < DIST_THRESH*1000

data$near_any_road[is.na(data$near_any_road)] <- FALSE
data$near_70kmhrmore_road[is.na(data$near_70kmhrmore_road)] <- FALSE
data$near_50kmhrmore_road[is.na(data$near_50kmhrmore_road)] <- FALSE
data$near_45kmhrmore_road[is.na(data$near_45kmhrmore_road)] <- FALSE
data$near_30kmhrmore_road[is.na(data$near_30kmhrmore_road)] <- FALSE

# Remove unneeded vars to clean up memory --------------------------------------
data <- subset(data, select=-c(long, lat, distance_road_speed_10, distance_road_speed_15,
                               distance_road_speed_20, distance_road_speed_25,
                               distance_road_speed_30, distance_road_speed_35,
                               distance_road_speed_45, distance_road_speed_50,
                               distance_road_speed_70, distance_road_speed_120,
                               distance_asphaltconcrete, distance_earth, 
                               distance_gravel, distance_majorgravel, distance_cobblestone))

# Baseline Values of Variables -------------------------------------------------
data <- data %>%
  group_by(cell_id) %>% 
  mutate(dmspols_1996=dmspols[year==1996]) %>% 
  mutate(globcover_urban_1996=globcover_urban[year==1996]) %>% 
  mutate(globcover_cropland_1996=globcover_cropland[year==1996]) %>%
  as.data.table

# Determine Years Since Treated ------------------------------------------------
create_treated_vars <- function(var){
  
  data$treatvar <- data[[var]]
  
  # treatvar_NA: 1 if treated; NA if not
  data$treatvar_NA <- ifelse(data$treatvar %in% TRUE, 1, NA)
  
  # treatvar_year: if treated, has year
  data$treatvar_year <- data$treatvar_NA * data$year
  
  # minimum value of treatvar_year for year of treatment (will be NA if no treatment)
  data <- data %>%
    group_by(cell_id) %>%
    mutate(treatvar_yeartreat = min(treatvar_year, na.rm=T))
  data$treatvar_yeartreat[data$treatvar_yeartreat == Inf] <- NA
  data$treatvar_some_point <- !is.na(data$treatvar_yeartreat)
  data$treatvar_years_since_treat <- data$year - data$treatvar_yeartreat
  
  data <- subset(data, select=c(treatvar_some_point, treatvar_yeartreat, treatvar_years_since_treat))
  
  names(data) <- gsub("treatvar",var, names(data))
  
  return(data)
}

data <- cbind(data, create_treated_vars("near_any_road"))
data <- cbind(data, create_treated_vars("near_70kmhrmore_road"))
data <- cbind(data, create_treated_vars("near_50kmhrmore_road"))
data <- cbind(data, create_treated_vars("near_45kmhrmore_road"))
data <- cbind(data, create_treated_vars("near_30kmhrmore_road"))

# Regression -------------------------------------------------------------------
lm1 <- felm(dmspols ~ factor(near_45kmhrmore_road_years_since_treat) | cell_id + year | 0 | GADM_ID_3, data=data[data$near_45kmhrmore_road_yeartreat >= 1997 & data$dmspols_1996 > 1,])
coefplot(lm1)

lm1 <- felm(globcover_urban ~ factor(near_70kmhrmore_road_years_since_treat) | cell_id | 0 | GADM_ID_3, data=data[data$near_70kmhrmore_road_yeartreat >= 1997 & data$dmspols_1996 %in% 0,])
coefplot(lm1)

lm1 <- felm(dmspols ~ factor(near_45kmhrmore_road_years_since_treat) - 1 | cell_id + year | 0 | GADM_ID_3, data=data[data$near_45kmhrmore_road_yeartreat >= 1998,])
lm1_confint_df <- confint(lm1) %>% as.data.frame
lm1_confint_df$years_since_treat <- row.names(lm1_confint_df) %>% str_replace_all("factor(near_45kmhrmore_road_years_since_treat)", "")

lm1 <- felm(dmspols ~ factor(near_45kmhrmore_road_years_since_treat) - 1 | cell_id + year | 0 | GADM_ID_3, data=data[data$near_45kmhrmore_road_yeartreat >= 1998,])
lm1 <- lm(dmspols ~ factor(near_45kmhrmore_road_years_since_treat) - 1, data=data[data$near_45kmhrmore_road_yeartreat >= 1996,])
coefplot(lm1)

coefficients(lm1)

lm1 <- felm(globcover_urban ~ factor(near_45kmhrmore_road_years_since_treat) - 1 | 0 | 0 | GADM_ID_3, data=data[data$year >= 1998,])
coefplot(lm1)

