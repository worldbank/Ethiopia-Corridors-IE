# Impact of Expressway Expansion
# Ethiopia IE

# PARAMETERS
dataset <- "points_5percent"
constant_sample <- FALSE
baseline <- "ntl_1" # all, ntl_1, ntl_2, ntl_3

WIDTH <- 5.5
HEIGHT <- 3.5

for(baseline in c("all", "ntl_1", "ntl_2", "ntl_3")){
  for(constant_sample in c(TRUE, FALSE)){
    print(baseline)

# Load Data & Incorporate Parameters -------------------------------------------
#### Data
if(dataset == "cluster_all"){
  data <- readRDS(file.path(project_file_path, "Data", "FinalData", "urban_cluster_dataset", "urban_cluster_data_analysisvars.Rds"))
  data$cell_id <- data$cluster_id
}

if(dataset == "points"){
  data <- readRDS(file.path(project_file_path, "Data", "FinalData", "dmspols_grid_dataset", "dmspols_level_dataset_analysisvars.Rds"))
}

if(dataset == "points_5percent"){
  data <- readRDS(file.path(project_file_path, "Data", "FinalData", "dmspols_grid_dataset", "dmspols_level_dataset_5percentsample_analysisvars.Rds"))
  data$gc_urban_mean <- data$globcover_urban
}

#### Constant Sample

if(constant_sample == TRUE){
  constant_01 <- 1
} else{
  constant_01 <- 0:1
}

#### Baseline Levels
if(baseline == "ntl_1") data <- data[data$dmspols_1997_group %in% 1,]
if(baseline == "ntl_2") data <- data[data$dmspols_1997_group %in% 2,]
if(baseline == "ntl_3") data <- data[data$dmspols_1997_group %in% 3,]

# Subset Data ------------------------------------------------------------------
# By Years
data <- data[(data$year >= 1992) & (data$year <= 2016),]

# Only Include Study Area
data <- data[data$near_improved_all_2016 %in% 1,]

#### Add variables
data$N <- 1

# Identify Cells in Constant Sample --------------------------------------------
year_improved_begin <- 2001
year_improved_end <- 2008

years_since_end <- 2012 - year_improved_end
years_since_begin <- 1997 - year_improved_begin 

data$constant_all <- 0
data$constant_all[(data$year_improved_all %in% year_improved_begin:year_improved_end) & 
                  (data$years_since_improved_all %in% years_since_begin:years_since_end)] <- 1

data$constant_50above <- 0
data$constant_50above[(data$year_improved_50above %in% year_improved_begin:year_improved_end) & 
                    (data$years_since_improved_50above %in% years_since_begin:years_since_end)] <- 1

data$constant_below50 <- 0
data$constant_below50[(data$year_improved_below50 %in% year_improved_begin:year_improved_end) & 
                        (data$years_since_improved_below50 %in% years_since_begin:years_since_end)] <- 1

#### Ensure sample is constant
## All
data$years_since_improved_all[data$constant_all %in% 1] %>% table %>% table
data$cell_id[data$constant_all %in% 1] %>% table %>% table

## 50 above
data$years_since_improved_50above[data$constant_50above %in% 1] %>% table %>% table
data$cell_id[data$constant_50above %in% 1] %>% table %>% table

## below 50
data$years_since_improved_below50[data$constant_below50 %in% 1] %>% table %>% table
data$cell_id[data$constant_below50 %in% 1] %>% table %>% table

# Summarize by Year ------------------------------------------------------------
data <- as.data.table(data)

data_annual_all <- data[data$constant_all %in% constant_01,][, .(dmspols = mean(dmspols, na.rm=T),
                            dmspols_zhang = mean(dmspols_zhang, na.rm=T),
                            dmspols_1 = mean(dmspols_1, na.rm=T),
                            dmspols_5 = mean(dmspols_5, na.rm=T),
                            dmspols_zhang_1 = mean(dmspols_zhang_1, na.rm=T),
                            dmspols_zhang_5 = mean(dmspols_zhang_5, na.rm=T),
                            globcover_urban = mean(globcover_urban, na.rm=T),
                            globcover_cropland = mean(globcover_cropland, na.rm=T),
                            globcover_cropland_irrigated = mean(globcover_cropland_irrigated, na.rm=T),
                            globcover_cropland_mosaic = mean(globcover_cropland_mosaic, na.rm=T),
                            globcover_cropland_rainfed = mean(globcover_cropland_rainfed, na.rm=T),
                            ndvi = mean(ndvi, na.rm=T),
                            ndvi_cropland = mean(ndvi_cropland, na.rm=T),
                            ndvi_nocropland = mean(ndvi_nocropland, na.rm=T),
                            N = sum(N, na.rm=T)),
                        by=years_since_improved_all] %>% 
  as.data.frame %>%
  dplyr::rename(years_since_treat = years_since_improved_all) %>%
  dplyr::mutate(treat = "all")

data_annual_50above <- data[data$near_improved_50above_2016 %in% 1 & data$constant_50above %in% constant_01,][, .(dmspols = mean(dmspols, na.rm=T),
                            dmspols_zhang = mean(dmspols_zhang, na.rm=T),
                            dmspols_1 = mean(dmspols_1, na.rm=T),
                            dmspols_5 = mean(dmspols_5, na.rm=T),
                            dmspols_zhang_1 = mean(dmspols_zhang_1, na.rm=T),
                            dmspols_zhang_5 = mean(dmspols_zhang_5, na.rm=T),
                            globcover_urban = mean(globcover_urban, na.rm=T),
                            globcover_cropland = mean(globcover_cropland, na.rm=T),
                            globcover_cropland_irrigated = mean(globcover_cropland_irrigated, na.rm=T),
                            globcover_cropland_mosaic = mean(globcover_cropland_mosaic, na.rm=T),
                            globcover_cropland_rainfed = mean(globcover_cropland_rainfed, na.rm=T),
                            ndvi = mean(ndvi, na.rm=T),
                            ndvi_cropland = mean(ndvi_cropland, na.rm=T),
                            ndvi_nocropland = mean(ndvi_nocropland, na.rm=T),
                            N = sum(N, na.rm=T)),
                        by=years_since_improved_all] %>% 
  as.data.frame %>%
  dplyr::rename(years_since_treat = years_since_improved_all) %>%
  dplyr::mutate(treat = "50above")

data_annual_below50 <- data[data$near_improved_below50_2016 %in% 1 & data$constant_below50 %in% constant_01,][, .(dmspols = mean(dmspols, na.rm=T),
                                                                         dmspols_zhang = mean(dmspols_zhang, na.rm=T),
                                                                         dmspols_1 = mean(dmspols_1, na.rm=T),
                                                                         dmspols_5 = mean(dmspols_5, na.rm=T),
                                                                         dmspols_zhang_1 = mean(dmspols_zhang_1, na.rm=T),
                                                                         dmspols_zhang_5 = mean(dmspols_zhang_5, na.rm=T),
                                                                         globcover_urban = mean(globcover_urban, na.rm=T),
                                                                         globcover_cropland = mean(globcover_cropland, na.rm=T),
                                                                         globcover_cropland_irrigated = mean(globcover_cropland_irrigated, na.rm=T),
                                                                         globcover_cropland_mosaic = mean(globcover_cropland_mosaic, na.rm=T),
                                                                         globcover_cropland_rainfed = mean(globcover_cropland_rainfed, na.rm=T),
                                                                         ndvi = mean(ndvi, na.rm=T),
                                                                         ndvi_cropland = mean(ndvi_cropland, na.rm=T),
                                                                         ndvi_nocropland = mean(ndvi_nocropland, na.rm=T),
                                                                         N = sum(N, na.rm=T)),
                                                                     by=years_since_improved_all] %>% 
  as.data.frame %>%
  dplyr::rename(years_since_treat = years_since_improved_all) %>%
  dplyr::mutate(treat = "below50")

# Stack ------------------------------------------------------------------------
data_annual_all <- bind_rows(data_annual_all,
                             data_annual_50above,
                             data_annual_below50) %>% gather("variable", "value", -years_since_treat, -treat)

if(constant_sample) data_annual_all <- data_annual_all[data_annual_all$years_since_treat %in% years_since_begin:years_since_end,]

#### Reorder Factors
data_annual_all$treat <- factor(data_annual_all$treat, levels=c("all", "50above", "below50"))

# Trends -----------------------------------------------------------------------
var_names <- bind_rows(
  data.frame(variable = "dmspols", name = "Average Luminosity (DMSP-OLS - Raw)"),
  data.frame(variable = "dmspols_zhang", name = "Average Luminosity (DMSP-OLS - Intercalibrated)"),
  data.frame(variable = "dmspols_1", name = "Proportion of Cells where NTL > 0 (Raw)"),
  data.frame(variable = "dmspols_5", name = "Proportion of Cells where NTL > 5 (Raw)"),
  data.frame(variable = "dmspols_zhang_1", name = "Proportion of Cells where NTL > 0 (Intercalibrated)"),
  data.frame(variable = "dmspols_zhang_5", name = "Proportion of Cells where NTL > 5 (Intercalibrated)"),
  data.frame(variable = "globcover_urban", name = "Proportion of Land\nClassified as Urban"),
  data.frame(variable = "globcover_cropland_rainfed", name = "Proportion of Land\nClassified as Cropland (Rainfed)"),
  data.frame(variable = "globcover_cropland_mosaic", name = "Proportion of Land\nClassified as Cropland (Mosaic)"),
  data.frame(variable = "globcover_cropland_irrigated", name = "Proportion of Land\nClassified as Cropland (Irrigated)")
)

for(i in 1:nrow(var_names)){
  var_i <- var_names[i,]
  
  fig <- ggplot(data_annual_all[data_annual_all$variable %in% var_i$variable,], 
         aes(x=years_since_treat, y=value, group=treat, color=treat)) +
    geom_vline(xintercept=0, alpha=0.2, size=2) +
    geom_line(size=2) +
    theme_minimal() +
    scale_color_manual(name = "Road", 
                       labels = c("All", "50 Above", "Below 50"),
                       values = c("dodgerblue4", "darkorange2", "darkgreen")) +
    labs(x="", y="", title=var_i$name) +
    theme(plot.title = element_text(hjust = 0.5, face="bold"))
  
  if(constant_sample == TRUE){
    ggsave(fig, filename = file.path(figures_file_path, "trends_years_since", "constant_sample",paste0("baseline_",baseline), paste0("trends_yearssince_", var_i$variable,"_constant",constant_sample,"_base",baseline,".png")), width=WIDTH, height=HEIGHT)
  } else{
    ggsave(fig, filename = file.path(figures_file_path, "trends_years_since", "full_sample",paste0("baseline_",baseline), paste0("trends_yearssince_", var_i$variable,"_constant",constant_sample,"_base",baseline,".png")), width=WIDTH, height=HEIGHT)
  }
  
}




  }
}


