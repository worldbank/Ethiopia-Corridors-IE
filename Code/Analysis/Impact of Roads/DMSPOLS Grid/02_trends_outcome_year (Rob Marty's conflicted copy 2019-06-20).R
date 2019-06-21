# Impact of Expressway Expansion
# Ethiopia IE

dataset <- "points_5percent"

# Load Data --------------------------------------------------------------------
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

#### Subset Data 
# By Years
data <- data[(data$year >= 1992) & (data$year <= 2016),]

# Only Include Study Area
data <- data[data$near_improved_all_2016 %in% 1,]

# Summarize by Year ------------------------------------------------------------
data <- as.data.table(data)

data_annual_all <- data[, .(dmspols = mean(dmspols),
                            dmspols_zhang = mean(dmspols_zhang),
                            dmspols_1 = mean(dmspols_1),
                            dmspols_5 = mean(dmspols_5),
                            dmspols_zhang_1 = mean(dmspols_zhang_1),
                            dmspols_zhang_5 = mean(dmspols_zhang_5),
                            globcover_urban = mean(globcover_urban),
                            globcover_cropland = mean(globcover_cropland),
                            globcover_cropland_irrigated = mean(globcover_cropland_irrigated),
                            globcover_cropland_mosaic = mean(globcover_cropland_mosaic),
                            globcover_cropland_rainfed = mean(globcover_cropland_rainfed),
                            ndvi = mean(ndvi),
                            ndvi_cropland = mean(ndvi_cropland),
                            ndvi_nocropland = mean(ndvi_nocropland)),
                        by=year] %>% as.data.frame

# Stack ------------------------------------------------------------------------
data_annual_all <- data_annual_all %>% gather("variable", "value", -year)

# Trends -----------------------------------------------------------------------
##### NTL
ggplot(data_annual_all[data_annual_all$variable %in% c("dmspols", "dmspols_zhang"),], 
       aes(x=year, y=value, group=variable, color=variable)) +
  geom_line(size=2) +
  theme_minimal() +
  scale_color_manual(name = "", labels = c("Raw", "Intercalibrated"),
                     values=c("dodgerblue4", "darkorange2")) +
  labs(x="", y="Luminosity", title="Nighttime Lights") +
  theme(plot.title = element_text(hjust= 0.5, face="bold")) +
  scale_x_continuous(breaks = seq(1992, 2014, 2),
                     limits = c(1992, 2014))

ggplot(data_annual_all[data_annual_all$variable %in% c("dmspols_1", "dmspols_zhang_1"),], 
       aes(x=year, y=value, group=variable, color=variable, size=variable)) +
  geom_line() +
  theme_minimal() +
  scale_color_manual(name = "", labels = c("Raw", "Intercalibrated"),
                     values=c("dodgerblue4", "darkorange2")) +
  scale_size_manual(name = "", labels = c("Raw", "Intercalibrated"),
                    values=c(2,1)) +
  labs(x="", y="", title="Proportion of Cells where NTL > 0") +
  theme(plot.title = element_text(hjust= 0.5, face="bold")) +
  scale_x_continuous(breaks = seq(1992, 2014, 2),
                     limits = c(1992, 2014))

ggplot(data_annual_all[data_annual_all$variable %in% c("dmspols_5", "dmspols_zhang_5"),], 
       aes(x=year, y=value, group=variable, color=variable, size=variable)) +
  geom_line() +
  theme_minimal() +
  scale_color_manual(name = "", labels = c("Raw", "Intercalibrated"),
                     values=c("dodgerblue4", "darkorange2")) +
  scale_size_manual(name = "", labels = c("Raw", "Intercalibrated"),
                    values=c(2,1.5)) +
  labs(x="", y="", title="Proportion of Cells where NTL > 5") +
  theme(plot.title = element_text(hjust= 0.5, face="bold")) +
  scale_x_continuous(breaks = seq(1992, 2014, 2),
                     limits = c(1992, 2014))

##### Globcover
gc_urban_fig <- ggplot(data_annual_all[data_annual_all$variable %in% "globcover_urban",], 
       aes(x=year, y=value), size=1, color="dodgerblue4") +
  geom_line(size=2) +
  theme_minimal() +
  labs(x="", y="", title = "Proportion of Cells\nClassified as Urban") +
  theme(plot.title = element_text(hjust = 0.5, face="bold")) +
  scale_x_continuous(breaks = seq(1992, 2016, 2),
                     limits = c(1992, 2016))

gc_crop_rain_fig <- ggplot(data_annual_all[data_annual_all$variable %in% "globcover_cropland_rainfed",], 
                       aes(x=year, y=value), size=1, color="dodgerblue4") +
  geom_line(size=2) +
  theme_minimal() +
  labs(x="", y="", title = "Proportion of Cells\nClassified as Cropland (Rainfed)") +
  theme(plot.title = element_text(hjust = 0.5, face="bold")) +
  scale_x_continuous(breaks = seq(1992, 2016, 2),
                     limits = c(1992, 2016))

gc_crop_mosaic_fig <- ggplot(data_annual_all[data_annual_all$variable %in% "globcover_cropland_mosaic",], 
                           aes(x=year, y=value), size=1, color="dodgerblue4") +
  geom_line(size=2) +
  theme_minimal() +
  labs(x="", y="", title = "Proportion of Cells\nClassified as Cropland (Mosaic)") +
  theme(plot.title = element_text(hjust = 0.5, face="bold")) +
  scale_x_continuous(breaks = seq(1992, 2016, 2),
                     limits = c(1992, 2016))

gc_crop_irrig_fig <- ggplot(data_annual_all[data_annual_all$variable %in% "globcover_cropland_irrigated",], 
                             aes(x=year, y=value), size=1, color="dodgerblue4") +
  geom_line(size=2) +
  theme_minimal() +
  labs(x="", y="", title = "Proportion of Cells\nClassified as Cropland (Irrigated)") +
  theme(plot.title = element_text(hjust = 0.5, face="bold")) +
  scale_x_continuous(breaks = seq(1992, 2016, 2),
                     limits = c(1992, 2016))

ggplot(data_annual_all[data_annual_all$variable %in% c("globcover_cropland_rainfed"),], 
       aes(x=year, y=value, group=variable, color=variable)) +
  geom_line()

ggplot(data_annual_all[data_annual_all$variable %in% "globcover_cropland_mosaic",], 
       aes(x=year, y=value, group=variable, color=variable)) +
  geom_line()

ggplot(data_annual_all[data_annual_all$variable %in% "globcover_cropland_irrigated",], 
       aes(x=year, y=value, group=variable, color=variable)) +
  geom_line()

#### NDVI
ggplot(data_annual_all[data_annual_all$variable %in% c("ndvi", "ndvi_cropland"),], 
       aes(x=year, y=value, group=variable, color=variable), size=1) +
  geom_line()
