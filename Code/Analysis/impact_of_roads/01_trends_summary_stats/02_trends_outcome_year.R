# Impact of Expressway Expansion
# Ethiopia IE

dataset <- "points_5percent"

WIDTH <- 5.5
HEIGHT <- 3.5

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
ntl_fig <- ggplot(data_annual_all[data_annual_all$variable %in% c("dmspols", "dmspols_zhang"),], 
       aes(x=year, y=value, group=variable, color=variable)) +
  geom_line(size=2) +
  theme_minimal() +
  scale_color_manual(name = "", labels = c("Raw", "Intercalibrated"),
                       values=c("dodgerblue4", "darkorange2")) +
  labs(x="", y="Luminosity", title="Nighttime Lights") +
  theme(plot.title = element_text(hjust= 0.5, face="bold")) +
  scale_x_continuous(breaks = seq(1992, 2014, 2),
                     limits = c(1992, 2014))
ggsave(ntl_fig,  filename = file.path(figures_file_path,"trends_annual", "trends_ntl.png"), width=WIDTH, height=HEIGHT)

ntl_1_fig <- ggplot(data_annual_all[data_annual_all$variable %in% c("dmspols_1", "dmspols_zhang_1"),], 
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
ggsave(ntl_1_fig,  filename = file.path(figures_file_path,"trends_annual", "trends_ntl_1.png"), width=WIDTH, height=HEIGHT)

ntl_5_fig <- ggplot(data_annual_all[data_annual_all$variable %in% c("dmspols_5", "dmspols_zhang_5"),], 
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
ggsave(ntl_5_fig,  filename = file.path(figures_file_path,"trends_annual", "trends_ntl_5.png"), width=WIDTH, height=HEIGHT)

##### Globcover
var_names <- bind_rows(
  data.frame(variable = "globcover_urban", name = "Proportion of Land\nClassified as Urban"),
  data.frame(variable = "globcover_cropland_rainfed", name = "Proportion of Land\nClassified as Cropland (Rainfed)"),
  data.frame(variable = "globcover_cropland_mosaic", name = "Proportion of Land\nClassified as Cropland (Mosaic)"),
  data.frame(variable = "globcover_cropland_irrigated", name = "Proportion of Land\nClassified as Cropland (Irrigated)")
)

for(i in 1:nrow(var_names)){
  var_i <- var_names[i,]
  
  fig <- ggplot(data_annual_all[data_annual_all$variable %in% var_i$variable,], 
         aes(x=year, y=value), size=1) +
    geom_line(size=2) +
    labs(x="", y="", title=var_i$name) +
    theme_minimal() +
    theme(plot.title = element_text(hjust=0.5, face="bold")) +
    scale_x_continuous(breaks = seq(1992, 2016, 2),
                       limits = c(1992, 2016))
  
  ggsave(fig, filename = file.path(figures_file_path,"trends_annual", paste0("trends_", var_i$variable,".png")), width=WIDTH, height=HEIGHT)

}

#### NDVI
ndvi_fig <- ggplot(data_annual_all[data_annual_all$variable %in% c("ndvi", "ndvi_cropland"),], 
       aes(x=year, y=value, group=variable, color=variable), size=1) +
  geom_line(size=2) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face="bold")) +
  labs(x="", y="", title="Average NDVI") + 
  scale_color_manual(name = "", values = c("dodgerblue4", "darkorange2"),
                     labels = c("NDVI", "NDVI in\nCropland Areas")) +
  scale_x_continuous(breaks = seq(2000, 2016, 2),
                     limits = c(2000, 2016))
ggsave(ndvi_fig,  filename = file.path(figures_file_path,"trends_annual", "trends_ndvi.png"), width=WIDTH, height=HEIGHT)





