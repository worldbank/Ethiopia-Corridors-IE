# Trends in Distance to Road Types 

# Setup ------------------------------------------------------------------------
if(Sys.info()[["user"]] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE"
if(Sys.info()[["user"]] == "robmarty") project_file_path <- "~/Dropbox/World Bank/IEs/Ethiopia IE"

library(ggplot2)
library(dplyr)
library(doBy)
library(reshape)

# Load Data --------------------------------------------------------------------
df <- readRDS(file.path(project_file_path, "Data", "FinalData", "dmspols_grid_dataset", "dmspols_level_dataset.Rds"))
df <- df[df$year %in% seq(1996, 2016, 2),]

df_sum <- summaryBy(distance_asphaltconcrete + distance_earth + distance_gravel + distance_majorgravel + distance_cobblestone +
                 distance_road_speed_10 + distance_road_speed_15 + distance_road_speed_20 + distance_road_speed_25 +
                 distance_road_speed_30 + distance_road_speed_35 + distance_road_speed_45 + distance_road_speed_50 + 
                 distance_road_speed_70 + distance_road_speed_120 ~ year, FUN=mean, data=df, keep.names=T, na.rm=T)
df_sum_stack <- melt(df_sum, id=c("year"))

df_sum_stack$speed <- NA
for(speed in c("10","15","20","25","30","35","45","50","70","120")){
  df_sum_stack$speed[df_sum_stack$variable == paste0("distance_road_speed_",speed)] <- speed
}

ggplot() +
  geom_line(data=df_sum_stack[grepl("distance_road_speed_", df_sum_stack$variable),], 
            aes(x=year, y=value, group=variable, color=variable), size=1) +
  geom_text(data=df_sum_stack[grepl("distance_road_speed_", df_sum_stack$variable) & df_sum_stack$year == 2016,],
            aes(x=year+.5, y=value, label=speed, group=variable, color=variable)) +
  labs(x="") +
  theme_minimal()


