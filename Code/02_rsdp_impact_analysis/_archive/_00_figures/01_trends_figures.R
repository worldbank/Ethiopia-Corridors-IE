# Impact of Expressway Expansion
# Ethiopia IE

# Setup ------------------------------------------------------------------------
if(Sys.info()[["user"]] == "r521633") project_file_path <- "/home/wb521633/IEs/Ethiopia IE"
if(Sys.info()[["user"]] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE"
if(Sys.info()[["user"]] == "robmarty") project_file_path <- "~/Dropbox/World Bank/IEs/Ethiopia IE"

set.seed(42)

library(lfe)
library(reshape)
library(dplyr)
library(ggplot2)
library(data.table)

DIST_THRESH <- 5 #km

# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(project_file_path, "Data", "FinalData", "dmspols_grid_dataset", "dmspols_level_dataset.Rds"))
#data <- readRDS(file.path(project_file_path, "Data", "FinalData", "dmspols_grid_dataset", "dmspols_level_dataset_evenyears.Rds"))

data <- subset(data, select=-c(viirs,long,lat,GADM_ID_1,GADM_ID_2,MA_constantpop,
                               distance_asphaltconcrete,distance_earth,
                               distance_gravel,distance_majorgravel,distance_cobblestone))

# Summarize by Year ------------------------------------------------------------
data_annual <- data[, list(MA_constantpop_mean = mean(MA_constantpop, na.rm = TRUE),
                           MA_constantpop_median = median(MA_constantpop, na.rm = TRUE),
                           
                           N_road_type_asphaltconcrete_mean = sum(distance_asphaltconcrete < DIST_THRESH*1000, na.rm = TRUE),
                           N_road_type_earth_mean = sum(distance_earth < DIST_THRESH*1000, na.rm = TRUE),
                           N_road_type_gravel_mean = sum(distance_gravel < DIST_THRESH*1000, na.rm = TRUE),
                           N_road_type_majorgravel_mean = sum(distance_majorgravel < DIST_THRESH*1000, na.rm = TRUE),
                           N_road_type_cobblestone_mean = sum(distance_cobblestone < DIST_THRESH*1000, na.rm = TRUE),
                           
                           distance_road_type_asphaltconcrete_mean = mean(distance_asphaltconcrete, na.rm = TRUE)/1000,
                           distance_road_type_earth_mean = mean(distance_earth, na.rm = TRUE)/1000,
                           distance_road_type_gravel_mean = mean(distance_gravel, na.rm = TRUE)/1000,
                           distance_road_type_majorgravel_mean = mean(distance_majorgravel, na.rm = TRUE)/1000,
                           distance_road_type_cobblestone_mean = mean(distance_cobblestone, na.rm = TRUE)/1000,
                           
                           distance_road_speed_10_mean = mean(distance_road_speed_10, na.rm = TRUE)/1000,
                           distance_road_speed_15_mean = mean(distance_road_speed_15, na.rm = TRUE)/1000,
                           distance_road_speed_20_mean = mean(distance_road_speed_20, na.rm = TRUE)/1000,
                           distance_road_speed_25_mean = mean(distance_road_speed_25, na.rm = TRUE)/1000,
                           distance_road_speed_30_mean = mean(distance_road_speed_30, na.rm = TRUE)/1000,
                           distance_road_speed_35_mean = mean(distance_road_speed_35, na.rm = TRUE)/1000,
                           distance_road_speed_45_mean = mean(distance_road_speed_45, na.rm = TRUE)/1000,
                           distance_road_speed_50_mean = mean(distance_road_speed_50, na.rm = TRUE)/1000,
                           distance_road_speed_70_mean = mean(distance_road_speed_70, na.rm = TRUE)/1000,
                           distance_road_speed_120_mean = mean(distance_road_speed_120, na.rm = TRUE)/1000,
                           
                           N_road_speed_10_mean = sum(distance_road_speed_10 < DIST_THRESH*1000, na.rm = TRUE),
                           N_road_speed_15_mean = sum(distance_road_speed_15 < DIST_THRESH*1000, na.rm = TRUE),
                           N_road_speed_20_mean = sum(distance_road_speed_20 < DIST_THRESH*1000, na.rm = TRUE),
                           N_road_speed_25_mean = sum(distance_road_speed_25 < DIST_THRESH*1000, na.rm = TRUE),
                           N_road_speed_30_mean = sum(distance_road_speed_30 < DIST_THRESH*1000, na.rm = TRUE),
                           N_road_speed_35_mean = sum(distance_road_speed_35 < DIST_THRESH*1000, na.rm = TRUE),
                           N_road_speed_45_mean = sum(distance_road_speed_45 < DIST_THRESH*1000, na.rm = TRUE),
                           N_road_speed_50_mean = sum(distance_road_speed_50 < DIST_THRESH*1000, na.rm = TRUE),
                           N_road_speed_70_mean = sum(distance_road_speed_70 < DIST_THRESH*1000, na.rm = TRUE),
                           N_road_speed_120_mean = sum(distance_road_speed_120 < DIST_THRESH*1000, na.rm = TRUE)),by = year] %>% as.data.frame
data_annual <- melt(data_annual, id=c("year"))
rm(data)
# Figures ----------------------------------------------------------------------

#### Number of Cells Near Roads
data_annual_Nspeed <- data_annual[grepl("N_road_speed", data_annual$variable),]
data_annual_Nspeed$speed <- gsub("[[:alpha:]]|_", "", data_annual_Nspeed$variable) %>% as.numeric %>% as.factor

N_cells_by_speed <- ggplot(data=data_annual_Nspeed[data_annual_Nspeed$year %in% 1996:2016,],
                              aes(x=year, y=value, group=speed, color=speed)) +
  geom_line(size=2.2,color="black") + 
  geom_line(size=1.85) + 
  labs(x="", y="", 
       title="Number of Cells within 1km of Road\nBy Road Speed Limit", 
       color="Speed Limit") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size=16, face="bold"),
        axis.title = element_text(size=13),
        axis.text = element_text(size=13)) +
  scale_x_continuous(breaks=seq(1996,2016,4)) +
  scale_y_continuous(labels = scales::comma) + 
  scale_color_manual(values=c("firebrick1","dodgerblue2","forestgreen","darkorchid","darkorange1",
                              "yellow1","tan3","hotpink","lightsteelblue","wheat"))
#N_cells_by_speed
ggsave(N_cells_by_speed, file=file.path(project_file_path, "Results", "Figures", "N_cells_by_speed.png"), height=4.5,width=6.5)



#### Market Access
MA_constantpop_fig <- ggplot(data=data_annual[data_annual$variable == "MA_constantpop_median",], aes(x=year, y=value)) +
  geom_line(size=1) + 
  geom_point(size=2) +
  theme_minimal() +
  labs(x="",y="Market Access",title="Market Access [Constant Population]", 
       caption="Median market access calculated for each year.") +
  theme(plot.title = element_text(hjust = 0.5, size=16, face="bold"),
        axis.title = element_text(size=13),
        axis.text = element_text(size=13)) +
  scale_x_continuous(breaks=seq(1996,2016,4))
ggsave(MA_constantpop_fig, file=file.path(project_file_path, "Results", "Figures", "MA_constantpop_trend.png"), height=4.5,width=6.5)

#### Distance to Road: By Speed
data_annual_speedvars <- data_annual[grepl("distance_road_speed_", data_annual$variable),]
data_annual_speedvars$speed <- gsub("[[:alpha:]]|_", "", data_annual_speedvars$variable) %>% as.numeric %>% as.factor

avg_dist_road_speed <- ggplot(data=data_annual_speedvars,
       aes(x=year, y=value, group=speed, color=speed)) +
  geom_line(size=2) + 
  labs(x="", y="Kilometers", title="Average Distance to Road\nBy Road Speed Limit", color="Speed Limit") +
  theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size=16, face="bold"),
          axis.title = element_text(size=13),
          axis.text = element_text(size=13)) +
    scale_x_continuous(breaks=seq(1996,2016,4)) +
  scale_color_brewer(palette="Set1")
ggsave(avg_dist_road_speed, file=file.path(project_file_path, "Results", "Figures", "avg_dist_road_speed.png"), height=4.5,width=6.5)

#### Number of Cells: By Speed
data_annual_speedvars <- data_annual[grepl("N_road_speed_", data_annual$variable),]
data_annual_speedvars$speed <- gsub("[[:alpha:]]|_", "", data_annual_speedvars$variable) %>% as.numeric %>% as.factor

N_road_speed <- ggplot(data=data_annual_speedvars,
                              aes(x=year, y=value, group=speed, color=speed)) +
  geom_line(size=2) + 
  labs(x="", y="Kilometers", title="Average Distance to Road\nBy Road Speed Limit", color="Speed Limit") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size=16, face="bold"),
        axis.title = element_text(size=13),
        axis.text = element_text(size=13)) +
  scale_x_continuous(breaks=seq(1996,2016,4)) +
  scale_color_brewer(palette="Set1")
ggsave(avg_dist_road_speed, file=file.path(project_file_path, "Results", "Figures", "avg_dist_road_speed.png"), height=4.5,width=6.5)


#### Distance to Road: By Type
data_annual_typevars <- data_annual[grepl("distance_road_type_", data_annual$variable),]
data_annual_typevars$type <- gsub("distance_road_type_|_mean", "", data_annual_typevars$variable)

data_annual_typevars$type[data_annual_typevars$type == "asphaltconcrete"] <- "Asphalt Concrete"
data_annual_typevars$type[data_annual_typevars$type == "earth"] <- "Earth"
data_annual_typevars$type[data_annual_typevars$type == "gravel"] <- "Gravel"
data_annual_typevars$type[data_annual_typevars$type == "majorgravel"] <- "Major Gravel"
data_annual_typevars$type[data_annual_typevars$type == "cobblestone"] <- "Cobblestone"

avg_dist_road_type <- ggplot(data=data_annual_typevars,
                        aes(x=year, y=value, group=type, color=type)) +
  geom_line(size=2) + 
  labs(x="", y="Kilometers", title="Average Distance to Road\nBy Road Type",color="Type") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size=16, face="bold"),
        axis.title = element_text(size=13),
        axis.text = element_text(size=13)) +
  scale_x_continuous(breaks=seq(1996,2016,4)) +
  scale_color_brewer(palette="Set1")
avg_dist_road_type
ggsave(avg_dist_road_type, file=file.path(project_file_path, "Results", "Figures", "avg_dist_road_type.png"), height=4.5,width=6.5)

#### Number of Cells: By Type
data_annual_typevars <- data_annual[grepl("N_road_type_", data_annual$variable),]
data_annual_typevars$type <- gsub("N_road_type_|_mean", "", data_annual_typevars$variable)

data_annual_typevars$type[data_annual_typevars$type == "asphaltconcrete"] <- "Asphalt Concrete"
data_annual_typevars$type[data_annual_typevars$type == "earth"] <- "Earth"
data_annual_typevars$type[data_annual_typevars$type == "gravel"] <- "Gravel"
data_annual_typevars$type[data_annual_typevars$type == "majorgravel"] <- "Major Gravel"
data_annual_typevars$type[data_annual_typevars$type == "cobblestone"] <- "Cobblestone"

N_road_type <- ggplot(data=data_annual_typevars,
                             aes(x=year, y=value, group=type, color=type)) +
  geom_line(size=2) + 
  labs(x="", y="Kilometers", title="Average Distance to Road\nBy Road Type",color="Type") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size=16, face="bold"),
        axis.title = element_text(size=13),
        axis.text = element_text(size=13)) +
  scale_x_continuous(breaks=seq(1996,2016,4)) +
  scale_color_brewer(palette="Set1")
N_road_type
ggsave(avg_dist_road_type, file=file.path(project_file_path, "Results", "Figures", "avg_dist_road_type.png"), height=4.5,width=6.5)



