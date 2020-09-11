# Impact of Expressway Expansion
# Ethiopia IE

gc()
# Setup ------------------------------------------------------------------------
if(Sys.info()[["user"]] == "r521633") project_file_path <- "/home/wb521633/IEs/Ethiopia IE"
if(Sys.info()[["user"]] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE"
if(Sys.info()[["user"]] == "robmarty") project_file_path <- "~/Dropbox/World Bank/IEs/Ethiopia IE"

library(lfe)
library(stargazer)
library(dplyr)
library(data.table)
library(ggplot2)
library(scales)
library(Rfast)
library(stringr)

DIST_THRESH <- 5
RANDOM_SUBSET <- TRUE

# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(project_file_path, "Data", "FinalData", "dmspols_grid_dataset", "dmspols_level_dataset.Rds"))

# Subset to random sample
if(RANDOM_SUBSET == TRUE){
  cells_to_keep <- sample(x=unique(data$cell_id), size=round(length(unique(data$cell_id))*0.30))
  data <- data[data$cell_id %in% cells_to_keep,]
}

# Treatment variables
data$near_any_road <- rowMins(as.matrix(subset(data, select=c(distance_road_speed_10, distance_road_speed_15, distance_road_speed_20, distance_road_speed_25, distance_road_speed_30, distance_road_speed_35, distance_road_speed_45, distance_road_speed_50, distance_road_speed_70, distance_road_speed_120))),value=T) < DIST_THRESH*1000
data$near_70kmhrmore_road <- rowMins(as.matrix(subset(data, select=c(distance_road_speed_70, distance_road_speed_120))),value=T) < DIST_THRESH*1000
data$near_50kmhrmore_road <- rowMins(as.matrix(subset(data, select=c(distance_road_speed_50, distance_road_speed_70, distance_road_speed_120))),value=T) < DIST_THRESH*1000
data$near_45kmhrmore_road <- rowMins(as.matrix(subset(data, select=c(distance_road_speed_45, distance_road_speed_50, distance_road_speed_70, distance_road_speed_120))),value=T) < DIST_THRESH*1000
data$near_30kmhrmore_road <- rowMins(as.matrix(subset(data, select=c(distance_road_speed_30, distance_road_speed_35, distance_road_speed_45, distance_road_speed_50, distance_road_speed_70, distance_road_speed_120))),value=T) < DIST_THRESH*1000

data$near_any_road[is.na(data$near_any_road)] <- FALSE
data$near_70kmhrmore_road[is.na(data$near_70kmhrmore_road)] <- FALSE
data$near_50kmhrmore_road[is.na(data$near_50kmhrmore_road)] <- FALSE
data$near_45kmhrmore_road[is.na(data$near_45kmhrmore_road)] <- FALSE
data$near_30kmhrmore_road[is.na(data$near_30kmhrmore_road)] <- FALSE

# Baseline/Endline
data <- data %>%
  group_by(cell_id) %>% 
  mutate(near_70kmhrmore_road_1996 = near_70kmhrmore_road[year==1996]) %>% 
  mutate(near_70kmhrmore_road_2012 = near_70kmhrmore_road[year==2012]) %>% 
  mutate(near_50kmhrmore_road_1996 = near_50kmhrmore_road[year==1996]) %>% 
  mutate(near_50kmhrmore_road_2012 = near_50kmhrmore_road[year==2012]) %>% 
  mutate(near_45kmhrmore_road_1996 = near_45kmhrmore_road[year==1996]) %>% 
  mutate(near_45kmhrmore_road_2012 = near_45kmhrmore_road[year==2012]) %>% 
  mutate(near_30kmhrmore_road_1996 = near_30kmhrmore_road[year==1996]) %>% 
  mutate(near_30kmhrmore_road_2012 = near_30kmhrmore_road[year==2012]) %>% 
  as.data.table

# 30
data_all_nearroad2012_30 <- data[data$near_30kmhrmore_road_2012 %in% TRUE & data$near_30kmhrmore_road_1996 == FALSE,][,list(dmspols_mean = mean(dmspols, na.rm = TRUE),
                                                                                                                            globcover_urban_mean = mean(globcover_urban, na.rm = TRUE),
                                                                                                                            globcover_cropland_mean = mean(globcover_cropland, na.rm = TRUE)),
                                                                                                                      by = year] %>% 
  as.data.frame() %>%
  mutate(speed = ">30km") %>%
  mutate(baseend = "In 2012, Not 1996") %>%
  mutate(type = "Near Road >30km in 2012, Not in 1996")

data_all_notnearroad2012_30 <- data[data$near_30kmhrmore_road_1996 %in% TRUE,][,list(dmspols_mean = mean(dmspols, na.rm = TRUE),
                                                                                     globcover_urban_mean = mean(globcover_urban, na.rm = TRUE),
                                                                                     globcover_cropland_mean = mean(globcover_cropland, na.rm = TRUE)),
                                                                               by = year] %>% 
  as.data.frame() %>%
  mutate(speed = ">30km") %>%
  mutate(baseend = "In 1996") %>%
  mutate(type = "Not Near Road >30km in 1996")

# 45
data_all_nearroad2012_45 <- data[data$near_45kmhrmore_road_2012 %in% TRUE & data$near_45kmhrmore_road_1996 == FALSE,][,list(dmspols_mean = mean(dmspols, na.rm = TRUE),
                                                                                                                            globcover_urban_mean = mean(globcover_urban, na.rm = TRUE),
                                                                                                                            globcover_cropland_mean = mean(globcover_cropland, na.rm = TRUE)),
                                                                                                                      by = year] %>% 
  as.data.frame() %>%
  mutate(speed = ">45km") %>%
  mutate(baseend = "In 2012, Not 1996") %>%
  mutate(type = "Near Road >45km in 2012, Not in 1996")

data_all_notnearroad2012_45 <- data[data$near_45kmhrmore_road_1996 %in% TRUE,][,list(dmspols_mean = mean(dmspols, na.rm = TRUE),
                                                                                     globcover_urban_mean = mean(globcover_urban, na.rm = TRUE),
                                                                                     globcover_cropland_mean = mean(globcover_cropland, na.rm = TRUE)),
                                                                               by = year] %>% 
  as.data.frame() %>%
  mutate(speed = ">45km") %>%
  mutate(baseend = "In 1996") %>%
  mutate(type = "Not Near Road >45km in 1996")

# 50
data_all_nearroad2012_50 <- data[data$near_50kmhrmore_road_2012 %in% TRUE & data$near_50kmhrmore_road_1996 == FALSE,][,list(dmspols_mean = mean(dmspols, na.rm = TRUE),
                                                                                                                            globcover_urban_mean = mean(globcover_urban, na.rm = TRUE),
                                                                                                                            globcover_cropland_mean = mean(globcover_cropland, na.rm = TRUE)),
                                                                                                                      by = year] %>% 
  as.data.frame() %>%
  mutate(speed = ">50km") %>%
  mutate(baseend = "In 2012, Not 1996") %>%
  mutate(type = "Near Road >50km in 2012, Not in 1996")

data_all_notnearroad2012_50 <- data[data$near_50kmhrmore_road_1996 %in% TRUE,][,list(dmspols_mean = mean(dmspols, na.rm = TRUE),
                                                                                     globcover_urban_mean = mean(globcover_urban, na.rm = TRUE),
                                                                                     globcover_cropland_mean = mean(globcover_cropland, na.rm = TRUE)),
                                                                               by = year] %>% 
  as.data.frame() %>%
  mutate(speed = ">50km") %>%
  mutate(baseend = "In 1996") %>%
  mutate(type = "Not Near Road >50km in 1996")

# 70
data_all_nearroad2012_70 <- data[data$near_70kmhrmore_road_2012 %in% TRUE & data$near_70kmhrmore_road_1996 == FALSE,][,list(dmspols_mean = mean(dmspols, na.rm = TRUE),
                                                                                                                            globcover_urban_mean = mean(globcover_urban, na.rm = TRUE),
                                                                                                                            globcover_cropland_mean = mean(globcover_cropland, na.rm = TRUE)),
                                                                                                                      by = year] %>% 
  as.data.frame() %>%
  mutate(speed = ">70km") %>%
  mutate(baseend = "In 2012, Not 1996") %>%
  mutate(type = "Near Road >70km in 2012, Not in 1996")

data_all_notnearroad2012_70 <- data[data$near_70kmhrmore_road_1996 %in% TRUE,][,list(dmspols_mean = mean(dmspols, na.rm = TRUE),
                                                                                     globcover_urban_mean = mean(globcover_urban, na.rm = TRUE),
                                                                                     globcover_cropland_mean = mean(globcover_cropland, na.rm = TRUE)),
                                                                               by = year] %>% 
  as.data.frame() %>%
  mutate(speed = ">70km") %>%
  mutate(baseend = "In 1996") %>%
  mutate(type = "Not Near Road >50km in 1996")

data_all_annual <- data[,list(dmspols_mean = mean(dmspols, na.rm = TRUE),
                                                                                     globcover_urban_mean = mean(globcover_urban, na.rm = TRUE),
                                                                                     globcover_cropland_mean = mean(globcover_cropland, na.rm = TRUE)),
                                                                               by = year] %>% 
  as.data.frame()

data_annual <- bind_rows(data_all_nearroad2012_30, data_all_notnearroad2012_30,
                         data_all_nearroad2012_45, data_all_notnearroad2012_45,
                         #data_all_nearroad2012_50, data_all_notnearroad2012_50,
                         data_all_nearroad2012_70, data_all_notnearroad2012_70)
rm(data)

fig_dmspols <- ggplot(data=data_annual, aes(x=year, y=dmspols_mean)) +
  geom_line(aes(color=speed, linetype=baseend), size=1) +
  #geom_line(data=data_all_annual, aes(x=year,y=dmspols_mean, alpha="All Cells"), size=1.5,color="orange") +
  #scale_alpha_manual(values=1) + 
  theme_minimal() +
  labs(x="", 
       y="", 
       title = "Average Luminosity (DMSP-OLS)",
       color="Speed of Roads",
       linetype="When Near Road",
       alpha="") +
  theme(plot.title = element_text(size=12, face="bold", hjust=0.5),
        axis.text = element_text(size=12)) +
  scale_x_continuous(breaks= pretty_breaks(10), limits=c(1996,2016))
ggsave(fig_dmspols, file=file.path(project_file_path, "Results", "Figures", "dmspols_trends.png"), height=3,width=7.5)

fig_urban <- ggplot(data=data_annual, aes(x=year, y=globcover_urban_mean)) +
  geom_line(aes(color=speed, linetype=baseend), size=1) +
  #geom_line(data=data_all_annual, aes(x=year,y=globcover_urban_mean, alpha="All Cells"), size=1.5,color="orange") +
  #scale_alpha_manual(values=1) + 
  theme_minimal() +
  labs(x="", 
       y="", 
       title = "Proportion Urban (Globcover)",
       color="Speed of Roads",
       linetype="When Near Road",
       alpha="") +
  theme(plot.title = element_text(size=12, face="bold", hjust=0.5),
        axis.text = element_text(size=12)) +
  scale_x_continuous(breaks= pretty_breaks(10), limits=c(1996,2016))
ggsave(fig_urban, file=file.path(project_file_path, "Results", "Figures", "urban_trends.png"), height=3,width=7.5)

fig_cropland <- ggplot(data=data_annual, aes(x=year, y=globcover_cropland_mean)) +
  geom_line(aes(color=speed, linetype=baseend), size=1) +
  #geom_line(data=data_all_annual, aes(x=year,y=globcover_cropland_mean, alpha="All Cells"), size=1.5,color="orange") +
  #scale_alpha_manual(values=1) + 
  theme_minimal() +
  labs(x="", 
       y="", 
       title = "Proportion Cropland (Globcover)",
       color="Speed of Roads",
       linetype="When Near Road",
       alpha="") +
  theme(plot.title = element_text(size=12, face="bold", hjust=0.5),
        axis.text = element_text(size=12)) +
  scale_x_continuous(breaks= pretty_breaks(10), limits=c(1996,2016))
ggsave(fig_cropland, file=file.path(project_file_path, "Results", "Figures", "cropland_trends.png"), height=3,width=7.5)

