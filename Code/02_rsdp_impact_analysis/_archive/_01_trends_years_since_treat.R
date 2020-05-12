# Impact of Expressway Expansion
# Ethiopia IE

# Setup ------------------------------------------------------------------------
if(Sys.info()[["user"]] == "r521633") project_file_path <- "/home/wb521633/IEs/Ethiopia IE"
if(Sys.info()[["user"]] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE"
if(Sys.info()[["user"]] == "robmarty") project_file_path <- "~/Dropbox/World Bank/IEs/Ethiopia IE"

library(lfe)
library(reshape)
library(dplyr)
library(ggplot2)
library(data.table)

set.seed(42)
RANDOM_SUBSET <- TRUE
DIST_THRESH <- 5 #km

# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(project_file_path, "Data", "FinalData", "dmspols_grid_dataset", "dmspols_level_dataset.Rds"))
data <- data[(data$year >= 1996) & (data$year <= 2016),]

# Subset to random sample
if(RANDOM_SUBSET == TRUE){
  cells_to_keep <- sample(x=unique(data$cell_id), size=round(length(unique(data$cell_id))*0.25))
  data <- data[data$cell_id %in% cells_to_keep,]
}

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

# Figures ----------------------------------------------------------------------
#### All Data

data_annual_anyroad <- data[data$year >= 1998,][, list(MA_constantpop_mean = mean(MA_constantpop, na.rm = TRUE),
                                                    MA_constantpop_median = median(MA_constantpop, na.rm = TRUE),
                                                    globcover_urban_mean = mean(globcover_urban, na.rm = TRUE),
                                                    globcover_cropland_mean = mean(globcover_cropland, na.rm = TRUE),
                                                    dmspols_mean = mean(dmspols, na.rm = TRUE),
                                                    dmspols_median = median(dmspols, na.rm = TRUE),
                                                    viirs_mean = mean(viirs, na.rm = TRUE)),
                                             by = near_any_road_years_since_treat] %>% 
  as.data.frame %>% 
  dplyr::rename(years_since_treat = near_any_road_years_since_treat)

data_annual_30km <- data[data$year >= 1998,][, list(MA_constantpop_mean = mean(MA_constantpop, na.rm = TRUE),
                                                    MA_constantpop_median = median(MA_constantpop, na.rm = TRUE),
                                                    globcover_urban_mean = mean(globcover_urban, na.rm = TRUE),
                                                    globcover_cropland_mean = mean(globcover_cropland, na.rm = TRUE),
                                                    dmspols_mean = mean(dmspols, na.rm = TRUE),
                                                    dmspols_median = median(dmspols, na.rm = TRUE),
                                                    viirs_mean = mean(viirs, na.rm = TRUE)),
                                             by = near_30kmhrmore_road_years_since_treat] %>% 
  as.data.frame %>%
  dplyr::rename(years_since_treat = near_30kmhrmore_road_years_since_treat)

data_annual_45km <- data[data$year >= 1998,][, list(MA_constantpop_mean = mean(MA_constantpop, na.rm = TRUE),
                                                    MA_constantpop_median = median(MA_constantpop, na.rm = TRUE),
                                                    globcover_urban_mean = mean(globcover_urban, na.rm = TRUE),
                                                    globcover_cropland_mean = mean(globcover_cropland, na.rm = TRUE),
                                                    dmspols_mean = mean(dmspols, na.rm = TRUE),
                                                    dmspols_median = median(dmspols, na.rm = TRUE),
                                                    viirs_mean = mean(viirs, na.rm = TRUE)),
                                             by = near_45kmhrmore_road_years_since_treat] %>% 
  as.data.frame %>%
  dplyr::rename(years_since_treat = near_45kmhrmore_road_years_since_treat)

data_annual_50km <- data[data$year >= 1998,][, list(MA_constantpop_mean = mean(MA_constantpop, na.rm = TRUE),
                                           MA_constantpop_median = median(MA_constantpop, na.rm = TRUE),
                                           globcover_urban_mean = mean(globcover_urban, na.rm = TRUE),
                                           globcover_cropland_mean = mean(globcover_cropland, na.rm = TRUE),
                                           dmspols_mean = mean(dmspols, na.rm = TRUE),
                                           dmspols_median = median(dmspols, na.rm = TRUE),
                                           viirs_mean = mean(viirs, na.rm = TRUE)),
                                         by = near_50kmhrmore_road_years_since_treat] %>% 
  as.data.frame %>%
  dplyr::rename(years_since_treat = near_50kmhrmore_road_years_since_treat)

data_annual_70km <- data[data$year >= 1998,][, list(MA_constantpop_mean = mean(MA_constantpop, na.rm = TRUE),
                                                   MA_constantpop_median = median(MA_constantpop, na.rm = TRUE),
                                                   globcover_urban_mean = mean(globcover_urban, na.rm = TRUE),
                                                   globcover_cropland_mean = mean(globcover_cropland, na.rm = TRUE),
                                                   dmspols_mean = mean(dmspols, na.rm = TRUE),
                                                   dmspols_median = median(dmspols, na.rm = TRUE),
                                                   viirs_mean = mean(viirs, na.rm = TRUE)),
                                            by = near_70kmhrmore_road_years_since_treat] %>% 
  as.data.frame %>%
  dplyr::rename(years_since_treat = near_70kmhrmore_road_years_since_treat)

#### Baseline NTL == 0
data_annual_anyroad_ntl0 <- data[data$year >= 1998 & data$dmspols_1996 %in% 0,][, list(MA_constantpop_mean = mean(MA_constantpop, na.rm = TRUE),
                                                       MA_constantpop_median = median(MA_constantpop, na.rm = TRUE),
                                                       globcover_urban_mean = mean(globcover_urban, na.rm = TRUE),
                                                       globcover_cropland_mean = mean(globcover_cropland, na.rm = TRUE),
                                                       dmspols_mean = mean(dmspols, na.rm = TRUE),
                                                       dmspols_median = median(dmspols, na.rm = TRUE),
                                                       viirs_mean = mean(viirs, na.rm = TRUE)),
                                                by = near_any_road_years_since_treat] %>% 
  as.data.frame %>% 
  dplyr::rename(years_since_treat = near_any_road_years_since_treat)

data_annual_30km_ntl0 <- data[data$year >= 1998 & data$dmspols_1996 %in% 0,][, list(MA_constantpop_mean = mean(MA_constantpop, na.rm = TRUE),
                                                    MA_constantpop_median = median(MA_constantpop, na.rm = TRUE),
                                                    globcover_urban_mean = mean(globcover_urban, na.rm = TRUE),
                                                    globcover_cropland_mean = mean(globcover_cropland, na.rm = TRUE),
                                                    dmspols_mean = mean(dmspols, na.rm = TRUE),
                                                    dmspols_median = median(dmspols, na.rm = TRUE),
                                                    viirs_mean = mean(viirs, na.rm = TRUE)),
                                             by = near_30kmhrmore_road_years_since_treat] %>% 
  as.data.frame %>%
  dplyr::rename(years_since_treat = near_30kmhrmore_road_years_since_treat)

data_annual_45km_ntl0 <- data[data$year >= 1998 & data$dmspols_1996 %in% 0,][, list(MA_constantpop_mean = mean(MA_constantpop, na.rm = TRUE),
                                                    MA_constantpop_median = median(MA_constantpop, na.rm = TRUE),
                                                    globcover_urban_mean = mean(globcover_urban, na.rm = TRUE),
                                                    globcover_cropland_mean = mean(globcover_cropland, na.rm = TRUE),
                                                    dmspols_mean = mean(dmspols, na.rm = TRUE),
                                                    dmspols_median = median(dmspols, na.rm = TRUE),
                                                    viirs_mean = mean(viirs, na.rm = TRUE)),
                                             by = near_45kmhrmore_road_years_since_treat] %>% 
  as.data.frame %>%
  dplyr::rename(years_since_treat = near_45kmhrmore_road_years_since_treat)

data_annual_50km_ntl0 <- data[data$year >= 1998 & data$dmspols_1996 %in% 0,][, list(MA_constantpop_mean = mean(MA_constantpop, na.rm = TRUE),
                                                    MA_constantpop_median = median(MA_constantpop, na.rm = TRUE),
                                                    globcover_urban_mean = mean(globcover_urban, na.rm = TRUE),
                                                    globcover_cropland_mean = mean(globcover_cropland, na.rm = TRUE),
                                                    dmspols_mean = mean(dmspols, na.rm = TRUE),
                                                    dmspols_median = median(dmspols, na.rm = TRUE),
                                                    viirs_mean = mean(viirs, na.rm = TRUE)),
                                             by = near_50kmhrmore_road_years_since_treat] %>% 
  as.data.frame %>%
  dplyr::rename(years_since_treat = near_50kmhrmore_road_years_since_treat)

data_annual_70km_ntl0 <- data[data$year >= 1998 & data$dmspols_1996 %in% 0,][, list(MA_constantpop_mean = mean(MA_constantpop, na.rm = TRUE),
                                                    MA_constantpop_median = median(MA_constantpop, na.rm = TRUE),
                                                    globcover_urban_mean = mean(globcover_urban, na.rm = TRUE),
                                                    globcover_cropland_mean = mean(globcover_cropland, na.rm = TRUE),
                                                    dmspols_mean = mean(dmspols, na.rm = TRUE),
                                                    dmspols_median = median(dmspols, na.rm = TRUE),
                                                    viirs_mean = mean(viirs, na.rm = TRUE)),
                                             by = near_70kmhrmore_road_years_since_treat] %>% 
  as.data.frame %>%
  dplyr::rename(years_since_treat = near_70kmhrmore_road_years_since_treat)

#### Baseline NTL > 0
data_annual_anyroad_ntlp <- data[data$year >= 1998 & data$dmspols_1996 > 0,][, list(MA_constantpop_mean = mean(MA_constantpop, na.rm = TRUE),
                                                                                       MA_constantpop_median = median(MA_constantpop, na.rm = TRUE),
                                                                                       globcover_urban_mean = mean(globcover_urban, na.rm = TRUE),
                                                                                       globcover_cropland_mean = mean(globcover_cropland, na.rm = TRUE),
                                                                                       dmspols_mean = mean(dmspols, na.rm = TRUE),
                                                                                       dmspols_median = median(dmspols, na.rm = TRUE),
                                                                                       viirs_mean = mean(viirs, na.rm = TRUE)),
                                                                                by = near_any_road_years_since_treat] %>% 
  as.data.frame %>% 
  dplyr::rename(years_since_treat = near_any_road_years_since_treat)

data_annual_30km_ntlp <- data[data$year >= 1998 & data$dmspols_1996 > 0,][, list(MA_constantpop_mean = mean(MA_constantpop, na.rm = TRUE),
                                                                                    MA_constantpop_median = median(MA_constantpop, na.rm = TRUE),
                                                                                    globcover_urban_mean = mean(globcover_urban, na.rm = TRUE),
                                                                                    globcover_cropland_mean = mean(globcover_cropland, na.rm = TRUE),
                                                                                    dmspols_mean = mean(dmspols, na.rm = TRUE),
                                                                                    dmspols_median = median(dmspols, na.rm = TRUE),
                                                                                    viirs_mean = mean(viirs, na.rm = TRUE)),
                                                                             by = near_30kmhrmore_road_years_since_treat] %>% 
  as.data.frame %>%
  dplyr::rename(years_since_treat = near_30kmhrmore_road_years_since_treat)

data_annual_45km_ntlp <- data[data$year >= 1998 & data$dmspols_1996 > 0,][, list(MA_constantpop_mean = mean(MA_constantpop, na.rm = TRUE),
                                                                                    MA_constantpop_median = median(MA_constantpop, na.rm = TRUE),
                                                                                    globcover_urban_mean = mean(globcover_urban, na.rm = TRUE),
                                                                                    globcover_cropland_mean = mean(globcover_cropland, na.rm = TRUE),
                                                                                    dmspols_mean = mean(dmspols, na.rm = TRUE),
                                                                                    dmspols_median = median(dmspols, na.rm = TRUE),
                                                                                    viirs_mean = mean(viirs, na.rm = TRUE)),
                                                                             by = near_45kmhrmore_road_years_since_treat] %>% 
  as.data.frame %>%
  dplyr::rename(years_since_treat = near_45kmhrmore_road_years_since_treat)

data_annual_50km_ntlp <- data[data$year >= 1998 & data$dmspols_1996 > 0,][, list(MA_constantpop_mean = mean(MA_constantpop, na.rm = TRUE),
                                                                                    MA_constantpop_median = median(MA_constantpop, na.rm = TRUE),
                                                                                    globcover_urban_mean = mean(globcover_urban, na.rm = TRUE),
                                                                                    globcover_cropland_mean = mean(globcover_cropland, na.rm = TRUE),
                                                                                    dmspols_mean = mean(dmspols, na.rm = TRUE),
                                                                                    dmspols_median = median(dmspols, na.rm = TRUE),
                                                                                    viirs_mean = mean(viirs, na.rm = TRUE)),
                                                                             by = near_50kmhrmore_road_years_since_treat] %>% 
  as.data.frame %>%
  dplyr::rename(years_since_treat = near_50kmhrmore_road_years_since_treat)

data_annual_70km_ntlp <- data[data$year >= 1998 & data$dmspols_1996 > 0,][, list(MA_constantpop_mean = mean(MA_constantpop, na.rm = TRUE),
                                                                                    MA_constantpop_median = median(MA_constantpop, na.rm = TRUE),
                                                                                    globcover_urban_mean = mean(globcover_urban, na.rm = TRUE),
                                                                                    globcover_cropland_mean = mean(globcover_cropland, na.rm = TRUE),
                                                                                    dmspols_mean = mean(dmspols, na.rm = TRUE),
                                                                                    dmspols_median = median(dmspols, na.rm = TRUE),
                                                                                    viirs_mean = mean(viirs, na.rm = TRUE)),
                                                                             by = near_70kmhrmore_road_years_since_treat] %>% 
  as.data.frame %>%
  dplyr::rename(years_since_treat = near_70kmhrmore_road_years_since_treat)

rm(data)

# Figures: Full Sample ---------------------------------------------------------

#### DMSP-OLS
years_since_treat_30_45_50_dmspols <- ggplot() +
  geom_line(data=data_annual_30km, aes(x=years_since_treat, y=dmspols_mean, color="Near 30km or Faster Road"), size=1.5) +
  geom_line(data=data_annual_45km, aes(x=years_since_treat, y=dmspols_mean, color="Near 45km or Faster Road"), size=1.5) +
  geom_line(data=data_annual_50km, aes(x=years_since_treat, y=dmspols_mean, color="Near 50km or Faster Road"), size=1.5) + 
  geom_vline(aes(xintercept=0)) +
  theme_minimal() + 
  labs(x="Years Since Cell Became Within 1km of Road", 
       y="Luminosity",
       color="") +
  theme(plot.title=element_text(hjust=0.5,face="bold"),
        axis.text = element_text(size=13),
        axis.title = element_text(size=13)) +
  scale_color_manual(values=c("dodgerblue1","forestgreen","darkorange"))
ggsave(years_since_treat_30_45_50_dmspols, file=file.path(project_file_path, "Results", "Figures", "years_since_treat_30_45_50_dmspols.png"), height=4,width=6.5)

years_since_treat_70_dmspols <- ggplot() +
  geom_line(data=data_annual_70km[data_annual_70km$years_since_treat < 10,], aes(x=years_since_treat, y=dmspols_mean, color="Near 70km or Faster Road"), size=1.5) + 
  geom_vline(aes(xintercept=0)) +
  theme_minimal() + 
  labs(x="Years Since Cell Became Within 1km of Road", 
       y="Luminosity",
       color="") +
  theme(plot.title=element_text(hjust=0.5,face="bold"),
        axis.text = element_text(size=13),
        axis.title = element_text(size=13)) +
  scale_color_manual(values=c("firebrick1"))
ggsave(years_since_treat_70_dmspols, file=file.path(project_file_path, "Results", "Figures", "years_since_treat_70_dmspols.png"), height=4,width=6.5)

#### Globcover-Urban
years_since_treat_30_45_50_urban <- ggplot() +
  geom_line(data=data_annual_30km, aes(x=years_since_treat, y=globcover_urban_mean, color="Near 30km or Faster Road"), size=1.5) +
  geom_line(data=data_annual_45km, aes(x=years_since_treat, y=globcover_urban_mean, color="Near 45km or Faster Road"), size=1.5) +
  geom_line(data=data_annual_50km, aes(x=years_since_treat, y=globcover_urban_mean, color="Near 50km or Faster Road"), size=1.5) + 
  geom_vline(aes(xintercept=0)) +
  theme_minimal() + 
  labs(x="Years Since Cell Became Within 1km of Road", 
       y="Proportion of Cells Urban",
       title="",
       color="") +
  theme(plot.title=element_text(hjust=0.5,face="bold"),
        axis.text = element_text(size=13),
        axis.title = element_text(size=13)) +
  scale_color_manual(values=c("dodgerblue1","forestgreen","darkorange"))
ggsave(years_since_treat_30_45_50_urban, file=file.path(project_file_path, "Results", "Figures", "years_since_treat_30_45_50_urban.png"), height=4,width=6.5)

years_since_treat_70_urban <- ggplot() +
   geom_line(data=data_annual_70km[data_annual_70km$years_since_treat < 10,], aes(x=years_since_treat, y=globcover_urban_mean, color="Near 70km or Faster Road"), size=1.5) + 
  geom_vline(aes(xintercept=0)) +
  theme_minimal() + 
  labs(x="Years Since Cell Became Within 1km of Road", 
       y="Proportion of Cells Urban",
       title="",
       color="") +
  theme(plot.title=element_text(hjust=0.5,face="bold"),
        axis.text = element_text(size=13),
        axis.title = element_text(size=13)) +
  scale_color_manual(values=c("firebrick1"))
ggsave(years_since_treat_70_urban, file=file.path(project_file_path, "Results", "Figures", "years_since_treat_70_urban.png"), height=4,width=6.5)

#### Globcover-Cropland
years_since_treat_30_45_50_cropland <- ggplot() +
  geom_line(data=data_annual_30km, aes(x=years_since_treat, y=globcover_cropland_mean, color="Near 30km or Faster Road"), size=1.5) +
  geom_line(data=data_annual_45km, aes(x=years_since_treat, y=globcover_cropland_mean, color="Near 45km or Faster Road"), size=1.5) +
  geom_line(data=data_annual_50km, aes(x=years_since_treat, y=globcover_cropland_mean, color="Near 50km or Faster Road"), size=1.5) + 
  geom_line(data=data_annual_70km[data_annual_70km$years_since_treat < 8,], aes(x=years_since_treat, y=globcover_cropland_mean, color="Near 70km or Faster Road"), size=1.5) + 
  geom_vline(aes(xintercept=0)) +
  theme_minimal() + 
  labs(x="Years Since Cell Became Within 1km of Road", 
       y="Proportion of Cells Cropland",
       title="",
       color="") +
  theme(plot.title=element_text(hjust=0.5,face="bold"),
        axis.text = element_text(size=13),
        axis.title = element_text(size=13)) +
  scale_color_manual(values=c("dodgerblue1","forestgreen","darkorange","firebrick1"))
ggsave(years_since_treat_30_45_50_cropland , file=file.path(project_file_path, "Results", "Figures", "years_since_treat_30_45_50_70_cropland.png"), height=4,width=6.5)

# Figures: Positive NTL at Baseline --------------------------------------------

#### DMSP-OLS
years_since_treat_30_45_50_dmspols_ntlp <- ggplot() +
  geom_line(data=data_annual_30km_ntlp, aes(x=years_since_treat, y=dmspols_mean, color="Near 30km or Faster Road"), size=1.5) +
  geom_line(data=data_annual_45km_ntlp, aes(x=years_since_treat, y=dmspols_mean, color="Near 45km or Faster Road"), size=1.5) +
  geom_line(data=data_annual_50km_ntlp, aes(x=years_since_treat, y=dmspols_mean, color="Near 50km or Faster Road"), size=1.5) + 
  geom_vline(aes(xintercept=0)) +
  theme_minimal() + 
  labs(x="Years Since Cell Became Within 1km of Road", 
       y="Luminosity",
       color="") +
  theme(plot.title=element_text(hjust=0.5,face="bold"),
        axis.text = element_text(size=13),
        axis.title = element_text(size=13)) +
  scale_color_manual(values=c("dodgerblue1","forestgreen","darkorange"))
ggsave(years_since_treat_30_45_50_dmspols_ntlp, file=file.path(project_file_path, "Results", "Figures", "years_since_treat_30_45_50_dmspols_ntlp.png"), height=4,width=6.5)

years_since_treat_70_dmspols_ntlp <- ggplot() +
  geom_line(data=data_annual_70km_ntlp[data_annual_70km_ntlp$years_since_treat < 10,], aes(x=years_since_treat, y=dmspols_mean, color="Near 70km or Faster Road"), size=1.5) + 
  geom_vline(aes(xintercept=0)) +
  theme_minimal() + 
  labs(x="Years Since Cell Became Within 1km of Road", 
       y="Luminosity",
       color="") +
  theme(plot.title=element_text(hjust=0.5,face="bold"),
        axis.text = element_text(size=13),
        axis.title = element_text(size=13)) +
  scale_color_manual(values=c("firebrick1"))
ggsave(years_since_treat_70_dmspols_ntlp, file=file.path(project_file_path, "Results", "Figures", "years_since_treat_70_dmspols_ntlp.png"), height=4,width=6.5)

#### Globcover-Urban
years_since_treat_30_45_50_urban_ntlp <- ggplot() +
  geom_line(data=data_annual_30km_ntlp, aes(x=years_since_treat, y=globcover_urban_mean, color="Near 30km or Faster Road"), size=1.5) +
  geom_line(data=data_annual_45km_ntlp, aes(x=years_since_treat, y=globcover_urban_mean, color="Near 45km or Faster Road"), size=1.5) +
  geom_line(data=data_annual_50km_ntlp, aes(x=years_since_treat, y=globcover_urban_mean, color="Near 50km or Faster Road"), size=1.5) + 
  geom_vline(aes(xintercept=0)) +
  theme_minimal() + 
  labs(x="Years Since Cell Became Within 1km of Road", 
       y="Proportion of Cells Urban",
       title="",
       color="") +
  theme(plot.title=element_text(hjust=0.5,face="bold"),
        axis.text = element_text(size=13),
        axis.title = element_text(size=13)) +
  scale_color_manual(values=c("dodgerblue1","forestgreen","darkorange"))
ggsave(years_since_treat_30_45_50_urban_ntlp, file=file.path(project_file_path, "Results", "Figures", "years_since_treat_30_45_50_urban_ntlp.png"), height=4,width=6.5)

years_since_treat_70_urban_ntlp <- ggplot() +
  geom_line(data=data_annual_70km_ntlp[data_annual_70km_ntlp$years_since_treat < 10,], aes(x=years_since_treat, y=globcover_urban_mean, color="Near 70km or Faster Road"), size=1.5) + 
  geom_vline(aes(xintercept=0)) +
  theme_minimal() + 
  labs(x="Years Since Cell Became Within 1km of Road", 
       y="Proportion of Cells Urban",
       title="",
       color="") +
  theme(plot.title=element_text(hjust=0.5,face="bold"),
        axis.text = element_text(size=13),
        axis.title = element_text(size=13)) +
  scale_color_manual(values=c("firebrick1"))
ggsave(years_since_treat_70_urban_ntlp, file=file.path(project_file_path, "Results", "Figures", "years_since_treat_70_urban_ntlp.png"), height=4,width=6.5)

#### Globcover-Cropland
years_since_treat_30_45_50_cropland_ntlp <- ggplot() +
  geom_line(data=data_annual_30km_ntlp, aes(x=years_since_treat, y=globcover_cropland_mean, color="Near 30km or Faster Road"), size=1.5) +
  geom_line(data=data_annual_45km_ntlp, aes(x=years_since_treat, y=globcover_cropland_mean, color="Near 45km or Faster Road"), size=1.5) +
  geom_line(data=data_annual_50km_ntlp, aes(x=years_since_treat, y=globcover_cropland_mean, color="Near 50km or Faster Road"), size=1.5) + 
  geom_line(data=data_annual_70km_ntlp[data_annual_70km_ntlp$years_since_treat < 8,], aes(x=years_since_treat, y=globcover_cropland_mean, color="Near 70km or Faster Road"), size=1.5) + 
  geom_vline(aes(xintercept=0)) +
  theme_minimal() + 
  labs(x="Years Since Cell Became Within 1km of Road", 
       y="Proportion of Cells Cropland",
       title="",
       color="") +
  theme(plot.title=element_text(hjust=0.5,face="bold"),
        axis.text = element_text(size=13),
        axis.title = element_text(size=13)) +
  scale_color_manual(values=c("dodgerblue1","forestgreen","darkorange","firebrick1"))
ggsave(years_since_treat_30_45_50_cropland_ntlp , file=file.path(project_file_path, "Results", "Figures", "years_since_treat_30_45_50_70_cropland_ntlp.png"), height=4,width=6.5)

# Figures:NTL 0 at Baseline --------------------------------------------

#### DMSP-OLS
years_since_treat_30_45_50_dmspols_ntl0 <- ggplot() +
  geom_line(data=data_annual_30km_ntl0, aes(x=years_since_treat, y=dmspols_mean, color="Near 30km or Faster Road"), size=1.5) +
  geom_line(data=data_annual_45km_ntl0, aes(x=years_since_treat, y=dmspols_mean, color="Near 45km or Faster Road"), size=1.5) +
  geom_line(data=data_annual_50km_ntl0, aes(x=years_since_treat, y=dmspols_mean, color="Near 50km or Faster Road"), size=1.5) + 
  geom_vline(aes(xintercept=0)) +
  theme_minimal() + 
  labs(x="Years Since Cell Became Within 1km of Road", 
       y="Luminosity",
       color="") +
  theme(plot.title=element_text(hjust=0.5,face="bold"),
        axis.text = element_text(size=13),
        axis.title = element_text(size=13)) +
  scale_color_manual(values=c("dodgerblue1","forestgreen","darkorange"))
ggsave(years_since_treat_30_45_50_dmspols_ntl0, file=file.path(project_file_path, "Results", "Figures", "years_since_treat_30_45_50_dmspols_ntl0.png"), height=4,width=6.5)

years_since_treat_70_dmspols_ntl0 <- ggplot() +
  geom_line(data=data_annual_70km_ntl0[data_annual_70km_ntl0$years_since_treat < 10,], aes(x=years_since_treat, y=dmspols_mean, color="Near 70km or Faster Road"), size=1.5) + 
  geom_vline(aes(xintercept=0)) +
  theme_minimal() + 
  labs(x="Years Since Cell Became Within 1km of Road", 
       y="Luminosity",
       color="") +
  theme(plot.title=element_text(hjust=0.5,face="bold"),
        axis.text = element_text(size=13),
        axis.title = element_text(size=13)) +
  scale_color_manual(values=c("firebrick1"))
ggsave(years_since_treat_70_dmspols_ntl0, file=file.path(project_file_path, "Results", "Figures", "years_since_treat_70_dmspols_ntl0.png"), height=4,width=6.5)

#### Globcover-Urban
years_since_treat_30_45_50_urban_ntl0 <- ggplot() +
  geom_line(data=data_annual_30km_ntl0, aes(x=years_since_treat, y=globcover_urban_mean, color="Near 30km or Faster Road"), size=1.5) +
  geom_line(data=data_annual_45km_ntl0, aes(x=years_since_treat, y=globcover_urban_mean, color="Near 45km or Faster Road"), size=1.5) +
  geom_line(data=data_annual_50km_ntl0, aes(x=years_since_treat, y=globcover_urban_mean, color="Near 50km or Faster Road"), size=1.5) + 
  geom_vline(aes(xintercept=0)) +
  theme_minimal() + 
  labs(x="Years Since Cell Became Within 1km of Road", 
       y="Proportion of Cells Urban",
       title="",
       color="") +
  theme(plot.title=element_text(hjust=0.5,face="bold"),
        axis.text = element_text(size=13),
        axis.title = element_text(size=13)) +
  scale_color_manual(values=c("dodgerblue1","forestgreen","darkorange"))
ggsave(years_since_treat_30_45_50_urban_ntl0, file=file.path(project_file_path, "Results", "Figures", "years_since_treat_30_45_50_urban_ntl0.png"), height=4,width=6.5)

years_since_treat_70_urban_ntl0 <- ggplot() +
  geom_line(data=data_annual_70km_ntl0[data_annual_70km_ntl0$years_since_treat < 10,], aes(x=years_since_treat, y=globcover_urban_mean, color="Near 70km or Faster Road"), size=1.5) + 
  geom_vline(aes(xintercept=0)) +
  theme_minimal() + 
  labs(x="Years Since Cell Became Within 1km of Road", 
       y="Proportion of Cells Urban",
       title="",
       color="") +
  theme(plot.title=element_text(hjust=0.5,face="bold"),
        axis.text = element_text(size=13),
        axis.title = element_text(size=13)) +
  scale_color_manual(values=c("firebrick1"))
ggsave(years_since_treat_70_urban_ntl0, file=file.path(project_file_path, "Results", "Figures", "years_since_treat_70_urban_ntl0.png"), height=4,width=6.5)

#### Globcover-Cropland
years_since_treat_30_45_50_cropland_ntl0 <- ggplot() +
  geom_line(data=data_annual_30km_ntl0, aes(x=years_since_treat, y=globcover_cropland_mean, color="Near 30km or Faster Road"), size=1.5) +
  geom_line(data=data_annual_45km_ntl0, aes(x=years_since_treat, y=globcover_cropland_mean, color="Near 45km or Faster Road"), size=1.5) +
  geom_line(data=data_annual_50km_ntl0, aes(x=years_since_treat, y=globcover_cropland_mean, color="Near 50km or Faster Road"), size=1.5) + 
  geom_line(data=data_annual_70km_ntl0[data_annual_70km_ntl0$years_since_treat < 8,], aes(x=years_since_treat, y=globcover_cropland_mean, color="Near 70km or Faster Road"), size=1.5) + 
  geom_vline(aes(xintercept=0)) +
  theme_minimal() + 
  labs(x="Years Since Cell Became Within 1km of Road", 
       y="Proportion of Cells Cropland",
       title="",
       color="") +
  theme(plot.title=element_text(hjust=0.5,face="bold"),
        axis.text = element_text(size=13),
        axis.title = element_text(size=13)) +
  scale_color_manual(values=c("dodgerblue1","forestgreen","darkorange","firebrick1"))
ggsave(years_since_treat_30_45_50_cropland_ntl0 , file=file.path(project_file_path, "Results", "Figures", "years_since_treat_30_45_50_70_cropland_ntl0.png"), height=4,width=6.5)

