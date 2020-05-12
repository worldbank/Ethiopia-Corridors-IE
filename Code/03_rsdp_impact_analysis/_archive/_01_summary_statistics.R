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

DIST_THRESH <- 5

# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(project_file_path, "Data", "FinalData", "dmspols_grid_dataset", "dmspols_level_dataset.Rds"))
#data <- data[data$year %in% c(1996,2005,2013,2015),]

data_annual <- data[,list(dmspols_mean = mean(dmspols, na.rm = TRUE),
                          globcover_urban_mean = mean(globcover_urban, na.rm = TRUE),
                          globcover_cropland_mean = mean(globcover_cropland, na.rm = TRUE)),
                    by = year] %>% 
  as.data.frame()
rm(data)

ggplot(data=data_annual, aes(x=year, y=dmspols_mean)) +
  geom_line() +
  theme_minimal()

ggplot(data=data_annual, aes(x=year, y=globcover_urban_mean)) +
  geom_line() +
  theme_minimal()

ggplot(data=data_annual, aes(x=year, y=globcover_cropland_mean)) +
  geom_line() +
  theme_minimal()

data_annual

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

data_annual <- data[,list(dmspols_mean = mean(dmspols, na.rm = TRUE),
                          globcover_urban_mean = mean(globcover_urban, na.rm = TRUE),
                          globcover_cropland_mean = mean(globcover_cropland, na.rm = TRUE),
                          near_any_road_mean = mean(near_any_road, na.rm=TRUE),
                          near_70kmhrmore_road_mean = mean(near_70kmhrmore_road, na.rm=TRUE),
                          near_50kmhrmore_road_mean = mean(near_50kmhrmore_road, na.rm=TRUE),
                          near_45kmhrmore_road_mean = mean(near_45kmhrmore_road, na.rm=TRUE),
                          near_30kmhrmore_road_mean = mean(near_30kmhrmore_road, na.rm=TRUE)),
                      by = year] %>% 
  as.data.frame

data_annual <- as.data.frame(t(data_annual))
names(data_annual) <- paste0("year_",c(1996,2005,2013,2015))





data <- subset(data, select=-c(long, lat, viirs, 
                               distance_asphaltconcrete, distance_earth, distance_gravel, distance_majorgravel, distance_cobblestone,
                               GADM_ID_1, GADM_ID_2, MA_constantpop))

# Subset to random sample
if(RANDOM_SUBSET == TRUE){
  cells_to_keep <- sample(x=unique(data$cell_id), size=round(length(unique(data$cell_id))*0.4))
  data <- data[data$cell_id %in% cells_to_keep,]
}

# Adjust variables
data$GADM_ID_3 <- as.factor(data$GADM_ID_3)

# Create Variable: Fastest Road you are Near -----------------------------------
fastest_road_within_buffer <- function(buffer, data){
  data_speeds <- subset(data, select=c(distance_road_speed_10, distance_road_speed_15, distance_road_speed_20,
                                       distance_road_speed_25, distance_road_speed_30, distance_road_speed_35, 
                                       distance_road_speed_45, distance_road_speed_50, distance_road_speed_70,
                                       distance_road_speed_120))
  for(var in names(data_speeds)){
    # If NA, give large value so can't be minimum
    data_speeds[[var]] <- as.numeric((data_speeds[[var]]) < buffer*1000)
    data_speeds[[var]][is.na(data_speeds[[var]])] <- 0
    
    speed <- gsub("[[:alpha:]]|_","", var) %>% as.numeric
  
    data_speeds[[var]] <- data_speeds[[var]] * speed
  }
  
  return(do.call(pmax, data_speeds))
}

data$fastest_road_2km <- fastest_road_within_buffer(2, data)
data$fastest_road_5km <- fastest_road_within_buffer(5, data)

#### Remove variables for memory management
data <- subset(data, select=-c(distance_road_speed_10,distance_road_speed_15,distance_road_speed_20,
                               distance_road_speed_25,distance_road_speed_30,distance_road_speed_35,
                               distance_road_speed_45,distance_road_speed_50,distance_road_speed_70,
                               distance_road_speed_120))

# Fastest Road by Category -----------------------------------------------------
data$fastest_road_2km_10_35 <- as.numeric(data$fastest_road_2km %in% c(10,15,20,25,30,35))
data$fastest_road_2km_45_50 <- as.numeric(data$fastest_road_2km %in% c(45,50))
data$fastest_road_2km_70_120 <- as.numeric(data$fastest_road_2km %in% c(70,120))

data$fastest_road_5km_10_35 <- as.numeric(data$fastest_road_5km %in% c(10,15,20,25,30,35))
data$fastest_road_5km_45_50 <- as.numeric(data$fastest_road_5km %in% c(45,50))
data$fastest_road_5km_70_120 <- as.numeric(data$fastest_road_5km %in% c(70,120))

# Add Baseline Variables -------------------------------------------------------
data <- data %>%
  group_by(cell_id) %>% 
  mutate(dmspols_1996 = dmspols[year==1996]) %>% 
  mutate(globcover_urban_1996 = globcover_urban[year==1996]) %>% 
  mutate(globcover_cropland_1996 = globcover_cropland[year==1996]) %>%
  as.data.table

#data$dmspols_quartile <- 0
#data$dmspols_quartile[data$dmspols > 0] <- ntile(data$dmspols[data$dmspols > 0], 4)

# Clean for Memory Management --------------------------------------------------
rm(cells_to_keep)
data <- subset(data, select=-c(fastest_road_2km, fastest_road_5km))
data <- subset(data, select=-c(fastest_road_5km_10_35, fastest_road_5km_45_50, fastest_road_5km_70_120))
data <- subset(data, select=-c(globcover_urban_1996, globcover_cropland_1996))

# Regressions ------------------------------------------------------------------
data$dmspols_1996 <- as.numeric(data$dmspols_1996 > 0)
gc()

felm_roadspeed_dmspols_2km <- felm(dmspols ~ fastest_road_2km_10_35 + fastest_road_2km_45_50 + fastest_road_2km_70_120 | cell_id + year | 0 | GADM_ID_3, data=data)
felm_roadspeed_dmspols_2km_10_35 <- felm(dmspols ~ fastest_road_2km_10_35 | cell_id + year | 0 | GADM_ID_3, data=data)
felm_roadspeed_dmspols_2km_45_50 <- felm(dmspols ~ fastest_road_2km_45_50 | cell_id + year | 0 | GADM_ID_3, data=data)
felm_roadspeed_dmspols_2km_70_120 <- felm(dmspols ~ fastest_road_2km_70_120 | cell_id + year | 0 | GADM_ID_3, data=data)

felm_roadspeed_urban_2km <- felm(globcover_urban ~ fastest_road_2km_10_35 + fastest_road_2km_45_50 + fastest_road_2km_70_120 | cell_id + year | 0 | GADM_ID_3, data=data)
felm_roadspeed_urban_2km_10_35 <- felm(globcover_urban ~ fastest_road_2km_10_35 | cell_id + year | 0 | GADM_ID_3, data=data)
felm_roadspeed_urban_2km_45_50 <- felm(globcover_urban ~ fastest_road_2km_45_50 | cell_id + year | 0 | GADM_ID_3, data=data)
felm_roadspeed_urban_2km_70_120 <- felm(globcover_urban ~ fastest_road_2km_70_120 | cell_id + year | 0 | GADM_ID_3, data=data)

felm_roadspeed_cropland_2km <- felm(globcover_cropland ~ fastest_road_2km_10_35 + fastest_road_2km_45_50 + fastest_road_2km_70_120 | cell_id + year | 0 | GADM_ID_3, data=data)
felm_roadspeed_cropland_2km_10_35 <- felm(globcover_cropland ~ fastest_road_2km_10_35 | cell_id + year | 0 | GADM_ID_3, data=data)
felm_roadspeed_cropland_2km_45_50 <- felm(globcover_cropland ~ fastest_road_2km_45_50 | cell_id + year | 0 | GADM_ID_3, data=data)
felm_roadspeed_cropland_2km_70_120 <- felm(globcover_cropland ~ fastest_road_2km_70_120 | cell_id + year | 0 | GADM_ID_3, data=data)

stargazer(felm_roadspeed_dmspols_2km,
          felm_roadspeed_dmspols_2km_10_35,
          felm_roadspeed_dmspols_2km_45_50,
          felm_roadspeed_dmspols_2km_70_120,
          felm_roadspeed_urban_2km,
          felm_roadspeed_urban_2km_10_35,
          felm_roadspeed_urban_2km_45_50,
          felm_roadspeed_urban_2km_70_120,
          felm_roadspeed_cropland_2km,
          felm_roadspeed_cropland_2km_10_35,
          felm_roadspeed_cropland_2km_45_50,
          felm_roadspeed_cropland_2km_70_120,
          dep.var.labels.include = F,
          dep.var.caption="",
          column.labels = c(rep("NTL",4), rep("Urban",4), rep("Crop",4)),
          covariate.labels = c("Fastest Road: 10-35km/hr",
                               "Fastest Road: 45-50km/hr",
                               "Fastest Road: 70-120km/hr"),
          omit.stat=c("f","ser"),
          align=T,
          no.space=T,
          float=F,
          column.sep.width="8pt",
          digits=2,
          add.lines = list(
            c("Cell FE", rep("Y",12)),
            c("Year FE", rep("Y",12))
          ),
          out = file.path(project_file_path, "Results", "Tables", "near_roadspeed_2km.tex"))

rm(felm_roadspeed_dmspols_2km,
          felm_roadspeed_dmspols_2km_10_35,
          felm_roadspeed_dmspols_2km_45_50,
          felm_roadspeed_dmspols_2km_70_120,
          felm_roadspeed_urban_2km,
          felm_roadspeed_urban_2km_10_35,
          felm_roadspeed_urban_2km_45_50,
          felm_roadspeed_urban_2km_70_120,
          felm_roadspeed_cropland_2km,
          felm_roadspeed_cropland_2km_10_35,
          felm_roadspeed_cropland_2km_45_50,
          felm_roadspeed_cropland_2km_70_120)

# Regressions: Interact NTL ----------------------------------------------------
felm_roadspeed_dmspols_2km_ntlbaseline <- felm(dmspols ~ dmspols_1996*fastest_road_2km_10_35 + dmspols_1996*fastest_road_2km_45_50 + dmspols_1996*fastest_road_2km_70_120 - dmspols_1996 | cell_id + year | 0 | GADM_ID_3, data=data)
felm_roadspeed_dmspols_2km_10_35_ntlbaseline <- felm(dmspols ~ dmspols_1996*fastest_road_2km_10_35 - dmspols_1996 | cell_id + year | 0 | GADM_ID_3, data=data)
felm_roadspeed_dmspols_2km_45_50_ntlbaseline <- felm(dmspols ~ dmspols_1996*fastest_road_2km_45_50 - dmspols_1996 | cell_id + year | 0 | GADM_ID_3, data=data)
felm_roadspeed_dmspols_2km_70_120_ntlbaseline <- felm(dmspols ~ dmspols_1996*fastest_road_2km_70_120 - dmspols_1996 | cell_id + year | 0 | GADM_ID_3, data=data)

felm_roadspeed_urban_2km_ntlbaseline <- felm(globcover_urban ~  dmspols_1996*fastest_road_2km_10_35 + dmspols_1996*fastest_road_2km_45_50 + dmspols_1996*fastest_road_2km_70_120 - dmspols_1996 | cell_id + year | 0 | GADM_ID_3, data=data)
felm_roadspeed_urban_2km_10_35_ntlbaseline <- felm(globcover_urban ~ dmspols_1996*fastest_road_2km_10_35 - dmspols_1996 | cell_id + year | 0 | GADM_ID_3, data=data)
felm_roadspeed_urban_2km_45_50_ntlbaseline <- felm(globcover_urban ~ dmspols_1996*fastest_road_2km_45_50 - dmspols_1996 | cell_id + year | 0 | GADM_ID_3, data=data)
felm_roadspeed_urban_2km_70_120_ntlbaseline <- felm(globcover_urban ~ dmspols_1996*fastest_road_2km_70_120 - dmspols_1996 | cell_id + year | 0 | GADM_ID_3, data=data)

felm_roadspeed_cropland_2km_ntlbaseline <- felm(globcover_cropland ~ dmspols_1996*fastest_road_2km_10_35 + dmspols_1996*fastest_road_2km_45_50 + dmspols_1996*fastest_road_2km_70_120 - dmspols_1996 | cell_id + year | 0 | GADM_ID_3, data=data)
felm_roadspeed_cropland_2km_10_35_ntlbaseline <- felm(globcover_cropland ~ dmspols_1996*fastest_road_2km_10_35 - dmspols_1996 | cell_id + year | 0 | GADM_ID_3, data=data)
felm_roadspeed_cropland_2km_45_50_ntlbaseline <- felm(globcover_cropland ~ dmspols_1996*fastest_road_2km_45_50 - dmspols_1996 | cell_id + year | 0 | GADM_ID_3, data=data)
felm_roadspeed_cropland_2km_70_120_ntlbaseline <- felm(globcover_cropland ~ dmspols_1996*fastest_road_2km_70_120 - dmspols_1996 | cell_id + year | 0 | GADM_ID_3, data=data)

stargazer(felm_roadspeed_dmspols_2km_ntlbaseline,
          felm_roadspeed_dmspols_2km_10_35_ntlbaseline,
          felm_roadspeed_dmspols_2km_45_50_ntlbaseline,
          felm_roadspeed_dmspols_2km_70_120_ntlbaseline,
          felm_roadspeed_urban_2km_ntlbaseline,
          felm_roadspeed_urban_2km_10_35_ntlbaseline,
          felm_roadspeed_urban_2km_45_50_ntlbaseline,
          felm_roadspeed_urban_2km_70_120_ntlbaseline,
          felm_roadspeed_cropland_2km_ntlbaseline,
          felm_roadspeed_cropland_2km_10_35_ntlbaseline,
          felm_roadspeed_cropland_2km_45_50_ntlbaseline,
          felm_roadspeed_cropland_2km_70_120_ntlbaseline,
          dep.var.labels.include = F,
          dep.var.caption="",
          column.labels = c(rep("NTL",4), rep("Urban",4), rep("Crop",4)),
          covariate.labels = c("Fastest Road: 10-35km/hr",
                               "Fastest Road: 45-50km/hr",
                               "Fastest Road: 70-120km/hr",
                               "Fastest Road: 10-35km/hr X Baseline NTL",
                               "Fastest Road: 45-50km/hr X Baseline NTL",
                               "Fastest Road: 70-120km/hr X Baseline NTL"),
          omit.stat=c("f","ser"),
          align=T,
          no.space=T,
          float=F,
          column.sep.width="8pt",
          digits=2,
          add.lines = list(
            c("Cell FE", rep("Y",12)),
            c("Year FE", rep("Y",12))
          ),
          out = file.path(project_file_path, "Results", "Tables", "near_roadspeed_2km_interact_baseline_ntl.tex"))

rm(felm_roadspeed_dmspols_2km_ntlbaseline,
          felm_roadspeed_dmspols_2km_10_35_ntlbaseline,
          felm_roadspeed_dmspols_2km_45_50_ntlbaseline,
          felm_roadspeed_dmspols_2km_70_120_ntlbaseline,
          felm_roadspeed_urban_2km_ntlbaseline,
          felm_roadspeed_urban_2km_10_35_ntlbaseline,
          felm_roadspeed_urban_2km_45_50_ntlbaseline,
          felm_roadspeed_urban_2km_70_120_ntlbaseline,
          felm_roadspeed_cropland_2km_ntlbaseline,
          felm_roadspeed_cropland_2km_10_35_ntlbaseline,
          felm_roadspeed_cropland_2km_45_50_ntlbaseline,
          felm_roadspeed_cropland_2km_70_120_ntlbaseline)

# Regressions: Low Baseline NTL ------------------------------------------------
# NTL == 0
felm_roadspeed_dmspols_2km_lowbaselinentl <- felm(dmspols ~ fastest_road_2km_10_35 + fastest_road_2km_45_50 + fastest_road_2km_70_120 | cell_id + year | 0 | GADM_ID_3, data=data[data$dmspols_1996 == 0,])
felm_roadspeed_dmspols_2km_10_35_lowbaselinentl <- felm(dmspols ~ fastest_road_2km_10_35 | cell_id + year | 0 | GADM_ID_3, data=data[data$dmspols_1996 == 0,])
felm_roadspeed_dmspols_2km_45_50_lowbaselinentl <- felm(dmspols ~ fastest_road_2km_45_50 | cell_id + year | 0 | GADM_ID_3, data=data[data$dmspols_1996 == 0,])
felm_roadspeed_dmspols_2km_70_120_lowbaselinentl <- felm(dmspols ~ fastest_road_2km_70_120 | cell_id + year | 0 | GADM_ID_3, data=data[data$dmspols_1996 == 0,])

felm_roadspeed_urban_2km_lowbaselinentl <- felm(globcover_urban ~ fastest_road_2km_10_35 + fastest_road_2km_45_50 + fastest_road_2km_70_120 | cell_id + year | 0 | GADM_ID_3, data=data[data$dmspols_1996 == 0,])
felm_roadspeed_urban_2km_10_35_lowbaselinentl <- felm(globcover_urban ~ fastest_road_2km_10_35 | cell_id + year | 0 | GADM_ID_3, data=data[data$dmspols_1996 == 0,])
felm_roadspeed_urban_2km_45_50_lowbaselinentl <- felm(globcover_urban ~ fastest_road_2km_45_50 | cell_id + year | 0 | GADM_ID_3, data=data[data$dmspols_1996 == 0,])
felm_roadspeed_urban_2km_70_120_lowbaselinentl <- felm(globcover_urban ~ fastest_road_2km_70_120 | cell_id + year | 0 | GADM_ID_3, data=data[data$dmspols_1996 == 0,])

felm_roadspeed_cropland_2km_lowbaselinentl <- felm(globcover_cropland ~ fastest_road_2km_10_35 + fastest_road_2km_45_50 + fastest_road_2km_70_120 | cell_id + year | 0 | GADM_ID_3, data=data[data$dmspols_1996 == 0,])
felm_roadspeed_cropland_2km_10_35_lowbaselinentl <- felm(globcover_cropland ~ fastest_road_2km_10_35 | cell_id + year | 0 | GADM_ID_3, data=data[data$dmspols_1996 == 0,])
felm_roadspeed_cropland_2km_45_50_lowbaselinentl <- felm(globcover_cropland ~ fastest_road_2km_45_50 | cell_id + year | 0 | GADM_ID_3, data=data[data$dmspols_1996 == 0,])
felm_roadspeed_cropland_2km_70_120_lowbaselinentl <- felm(globcover_cropland ~ fastest_road_2km_70_120 | cell_id + year | 0 | GADM_ID_3, data=data[data$dmspols_1996 == 0,])

stargazer(felm_roadspeed_dmspols_2km_lowbaselinentl,
          felm_roadspeed_dmspols_2km_10_35_lowbaselinentl,
          felm_roadspeed_dmspols_2km_45_50_lowbaselinentl,
          felm_roadspeed_dmspols_2km_70_120_lowbaselinentl,
          felm_roadspeed_urban_2km_lowbaselinentl,
          felm_roadspeed_urban_2km_10_35_lowbaselinentl,
          felm_roadspeed_urban_2km_45_50_lowbaselinentl,
          felm_roadspeed_urban_2km_70_120_lowbaselinentl,
          felm_roadspeed_cropland_2km_lowbaselinentl,
          felm_roadspeed_cropland_2km_10_35_lowbaselinentl,
          felm_roadspeed_cropland_2km_45_50_lowbaselinentl,
          felm_roadspeed_cropland_2km_70_120_lowbaselinentl,
          dep.var.labels.include = F,
          dep.var.caption="",
          column.labels = c(rep("NTL",4), rep("Urban",4), rep("Crop",4)),
          covariate.labels = c("Fastest Road: 10-35km/hr",
                               "Fastest Road: 45-50km/hr",
                               "Fastest Road: 70-120km/hr"),
          omit.stat=c("f","ser"),
          align=T,
          no.space=T,
          float=F,
          column.sep.width="8pt",
          digits=2,
          add.lines = list(
            c("Cell FE", rep("Y",12)),
            c("Year FE", rep("Y",12))
          ),
          out = file.path(project_file_path, "Results", "Tables", "near_roadspeed_2km_lowbaselinentl.tex"))

rm(felm_roadspeed_dmspols_2km_lowbaselinentl,
          felm_roadspeed_dmspols_2km_10_35_lowbaselinentl,
          felm_roadspeed_dmspols_2km_45_50_lowbaselinentl,
          felm_roadspeed_dmspols_2km_70_120_lowbaselinentl,
          felm_roadspeed_urban_2km_lowbaselinentl,
          felm_roadspeed_urban_2km_10_35_lowbaselinentl,
          felm_roadspeed_urban_2km_45_50_lowbaselinentl,
          felm_roadspeed_urban_2km_70_120_lowbaselinentl,
          felm_roadspeed_cropland_2km_lowbaselinentl,
          felm_roadspeed_cropland_2km_10_35_lowbaselinentl,
          felm_roadspeed_cropland_2km_45_50_lowbaselinentl,
          felm_roadspeed_cropland_2km_70_120_lowbaselinentl)

# Regressions: High Baseline NTL -----------------------------------------------
# NTL > 0
felm_roadspeed_dmspols_2km_highbaselinentl <- felm(dmspols ~ fastest_road_2km_10_35 + fastest_road_2km_45_50 + fastest_road_2km_70_120 | cell_id + year | 0 | GADM_ID_3, data=data[data$dmspols_1996 > 0,])
felm_roadspeed_dmspols_2km_10_35_highbaselinentl <- felm(dmspols ~ fastest_road_2km_10_35 | cell_id + year | 0 | GADM_ID_3, data=data[data$dmspols_1996 > 0,])
felm_roadspeed_dmspols_2km_45_50_highbaselinentl <- felm(dmspols ~ fastest_road_2km_45_50 | cell_id + year | 0 | GADM_ID_3, data=data[data$dmspols_1996 > 0,])
felm_roadspeed_dmspols_2km_70_120_highbaselinentl <- felm(dmspols ~ fastest_road_2km_70_120 | cell_id + year | 0 | GADM_ID_3, data=data[data$dmspols_1996 > 0,])

felm_roadspeed_urban_2km_highbaselinentl <- felm(globcover_urban ~ fastest_road_2km_10_35 + fastest_road_2km_45_50 + fastest_road_2km_70_120 | cell_id + year | 0 | GADM_ID_3, data=data[data$dmspols_1996 > 0,])
felm_roadspeed_urban_2km_10_35_highbaselinentl <- felm(globcover_urban ~ fastest_road_2km_10_35 | cell_id + year | 0 | GADM_ID_3, data=data[data$dmspols_1996 > 0,])
felm_roadspeed_urban_2km_45_50_highbaselinentl <- felm(globcover_urban ~ fastest_road_2km_45_50 | cell_id + year | 0 | GADM_ID_3, data=data[data$dmspols_1996 > 0,])
felm_roadspeed_urban_2km_70_120_highbaselinentl <- felm(globcover_urban ~ fastest_road_2km_70_120 | cell_id + year | 0 | GADM_ID_3, data=data[data$dmspols_1996 > 0,])

felm_roadspeed_cropland_2km_highbaselinentl <- felm(globcover_cropland ~ fastest_road_2km_10_35 + fastest_road_2km_45_50 + fastest_road_2km_70_120 | cell_id + year | 0 | GADM_ID_3, data=data[data$dmspols_1996 > 0,])
felm_roadspeed_cropland_2km_10_35_highbaselinentl <- felm(globcover_cropland ~ fastest_road_2km_10_35 | cell_id + year | 0 | GADM_ID_3, data=data[data$dmspols_1996 > 0,])
felm_roadspeed_cropland_2km_45_50_highbaselinentl <- felm(globcover_cropland ~ fastest_road_2km_45_50 | cell_id + year | 0 | GADM_ID_3, data=data[data$dmspols_1996 > 0,])
felm_roadspeed_cropland_2km_70_120_highbaselinentl <- felm(globcover_cropland ~ fastest_road_2km_70_120 | cell_id + year | 0 | GADM_ID_3, data=data[data$dmspols_1996 > 0,])

stargazer(felm_roadspeed_dmspols_2km_highbaselinentl,
          felm_roadspeed_dmspols_2km_10_35_highbaselinentl,
          felm_roadspeed_dmspols_2km_45_50_highbaselinentl,
          felm_roadspeed_dmspols_2km_70_120_highbaselinentl,
          felm_roadspeed_urban_2km_highbaselinentl,
          felm_roadspeed_urban_2km_10_35_highbaselinentl,
          felm_roadspeed_urban_2km_45_50_highbaselinentl,
          felm_roadspeed_urban_2km_70_120_highbaselinentl,
          felm_roadspeed_cropland_2km_highbaselinentl,
          felm_roadspeed_cropland_2km_10_35_highbaselinentl,
          felm_roadspeed_cropland_2km_45_50_highbaselinentl,
          felm_roadspeed_cropland_2km_70_120_highbaselinentl,
          dep.var.labels.include = F,
          dep.var.caption="",
          column.labels = c(rep("NTL",4), rep("Urban",4), rep("Crop",4)),
          covariate.labels = c("Fastest Road: 10-35km/hr",
                               "Fastest Road: 45-50km/hr",
                               "Fastest Road: 70-120km/hr"),
          omit.stat=c("f","ser"),
          align=T,
          no.space=T,
          float=F,
          column.sep.width="8pt",
          digits=2,
          add.lines = list(
            c("Cell FE", rep("Y",12)),
            c("Year FE", rep("Y",12))
          ),
          out = file.path(project_file_path, "Results", "Tables", "near_roadspeed_2km_highbaselinentl.tex"))

rm(felm_roadspeed_dmspols_2km_highbaselinentl,
          felm_roadspeed_dmspols_2km_10_35_highbaselinentl,
          felm_roadspeed_dmspols_2km_45_50_highbaselinentl,
          felm_roadspeed_dmspols_2km_70_120_highbaselinentl,
          felm_roadspeed_urban_2km_highbaselinentl,
          felm_roadspeed_urban_2km_10_35_highbaselinentl,
          felm_roadspeed_urban_2km_45_50_highbaselinentl,
          felm_roadspeed_urban_2km_70_120_highbaselinentl,
          felm_roadspeed_cropland_2km_highbaselinentl,
          felm_roadspeed_cropland_2km_10_35_highbaselinentl,
          felm_roadspeed_cropland_2km_45_50_highbaselinentl,
          felm_roadspeed_cropland_2km_70_120_highbaselinentl)


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
##### PURGATORY #####
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Regression: Individual Coefficients ------------------------------------------
data$fastest_road_2km_10 <- as.numeric(data$fastest_road_2km %in% 10)
data$fastest_road_2km_15 <- as.numeric(data$fastest_road_2km %in% 15)
data$fastest_road_2km_20 <- as.numeric(data$fastest_road_2km %in% 20)
data$fastest_road_2km_25 <- as.numeric(data$fastest_road_2km %in% 25)
data$fastest_road_2km_30 <- as.numeric(data$fastest_road_2km %in% 30)
data$fastest_road_2km_35 <- as.numeric(data$fastest_road_2km %in% 35)
data$fastest_road_2km_45 <- as.numeric(data$fastest_road_2km %in% 45)
data$fastest_road_2km_50 <- as.numeric(data$fastest_road_2km %in% 50)
data$fastest_road_2km_70 <- as.numeric(data$fastest_road_2km %in% 70)

# Aggregate Speeds Together
data$fastest_road_2km_simple <- data$fastest_road_2km
data$fastest_road_2km_simple[data$fastest_road_2km_simple %in% 15] <- 10
data$fastest_road_2km_simple[data$fastest_road_2km_simple %in% 25] <- 20
data$fastest_road_2km_simple[data$fastest_road_2km_simple %in% 35] <- 30

data$fastest_road_2km_10_15 <- as.numeric(data$fastest_road_2km %in% c(10,15))
data$fastest_road_2km_20_25 <- as.numeric(data$fastest_road_2km %in% c(20,25))
data$fastest_road_2km_30_35 <- as.numeric(data$fastest_road_2km %in% c(30,35))
data$fastest_road_2km_45 <- as.numeric(data$fastest_road_2km %in% 45)
data$fastest_road_2km_50 <- as.numeric(data$fastest_road_2km %in% 50)
data$fastest_road_2km_70 <- as.numeric(data$fastest_road_2km %in% 70)

felm_roadspeed_2km_10_15 <- felm(dep_var ~ fastest_road_2km_10_15 | cell_id + year | 0 | GADM_ID_3, data=data)
felm_roadspeed_2km_20_25 <- felm(dep_var ~ fastest_road_2km_20_25 | cell_id + year | 0 | GADM_ID_3, data=data)
felm_roadspeed_2km_30_35 <- felm(dep_var ~ fastest_road_2km_30_35 | cell_id + year | 0 | GADM_ID_3, data=data)
felm_roadspeed_2km_45 <- felm(dep_var ~ fastest_road_2km_45 | cell_id + year | 0 | GADM_ID_3, data=data)
felm_roadspeed_2km_50 <- felm(dep_var ~ fastest_road_2km_50 | cell_id + year | 0 | GADM_ID_3, data=data)
felm_roadspeed_2km_70 <- felm(dep_var ~ fastest_road_2km_70 | cell_id + year | 0 | GADM_ID_3, data=data)

felm_roadtype_5km <- felm(dep_var ~ factor(fastest_road_2km_simple) | cell_id + year | 0 | GADM_ID_3, data=data)


dep_var <- "dmspols"
data$dep_var <- data[[dep_var]]

felm_roadtype_5km <- felm(dep_var ~ factor(fastest_road_2km) | cell_id + year | 0 | GADM_ID_3, data=data)

data$fastest_road_2km_45kmhr_2 %>% is.na %>% table
felm_roadtype_5km <- felm(dep_var ~ factor(fastest_road_2km_70kmhr) | cell_id + year | 0 | GADM_ID_3, data=data)
felm_roadtype_5km <- felm(dep_var ~ fastest_road_2km_45 | cell_id + year | 0 | GADM_ID_3, data=data)



table(data$fastest_road_5km_45kmhr_1, data$fastest_road_5km_45kmhr_2)
data$fastest_road_2km_45kmhr_1[data$fastest_road_2km_45kmhr_2==1] %>% table

data$fastest_road_2km_45kmhr_2[data$year == 2004] %>% table

felm_roadtype_5km <- felm(dep_var ~ distance_asphaltconcrete_5km + distance_cobblestone_5km + distance_majorgravel_5km + distance_gravel_5km + distance_earth_5km | cell_id + year | 0 | GADM_ID_3, data=data)
felm_roadtype_5km_asphaltconcrete <- felm(dep_var ~ distance_asphaltconcrete_5km | cell_id + year | 0 | GADM_ID_3, data=data)
felm_roadtype_5km_cobblestone <- felm(dep_var ~ distance_cobblestone_5km | cell_id + year | 0 | GADM_ID_3, data=data)
felm_roadtype_5km_majorgravel <- felm(dep_var ~ distance_majorgravel_5km | cell_id + year | 0 | GADM_ID_3, data=data)
felm_roadtype_5km_gravel <- felm(dep_var ~ distance_gravel_5km | cell_id + year | 0 | GADM_ID_3, data=data)
felm_roadtype_5km_earth <- felm(dep_var ~ distance_earth_5km | cell_id + year | 0 | GADM_ID_3, data=data)

felm_roadtype_5km_baselinentl <- felm(dep_var ~ distance_asphaltconcrete_5km*dmspols_1996 + distance_cobblestone_5km*dmspols_1996 + distance_majorgravel_5km*dmspols_1996 + distance_gravel_5km*dmspols_1996 + distance_earth_5km*dmspols_1996 - dmspols_1996 | cell_id + year | 0 | GADM_ID_3, data=data)
felm_roadtype_5km_baselinentl_asphaltconcrete <- felm(dep_var ~ distance_asphaltconcrete_5km*dmspols_1996 - dmspols_1996 | cell_id + year | 0 | GADM_ID_3, data=data)
felm_roadtype_5km_baselinentl_cobblestone <- felm(dep_var ~ distance_cobblestone_5km*dmspols_1996 - dmspols_1996 | cell_id + year | 0 | GADM_ID_3, data=data)
felm_roadtype_5km_baselinentl_majorgravel <- felm(dep_var ~ distance_majorgravel_5km*dmspols_1996 - dmspols_1996 | cell_id + year | 0 | GADM_ID_3, data=data)
felm_roadtype_5km_baselinentl_gravel <- felm(dep_var ~ distance_gravel_5km*dmspols_1996 - dmspols_1996 | cell_id + year | 0 | GADM_ID_3, data=data)
felm_roadtype_5km_baselinentl_earth <- felm(dep_var ~ distance_earth_5km*dmspols_1996 - dmspols_1996 | cell_id + year | 0 | GADM_ID_3, data=data)



felm_70kmhrroad_5km <- felm(dep_var ~ fastest_road_5km_70kmhr_1 + fastest_road_5km_70kmhr_2 | cell_id + year | 0 | GADM_ID_3, data=data)
felm_70kmhrroad_5km_less70 <- felm(dep_var ~ fastest_road_5km_70kmhr_1 | cell_id + year | 0 | GADM_ID_3, data=data)
felm_70kmhrroad_5km_greater70 <- felm(dep_var ~ fastest_road_5km_70kmhr_2 | cell_id + year | 0 | GADM_ID_3, data=data)





# Regression -------------------------------------------------------------------
#### Road Type
felm_dmspols_roadtype_5km <- felm(dmspols ~ distance_asphaltconcrete_5km + distance_cobblestone_5km + distance_majorgravel_5km + distance_gravel_5km + distance_earth_5km | cell_id + year | 0 | GADM_ID_3, data=data)
felm_dmspols_roadtype_5km_baselinentl <- felm(dmspols ~ distance_asphaltconcrete_5km*dmspols_1996 + distance_cobblestone_5km*dmspols_1996 + distance_majorgravel_5km*dmspols_1996 + distance_gravel_5km*dmspols_1996 + distance_earth_5km*dmspols_1996 - dmspols_1996 | cell_id + year | 0 | GADM_ID_3, data=data)

felm_urban_roadtype_5km <- felm(globcover_urban ~ distance_asphaltconcrete_5km + distance_cobblestone_5km + distance_majorgravel_5km + distance_gravel_5km + distance_earth_5km | cell_id + year | 0 | GADM_ID_3, data=data)
felm_urban_roadtype_5km_baselinentl <- felm(globcover_urban ~ distance_asphaltconcrete_5km*dmspols_1996 + distance_cobblestone_5km*dmspols_1996 + distance_majorgravel_5km*dmspols_1996 + distance_gravel_5km*dmspols_1996 + distance_earth_5km*dmspols_1996 - dmspols_1996 | cell_id + year | 0 | GADM_ID_3, data=data)

felm_crop_roadtype_5km <- felm(globcover_cropland ~ distance_asphaltconcrete_5km + distance_cobblestone_5km + distance_majorgravel_5km + distance_gravel_5km + distance_earth_5km | cell_id + year | 0 | GADM_ID_3, data=data)
felm_crop_roadtype_5km_baselinentl <- felm(globcover_cropland ~ distance_asphaltconcrete_5km*dmspols_1996 + distance_cobblestone_5km*dmspols_1996 + distance_majorgravel_5km*dmspols_1996 + distance_gravel_5km*dmspols_1996 + distance_earth_5km*dmspols_1996 - dmspols_1996 | cell_id + year | 0 | GADM_ID_3, data=data)

#### Any Road
felm_dmspols_anyroad_5km <- felm(dmspols ~ near_road_5km | cell_id + year | 0 | GADM_ID_3, data=data)
felm_dmspols_anyroad_5km_baselinentl <- felm(dmspols ~ near_road_5km*dmspols_1996 - dmspols_1996 | cell_id + year | 0 | GADM_ID_3, data=data)

felm_urban_anyroad_5km <- felm(globcover_urban ~ near_road_5km | cell_id + year | 0 | GADM_ID_3, data=data)
felm_urban_anyroad_5km_baselinentl <- felm(globcover_urban ~ near_road_5km*dmspols_1996 - dmspols_1996 | cell_id + year | 0 | GADM_ID_3, data=data)

felm_crop_anyroad_5km <- felm(globcover_cropland ~ near_road_5km | cell_id + year | 0 | GADM_ID_3, data=data)
felm_crop_anyroad_5km_baselinentl <- felm(globcover_cropland ~ near_road_5km*dmspols_1996 - dmspols_1996 | cell_id + year | 0 | GADM_ID_3, data=data)

#### 70km/hr Road
felm_dmspols_70kmhrroad_5km <- felm(dmspols ~ fastest_road_5km_70kmhr_1 + fastest_road_5km_70kmhr_2 | cell_id + year | 0 | GADM_ID_3, data=data)
felm_dmspols_70kmhrroad_5km_baselinentl <- felm(dmspols ~ fastest_road_5km_70kmhr_1*dmspols_1996 + fastest_road_5km_70kmhr_2*dmspols_1996 - dmspols_1996 | cell_id + year | 0 | GADM_ID_3, data=data)

felm_urban_70kmhrroad_5km <- felm(globcover_urban ~ fastest_road_5km_70kmhr_1 + fastest_road_5km_70kmhr_2 | cell_id + year | 0 | GADM_ID_3, data=data)
felm_urban_70kmhrroad_5km_baselinentl <- felm(globcover_urban ~ fastest_road_5km_70kmhr_1*dmspols_1996 + fastest_road_5km_70kmhr_2*dmspols_1996 - dmspols_1996 | cell_id + year | 0 | GADM_ID_3, data=data)

felm_crop_70kmhrroad_5km <- felm(globcover_cropland ~ fastest_road_5km_70kmhr_1 + fastest_road_5km_70kmhr_2 | cell_id + year | 0 | GADM_ID_3, data=data)
felm_crop_70kmhrroad_5km_baselinentl <- felm(globcover_cropland ~ fastest_road_5km_70kmhr_1*dmspols_1996 + fastest_road_5km_70kmhr_2*dmspols_1996 - dmspols_1996 | cell_id + year | 0 | GADM_ID_3, data=data)

# Export Results ---------------------------------------------------------------
#### Road Type
stargazer(felm_dmspols_roadtype_5km, 
          felm_urban_roadtype_5km,
          felm_crop_roadtype_5km,
          felm_dmspols_roadtype_5km_baselinentl,
          felm_urban_roadtype_5km_baselinentl,
          felm_crop_roadtype_5km_baselinentl,
          dep.var.labels.include = F,
          column.labels = c("NTL", "Urban", "Crop", "NTL", "Urban", "Crop"),
          covariate.labels=c("Near Asphalt Road", 
                             "Near Cobblestone Road",
                             "Near Major Gravel Road",
                             "Near Gravel Road",
                             "Near Earth Road",
                             "Near Asphalt Road X Baseline NTL", 
                             "Near Cobblestone Road X Baseline NTL",
                             "Near Major Gravel Road X Baseline NTL",
                             "Near Gravel Road X Baseline NTL",
                             "Near Earth Road X Baseline NTL"),
          dep.var.caption="",
          omit.stat=c("f","ser"),
          align=T,
          no.space=T,
          float=F,
          column.sep.width="8pt",
          digits=2,
          add.lines = list(
            c("Cell FE", rep("Y",6)),
            c("Year FE", rep("Y",6))
          ),
          out = file.path(project_file_path, "Results", "Tables", "near_roadtype_5km.tex"))

#### Any Road
stargazer(felm_dmspols_anyroad_5km, 
          felm_urban_anyroad_5km,
          felm_crop_anyroad_5km,
          felm_dmspols_anyroad_5km_baselinentl,
          felm_urban_anyroad_5km_baselinentl,
          felm_crop_anyroad_5km_baselinentl,
          dep.var.labels.include = F,
          column.labels = c("NTL", "Urban", "Crop", "NTL", "Urban", "Crop"),
          covariate.labels=c("Near Road", "Near Road X Baseline NTL"),
          dep.var.caption="",
          omit.stat=c("f","ser"),
          align=T,
          no.space=T,
          float=F,
          column.sep.width="8pt",
          digits=2,
          add.lines = list(
            c("Cell FE", rep("Y",6)),
            c("Year FE", rep("Y",6))
          ),
          out = file.path(project_file_path, "Results", "Tables", "near_road_5km.tex"))

#### > 70km/hr Road
stargazer(felm_dmspols_70kmhrroad_5km, 
          felm_urban_70kmhrroad_5km,
          felm_crop_70kmhrroad_5km,
          felm_dmspols_70kmhrroad_5km_baselinentl,
          felm_urban_70kmhrroad_5km_baselinentl,
          felm_crop_70kmhrroad_5km_baselinentl,
          dep.var.labels.include = F,
          column.labels = c("NTL", "Urban", "Crop", "NTL", "Urban", "Crop"),
          covariate.labels=c("Fastest Road Near $<$70km/hr", 
                             "Fastest Road Near $\\geq$70km/hr",
                             "Fastest Road Near $<$70km/hr X Baseline NTL", 
                             "Fastest Road Near $\\geq$70km/hr X Baseline NTL"),
          dep.var.caption="",
          omit.stat=c("f","ser"),
          align=T,
          no.space=T,
          float=F,
          column.sep.width="8pt",
          digits=2,
          add.lines = list(
            c("Cell FE", rep("Y",6)),
            c("Year FE", rep("Y",6))
          ),
          out = file.path(project_file_path, "Results", "Tables", "near_70kmhrroad_5km.tex"))
















stargazer(felm_dmspols_anyroad_5km, 
          felm_urban_anyroad_5km,
          felm_cropland_anyroad_5km,
          felm_dmspols_anyroad_5km_ntlbaseline,
          felm_urban_anyroad_5km_ntlbaseline,
          felm_cropland_anyroad_5km_ntlbaseline,
          dep.var.labels.include = F,
          column.labels = c("NTL", "Urban", "Crop", "NTL", "Urban", "Crop"),
          covariate.labels = c("Near Road", "Near Road X Baseline NTL"),
          dep.var.caption="",
          omit.stat=c("f","ser"),
          align=T,
          no.space=T,
          float=F,
          column.sep.width="8pt",
          digits=2,
          add.lines = list(
            c("Cell FE", rep("Y",6)),
            c("Year FE", rep("Y",6))
          ),
          out = file.path(project_file_path, "Results", "Tables", "near_road_5km.tex"))


























felm_dmspols_70kmhrroad_5km <- felm(dmspols ~ fastest_road_5km_70kmhr_1 + fastest_road_5km_70kmhr_2 | cell_id + year | 0 | GADM_ID_3, data=data)



#### 5km Buffer
felm_dmspols_anyroad_5km <- felm(dmspols ~ near_road_5km | cell_id + year | 0 | GADM_ID_3, data=data)
felm_dmspols_70kmhrroad_5km <- felm(dmspols ~ factor(fastest_road_5km_70kmhr) | cell_id + year | 0 | GADM_ID_3, data=data)
felm_dmspols_byspeed_5km <- felm(dmspols ~ factor(fastest_road_5km) | cell_id + year | 0 | GADM_ID_3, data=data)

felm_dmspols_anyroad_5km_baselinedmspols <- felm(dmspols ~ near_road_5km + near_road_5km*dmspols_1996 - dmspols_1996 | cell_id + year | 0 | GADM_ID_3, data=data)
felm_dmspols_70kmhrroad_5km_baselinedmspols <- felm(dmspols ~ factor(fastest_road_5km_70kmhr)*dmspols_1996 - dmspols_1996 | cell_id + year | 0 | GADM_ID_3, data=data)

felm_dmspols_anyroad_5km <- felm(dmspols ~ distance_asphaltconcrete + distance_earth + distance_gravel + distance_majorgravel | cell_id + year | 0 | GADM_ID_3, data=data)


# Near Any Road
felm_dmspols_anyroad_5km <- felm(dmspols ~ near_road_5km | cell_id + year | 0 | GADM_ID_3, data=data)
felm_urban_anyroad_5km <- felm(globcover_urban ~ near_road_5km | cell_id + year | 0 | GADM_ID_3, data=data)
felm_cropland_anyroad_5km <- felm(globcover_cropland ~ near_road_5km | cell_id + year | 0 | GADM_ID_3, data=data)

felm_dmspols_anyroad_5km_ntlbaseline <- felm(dmspols ~ near_road_5km + near_road_5km*dmspols_1996-dmspols_1996 | cell_id + year | 0 | GADM_ID_3, data=data)
felm_urban_anyroad_5km_ntlbaseline <- felm(globcover_urban ~ near_road_5km + near_road_5km*dmspols_1996 - dmspols_1996 | cell_id + year | 0 | GADM_ID_3, data=data)
felm_cropland_anyroad_5km_ntlbaseline <- felm(globcover_cropland ~ near_road_5km + near_road_5km*dmspols_1996 - dmspols_1996 | cell_id + year | 0 | GADM_ID_3, data=data)

# By Speed
felm_dmspols_byspeed_5km <- felm(dmspols ~ factor(fastest_road_5km) | cell_id + year | 0 | GADM_ID_3, data=data)
felm_urban_byspeed_5km <- felm(globcover_urban ~ factor(fastest_road_5km) | cell_id + year | 0 | GADM_ID_3, data=data)
felm_cropland_byspeed_5km <- felm(globcover_cropland ~ factor(fastest_road_5km) | cell_id + year | 0 | GADM_ID_3, data=data)

felm_dmspols_byspeed_5km_ntlbaseline <- felm(dmspols ~ factor(fastest_road_5km) + factor(fastest_road_5km)*dmspols_1996 - dmspols_1996 | cell_id + year | 0 | GADM_ID_3, data=data)
felm_urban_byspeed_5km_ntlbaseline <- felm(globcover_urban ~ factor(fastest_road_5km) + factor(fastest_road_5km)*dmspols_1996 - dmspols_1996 | cell_id + year | 0 | GADM_ID_3, data=data)
felm_cropland_byspeed_5km_ntlbaseline <- felm(globcover_cropland ~ factor(fastest_road_5km) + factor(fastest_road_5km)*dmspols_1996 - dmspols_1996 | cell_id + year | 0 | GADM_ID_3, data=data)

stargazer(felm_dmspols_anyroad_5km, 
          felm_urban_anyroad_5km,
          felm_cropland_anyroad_5km,
          felm_dmspols_anyroad_5km_ntlbaseline,
          felm_urban_anyroad_5km_ntlbaseline,
          felm_cropland_anyroad_5km_ntlbaseline,
          dep.var.labels.include = F,
          column.labels = c("NTL", "Urban", "Crop", "NTL", "Urban", "Crop"),
          covariate.labels = c("Near Road", "Near Road X Baseline NTL"),
          dep.var.caption="",
          omit.stat=c("f","ser"),
          align=T,
          no.space=T,
          float=F,
          column.sep.width="8pt",
          digits=2,
          add.lines = list(
            c("Cell FE", rep("Y",6)),
            c("Year FE", rep("Y",6))
          ),
          out = file.path(project_file_path, "Results", "Tables", "near_road_5km.tex"))

stargazer(felm_dmspols_byspeed_5km, 
          felm_urban_byspeed_5km,
          felm_cropland_byspeed_5km,
          felm_dmspols_byspeed_5km_ntlbaseline,
          felm_urban_byspeed_5km_ntlbaseline,
          felm_cropland_byspeed_5km_ntlbaseline,
          dep.var.labels.include = F,
          column.labels = c("NTL", "Urban", "Crop", "NTL", "Urban", "Crop"),
          covariate.labels = c("10 km/hr", 
                               "15 km/hr", 
                               "20 km/hr", 
                               "25 km/hr", 
                               "30 km/hr", 
                               "35 km/hr",
                               "45 km/hr",
                               "50 km/hr",
                               "70 km/hr",
                               "120 km/hr",
                               "No Road X Baseline NTL",
                               "10 km/hr X Baseline NTL", 
                               "15 km/hr X Baseline NTL", 
                               "20 km/hr X Baseline NTL", 
                               "25 km/hr X Baseline NTL", 
                               "30 km/hr X Baseline NTL", 
                               "35 km/hr X Baseline NTL",
                               "45 km/hr X Baseline NTL",
                               "50 km/hr X Baseline NTL",
                               "70 km/hr X Baseline NTL",
                               "120 km/hr X Baseline NTL"),
          dep.var.caption="",
          omit.stat=c("f","ser"),
          align=T,
          no.space=T,
          float=F,
          column.sep.width="8pt",
          digits=2,
          add.lines = list(
            c("Cell FE", rep("Y",6)),
            c("Year FE", rep("Y",6))
          ),
          out = file.path(project_file_path, "Results", "Tables", "near_road_byspeed_5km.tex"))
