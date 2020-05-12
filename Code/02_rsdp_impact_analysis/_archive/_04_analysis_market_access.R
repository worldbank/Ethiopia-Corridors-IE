# Impact of Expressway Expansion
# Ethiopia IE

# Setup ------------------------------------------------------------------------
if(Sys.info()[["user"]] == "r521633") project_file_path <- "/home/wb521633/IEs/Ethiopia IE"
if(Sys.info()[["user"]] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE"
if(Sys.info()[["user"]] == "robmarty") project_file_path <- "~/Dropbox/World Bank/IEs/Ethiopia IE"

set.seed(42)

library(lfe)
library(stargazer)

RANDOM_SUBSET <- FALSE

# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(project_file_path, "Data", "FinalData", "dmspols_grid_dataset", "dmspols_level_dataset_evenyears.Rds"))

# Keep relevant variables
data <- subset(data, select=c(cell_id, year, GADM_ID_1, GADM_ID_2, GADM_ID_3, dmspols, MA_constantpop))

# Subset to random sample
if(RANDOM_SUBSET == TRUE){
  cells_to_keep <- sample(x=unique(data$cell_id), size=round(length(unique(data$cell_id))*0.10))
  data <- data[data$cell_id %in% cells_to_keep,]
}

# Adjust variables
data$GADM_ID_1 <- as.factor(data$GADM_ID_1)
data$GADM_ID_2 <- as.factor(data$GADM_ID_2)
data$GADM_ID_3 <- as.factor(data$GADM_ID_3)

# Add Baseline Variables -------------------------------------------------------
data_1992 <- data[data$year == 1992,] %>% 
  subset(select=c(cell_id, dmspols, MA_constantpop)) %>%
  dplyr::rename(dmspols_1992 = dmspols,
                MA_constantpop_1992 = MA_constantpop)
data_1996 <- data[data$year == 1996,] %>% 
  subset(select=c(cell_id, dmspols, MA_constantpop)) %>%
  dplyr::rename(dmspols_1996 = dmspols,
                MA_constantpop_1996 = MA_constantpop)
data_1998 <- data[data$year == 1998,] %>% 
  subset(select=c(cell_id, dmspols, MA_constantpop)) %>%
  dplyr::rename(dmspols_1998 = dmspols,
                MA_constantpop_1998 = MA_constantpop)

#data <- data[data$year == 2012,]
data <- merge(data, data_1992, by="cell_id")
data <- merge(data, data_1996, by="cell_id")
data <- merge(data, data_1998, by="cell_id")

# Add Variables ----------------------------------------------------------------
data$dmspols_ln <- log(data$dmspols+1)
data$MA_constantpop_ln <- log(data$MA_constantpop)

ma_ntl_elasticty <- felm(dmspols_ln ~ MA_constantpop_ln | cell_id + year | 0 | 0 ,data=data)

data_2012 <- data[data$year == 2012,]
data_2012$change_ln_dmspols <- log(data_2012$dmspols+1) - log(data_2012$dmspols_1996+1)
data_2012$change_ln_MA_constantpop <- log(data_2012$MA_constantpop+1) - log(data_2012$MA_constantpop_1996+1)
data_2012$MA_constantpop_1996_ln <- log(data_2012$MA_constantpop_1996)
data_2012$dmspols_1996_ln <- log(data_2012$dmspols_1996+1)

ma_ntl_change_elasticty <- felm(change_ln_dmspols ~ change_ln_MA_constantpop + MA_constantpop_1996_ln + dmspols_1996_ln | GADM_ID_3 | 0 | 0 , data=data_2012)

data_2012$dmspols_1996_1992_ln <- log(data_2012$dmspols_1996+1) - log(data_2012$dmspols_1992+1)
data_2012$MA_constantpop_2012_1998_ln <- log(data_2012$MA_constantpop) - log(data_2012$MA_constantpop_1998)

ma_ntl_change_elasticty <- felm(dmspols_1996_1992_ln ~ MA_constantpop_2012_1998_ln | GADM_ID_1 | 0 | 0 , data=data_2012)


# Regression -------------------------------------------------------------------
#### 5km Buffer

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
