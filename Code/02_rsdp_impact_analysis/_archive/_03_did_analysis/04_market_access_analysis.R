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

data <- data[(data$year >= 1996) & (data$year <= 2016),]

# Prep Variables ---------------------------------------------------------------
data$MA_constantpop_log <- log(data$MA_constantpop)
data$dmspols_log <- log(data$dmspols+1)
data$region_type <- ifelse(data$GADM_ID_1 %in% c("Afar", "Benshangul-Gumaz", "Somali"), "Sparse", "Dense")
data$dense_region <- as.numeric(data$region_type %in% "Dense") %>% as.factor

# Regressions ------------------------------------------------------------------
#### DMSP-OLS
lm_dmspols <- felm(dmspols_log ~ MA_constantpop_log | cell_id + year | 0 | GADM_ID_3, data=data)
lm_dmspols_dense <- felm(dmspols_log ~ MA_constantpop_log + MA_constantpop_log*dense_region - dense_region | cell_id + year | 0 | GADM_ID_3, data=data)
lm_dmspols_ntlbase <- felm(dmspols_log ~ MA_constantpop_log + MA_constantpop_log*factor(dmspols_1997_group) - factor(dmspols_1997_group) | cell_id + year | 0 | GADM_ID_3, data=data)
lm_dmspols_dense_ntlbase <- felm(dmspols_log ~ MA_constantpop_log + MA_constantpop_log*dense_region*factor(dmspols_1997_group) - dense_region - factor(dmspols_1997_group) - dense_region*factor(dmspols_1997_group) | cell_id + year | 0 | GADM_ID_3, data=data)

#### Globcover-Urban
lm_urban <- felm(globcover_urban ~ MA_constantpop_log | cell_id + year | 0 | GADM_ID_3, data=data)
lm_urban_dense <- felm(globcover_urban ~ MA_constantpop_log + MA_constantpop_log*dense_region - dense_region | cell_id + year | 0 | GADM_ID_3, data=data)
lm_urban_ntlbase <- felm(globcover_urban ~ MA_constantpop_log + MA_constantpop_log*factor(dmspols_1997_group) - factor(dmspols_1997_group) | cell_id + year | 0 | GADM_ID_3, data=data)
lm_urban_dense_ntlbase <- felm(globcover_urban ~ MA_constantpop_log + MA_constantpop_log*dense_region*factor(dmspols_1997_group) - dense_region - factor(dmspols_1997_group) - dense_region*factor(dmspols_1997_group) | cell_id + year | 0 | GADM_ID_3, data=data)

stargazer(lm_urban, 
          lm_urban_dense,
          lm_urban_ntlbase,
          lm_urban_dense_ntlbase,
          lm_dmspols, 
          lm_dmspols_dense,
          lm_dmspols_ntlbase,
          lm_dmspols_dense_ntlbase,
          covariate.labels = c("MA",
                               "MA $\\times$ Dense",
                               "MA $\\times$ NTL Baseline, Low",
                               "MA $\\times$ NTL Baseline, High",
                               "MA $\\times$ Dense $\\times$ NTL Baseline, Low",
                               "MA $\\times$ Dense $\\times$ NTL Baseline, High"),
          dep.var.labels = c("Globcover: Urban", "DMSP-OLS"),
          omit.stat = c("f","ser"), 
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width = "8pt",
          digits = 2,
          out=file.path(tables_file_path, "market_access_reg_urban_dmspols.tex"))


