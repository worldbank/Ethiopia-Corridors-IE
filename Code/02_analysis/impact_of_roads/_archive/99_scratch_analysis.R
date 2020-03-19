
data <- readRDS(file.path(project_file_path, "Data", "FinalData", "dmspols_grid_dataset", "dmspols_level_dataset_5percentsample_analysisvars.Rds"))

felm_allroads_yearssince_dmspols_group <- felm(dmspols_zhang ~ factor(years_since_improved_all_group) | cell_id + year | 0 | GADM_ID_3, data=data)
felm_allroads_yearssince_dmspols <- felm(dmspols_zhang ~ factor(years_since_improved_all) | cell_id + year | 0 | GADM_ID_3, data=data)

stargazer()

summary(felm_allroads_yearssince_dmspols_group)
summary(felm_allroads_yearssince_dmspols)

data$years_since_improved_all_group %>% table
data$years_since_improved_all %>% table

data$years_since_improved_all_group %>% is.na %>% table
data$years_since_improved_all %>% is.na %>% table
