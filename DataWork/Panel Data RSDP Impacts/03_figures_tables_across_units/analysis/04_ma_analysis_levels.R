# Market Access Analysis

unit <- "woreda"
theta <- "3_8"
log <- "_log"
exclude <- "_exclude50km"

# Regressions ------------------------------------------------------------------
for(unit in c("woreda", "kebele")){ 
  for(theta in c("1","2","3_8","5","8")){ # 
    for(log in c("_log")){
      for(exclude in c("_exclude20km", "_exclude50km", "_exclude100km")){ 
        
        print(paste(unit, theta, log, exclude))
        
        #### Load/Subset Data
        data <- readRDS(file.path(panel_rsdp_imp_data_file_path, unit, "merged_datasets", "panel_data_clean.Rds"))
        
        data <- data %>%
          filter(year >= 1996)
        
        #### Cluster Var and FE Var
        if(unit %in% "woreda") data$cluster_var <- data$Z_CODE
        if(unit %in% "kebele") data$cluster_var <- data$woreda_id
        
        #### Prep Variables
        data$MA_var     <- data[[paste0("MA_pop2000_tt_theta",theta, log)]]
        data$MA_var_exc <- data[[paste0("MA_pop2000_tt_theta",theta,exclude, log)]]
        
        data <- data[data$MA_var != Inf,] # CAN DELETE ME!
        
        data$distance_city_addisababa <- data$distance_city_addisababa / 1000 / 100

        ## Interaction Vars - MA
        data$MA_varXdmspols_harmon_ihs_1996      <- data$MA_var * data$dmspols_harmon_ihs_1996
        data$MA_varXglobcover_urban_1996         <- data$MA_var * data$globcover_urban_1996
        data$MA_varXdistance_city_addisababa     <- data$MA_var * data$distance_city_addisababa
        data$MA_varXglobcover_urban_sum_ihs_1996 <- data$MA_var * data$globcover_urban_sum_ihs_1996

        data$MA_varXdmspols_harmon_1996_bin4_1 <- data$MA_var * data$dmspols_harmon_1996_bin4_1
        data$MA_varXdmspols_harmon_1996_bin4_2 <- data$MA_var * data$dmspols_harmon_1996_bin4_2
        data$MA_varXdmspols_harmon_1996_bin4_3 <- data$MA_var * data$dmspols_harmon_1996_bin4_3
        data$MA_varXdmspols_harmon_1996_bin4_4 <- data$MA_var * data$dmspols_harmon_1996_bin4_4

        ## Interaction Vars - MA_exclude
        data$MA_var_excXdmspols_harmon_ihs_1996       <- data$MA_var_exc * data$dmspols_harmon_ihs_1996
        data$MA_var_excXglobcover_urban_1996         <- data$MA_var_exc * data$globcover_urban_1996
        data$MA_var_excXdistance_city_addisababa     <- data$MA_var_exc * data$distance_city_addisababa
        data$MA_var_excXglobcover_urban_sum_ihs_1996 <- data$MA_var_exc * data$globcover_urban_sum_ihs_1996

        data$MA_var_excXdmspols_harmon_1996_bin4_1 <- data$MA_var_exc * data$dmspols_harmon_1996_bin4_1
        data$MA_var_excXdmspols_harmon_1996_bin4_2 <- data$MA_var_exc * data$dmspols_harmon_1996_bin4_2
        data$MA_var_excXdmspols_harmon_1996_bin4_3 <- data$MA_var_exc * data$dmspols_harmon_1996_bin4_3
        data$MA_var_excXdmspols_harmon_1996_bin4_4 <- data$MA_var_exc * data$dmspols_harmon_1996_bin4_4

        ## OLS Regressions
        ols1 <- felm(dmspols_harmon_ihs ~ MA_var                                       + temp_avg + precipitation | year + cell_id | 0                     | cluster_var, data = data)
        ols2 <- felm(dmspols_harmon_ihs ~ MA_var + MA_varXdmspols_harmon_1996_bin4_2 + MA_varXdmspols_harmon_1996_bin4_3 + MA_varXdmspols_harmon_1996_bin4_4 + temp_avg + precipitation | year + cell_id | 0  | cluster_var, data = data)
        ols3 <- felm(dmspols_harmon_ihs ~ MA_var + MA_varXdistance_city_addisababa + temp_avg + precipitation | year + cell_id | 0                     | cluster_var, data = data) 
        
        ols4 <- felm(globcover_urban_sum_ihs ~ MA_var                                       + temp_avg + precipitation | year + cell_id | 0                     | cluster_var, data = data)
        ols5 <- felm(globcover_urban_sum_ihs ~ MA_var + MA_varXdmspols_harmon_1996_bin4_2 + MA_varXdmspols_harmon_1996_bin4_3 + MA_varXdmspols_harmon_1996_bin4_4 + temp_avg + precipitation | year + cell_id | 0  | cluster_var, data = data)
        ols6 <- felm(globcover_urban_sum_ihs ~ MA_var + MA_varXdistance_city_addisababa + temp_avg + precipitation | year + cell_id | 0                     | cluster_var, data = data) 
        
        ols7 <- felm(globcover_cropland_sum_ihs ~ MA_var                                       + temp_avg + precipitation | year + cell_id | 0                     | cluster_var, data = data)
        ols8 <- felm(globcover_cropland_sum_ihs ~ MA_var + MA_varXdmspols_harmon_1996_bin4_2 + MA_varXdmspols_harmon_1996_bin4_3 + MA_varXdmspols_harmon_1996_bin4_4 + temp_avg + precipitation | year + cell_id | 0  | cluster_var, data = data)
        ols9 <- felm(globcover_cropland_sum_ihs ~ MA_var + MA_varXdistance_city_addisababa + temp_avg + precipitation | year + cell_id | 0                     | cluster_var, data = data) 
        
        ## IV Regressions
        iv1 <- felm(dmspols_harmon_ihs ~ temp_avg + precipitation           | year + cell_id | (MA_var ~ MA_var_exc) |                                                                       cluster_var, data = data) 
        iv2 <- felm(dmspols_harmon_ihs ~ temp_avg + precipitation           | year + cell_id | (MA_var|MA_varXdmspols_harmon_1996_bin4_2|MA_varXdmspols_harmon_1996_bin4_3|MA_varXdmspols_harmon_1996_bin4_4 ~ MA_var_exc + MA_var_excXdmspols_harmon_1996_bin4_2 + MA_var_excXdmspols_harmon_1996_bin4_3 + MA_var_excXdmspols_harmon_1996_bin4_4) | cluster_var, data = data) 
        iv3 <- felm(dmspols_harmon_ihs ~ temp_avg + precipitation           | year + cell_id | (MA_var|MA_varXdistance_city_addisababa ~ MA_var_exc + MA_var_excXdistance_city_addisababa) | cluster_var, data = data) 
        
        iv4 <- felm(globcover_urban_sum_ihs ~ temp_avg + precipitation      | year + cell_id | (MA_var ~ MA_var_exc) |                                                                       cluster_var, data = data) 
        iv5 <- felm(globcover_urban_sum_ihs ~ temp_avg + precipitation      | year + cell_id | (MA_var|MA_varXdmspols_harmon_1996_bin4_2|MA_varXdmspols_harmon_1996_bin4_3|MA_varXdmspols_harmon_1996_bin4_4 ~ MA_var_exc + MA_var_excXdmspols_harmon_1996_bin4_2 + MA_var_excXdmspols_harmon_1996_bin4_3 + MA_var_excXdmspols_harmon_1996_bin4_4) | cluster_var, data = data) 
        iv6 <- felm(globcover_urban_sum_ihs ~ temp_avg + precipitation      | year + cell_id | (MA_var|MA_varXdistance_city_addisababa ~ MA_var_exc + MA_var_excXdistance_city_addisababa) | cluster_var, data = data)
        
        iv7 <- felm(globcover_cropland_sum_ihs ~ temp_avg + precipitation   | year + cell_id | (MA_var ~ MA_var_exc) |                                                                       cluster_var, data = data) 
        iv8 <- felm(globcover_cropland_sum_ihs ~ temp_avg + precipitation   | year + cell_id | (MA_var|MA_varXdmspols_harmon_1996_bin4_2|MA_varXdmspols_harmon_1996_bin4_3|MA_varXdmspols_harmon_1996_bin4_4 ~ MA_var_exc + MA_var_excXdmspols_harmon_1996_bin4_2 + MA_var_excXdmspols_harmon_1996_bin4_3 + MA_var_excXdmspols_harmon_1996_bin4_4) | cluster_var, data = data) 
        iv9 <- felm(globcover_cropland_sum_ihs ~ temp_avg + precipitation   | year + cell_id | (MA_var|MA_varXdistance_city_addisababa ~ MA_var_exc + MA_var_excXdistance_city_addisababa) | cluster_var, data = data)
        
        ## OLS Stargazer
        stargazer(ols1,
                  ols2,
                  ols3,
                  ols4,
                  ols5,
                  ols6,
                  ols7,
                  ols8,
                  ols9,
                  dep.var.labels.include = T,
                  dep.var.labels   = c("NTL", "Urban", "Cropland"), # "NTL$\\geq$2", "NTL$\\geq6$",
                  #keep=c("MA_var"),
                  omit = c("temp_avg", "precipitation"),
                  covariate.labels = c("MA",
                                       "MA$\\times NTL_{96}$ Low",
                                       "MA$\\times NTL_{96}$ Med",
                                       "MA$\\times NTL_{96}$ High",
                                       "MA X Dist Addis"),
                  dep.var.caption = "",
                  omit.stat = c("f","ser"), 
                  align=TRUE,
                  no.space=TRUE,
                  float=FALSE,
                  column.sep.width = "8pt",
                  digits = 2,
                  omit.table.layout = "n",
                  add.lines = list(
                    c("Year   FE", rep("Y", 8)),
                    c("Unit FE", rep("Y", 8))
                  ),
                  out=file.path(paper_tables,
                                paste0("MA_table",log,"_theta",theta,"_",unit,"_ols.tex")))
        
        ## IV Stargazer
        stargazer(iv1,
                  iv2,
                  iv3,
                  iv4,
                  iv5,
                  iv6,
                  iv7,
                  iv8,
                  iv9,
                  dep.var.labels.include = T,
                  dep.var.labels   = c("NTL", "Urban", "Cropland"), # "NTL$\\geq$2", "NTL$\\geq6$",
                  #keep=c("MA_var"),
                  omit = c("temp_avg", "precipitation"),
                  covariate.labels = c("MA",
                                       "MA$\\times NTL_{96}$ Low",
                                       "MA$\\times NTL_{96}$ Med",
                                       "MA$\\times NTL_{96}$ High",
                                       "MA X Dist Addis"),
                  dep.var.caption = "",
                  omit.stat = c("f","ser"), 
                  align=TRUE,
                  no.space=TRUE,
                  float=FALSE,
                  column.sep.width = "8pt",
                  digits = 2,
                  omit.table.layout = "n",
                  add.lines = list(
                    c("Year   FE", rep("Y", 8)),
                    c("Unit FE", rep("Y", 8))
                  ),
                  out=file.path(paper_tables,
                                paste0("MA_table",log,"_theta",theta,exclude,"_",unit,"_iv.tex")))
        
      }
    }
  }
}





