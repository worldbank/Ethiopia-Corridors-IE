# Market Access Analysis

unit  <- "woreda"
log = "_log"
theta = "3_8"
exclude = "_exclude50km"

# Regressions ------------------------------------------------------------------
for(unit in c("woreda", "kebele")){ # "woreda", "clusters_of_ntl"
  for(log in c("_log")){
    for(theta in c("1", "2", "3_8", "5", "8")){ # 
      for(exclude in c("_exclude20km", "_exclude50km", "_exclude100km")){ # "_exclude20km", "_exclude50km", "_exclude100km"
        
        #### Load Data
        data <- readRDS(file.path(panel_rsdp_imp_data_file_path, unit, "merged_datasets",
                                  "longdiff_data_clean_base1996_end2016.Rds"))
        
        #### Cluster Var and FE Var
        if(unit %in% "woreda") data$cluster_var <- data$Z_CODE
        if(unit %in% "kebele") data$cluster_var <- data$woreda_id
        
        if(unit %in% "woreda") data$fe_var <- data$Z_CODE
        if(unit %in% "kebele") data$fe_var <- data$Z_CODE
        
        #### Prep Data
        
        ## Dist Addis
        data$distance_city_addisababa <- data$distance_city_addisababa / 1000 / 100
        
        ## MA Variables
        data$MA_var      <- data[[paste0("MA_pop2000_tt_theta",theta, log)]]
        data$MA_var_1996 <- data[[paste0("MA_pop2000_tt_theta",theta, log, "_1996")]]
        
        data$MA_var_exc      <- data[[paste0("MA_pop2000_tt_theta",theta, exclude, log)]]
        data$MA_var_exc_1996 <- data[[paste0("MA_pop2000_tt_theta",theta, exclude, log, "_1996")]]
        
        ## Interactions - MA
        data$MA_varXdistance_city_addisababa <- data$MA_var * data$distance_city_addisababa
        data$MA_varXglobcover_urban_1996     <- data$MA_var * data$globcover_urban_1996
        
        data$MA_varXdmspols_harmon_ihs_1996 <- data$MA_var * data$dmspols_harmon_ihs_1996
        
        data$MA_varXdmspols_harmon_1996_bin4_1 <- data$MA_var * data$dmspols_harmon_1996_bin4_1
        data$MA_varXdmspols_harmon_1996_bin4_2 <- data$MA_var * data$dmspols_harmon_1996_bin4_2
        data$MA_varXdmspols_harmon_1996_bin4_3 <- data$MA_var * data$dmspols_harmon_1996_bin4_3
        data$MA_varXdmspols_harmon_1996_bin4_4 <- data$MA_var * data$dmspols_harmon_1996_bin4_4
        
        ## Interactions - MA_exclude
        data$MA_var_excXdistance_city_addisababa <- data$MA_var_exc * data$distance_city_addisababa
        data$MA_var_excXglobcover_urban_1996     <- data$MA_var_exc * data$globcover_urban_1996
        
        data$MA_var_excXdmspols_harmon_ihs_1996 <- data$MA_var_exc * data$dmspols_harmon_ihs_1996
        
        data$MA_var_excXdmspols_harmon_1996_bin4_1 <- data$MA_var_exc * data$dmspols_harmon_1996_bin4_1
        data$MA_var_excXdmspols_harmon_1996_bin4_2 <- data$MA_var_exc * data$dmspols_harmon_1996_bin4_2
        data$MA_var_excXdmspols_harmon_1996_bin4_3 <- data$MA_var_exc * data$dmspols_harmon_1996_bin4_3
        data$MA_var_excXdmspols_harmon_1996_bin4_4 <- data$MA_var_exc * data$dmspols_harmon_1996_bin4_4
        
        #### OLS
        ols1 <- felm(dmspols_harmon_ihs         ~ MA_var                                                                                         + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var | 0 | cluster_var, data = data)
        ols2 <- felm(dmspols_harmon_ihs         ~ MA_var + MA_varXdmspols_harmon_1996_bin4_2 + MA_varXdmspols_harmon_1996_bin4_3 + MA_varXdmspols_harmon_1996_bin4_4  + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var | 0 | cluster_var, data = data) 
        ols3 <- felm(dmspols_harmon_ihs         ~ MA_var + MA_varXdistance_city_addisababa                                                       + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var | 0 | cluster_var, data = data) 
        
        ols4 <- felm(globcover_urban_sum_ihs     ~ MA_var                                                                                         + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var | 0 | cluster_var, data = data)
        ols5 <- felm(globcover_urban_sum_ihs    ~ MA_var + MA_varXdmspols_harmon_1996_bin4_2 + MA_varXdmspols_harmon_1996_bin4_3 + MA_varXdmspols_harmon_1996_bin4_4  + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var | 0 | cluster_var, data = data) 
        ols6 <- felm(globcover_urban_sum_ihs     ~ MA_var + MA_varXdistance_city_addisababa                                                       + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var | 0 | cluster_var, data = data) 
        
        ols7 <- felm(globcover_cropland_sum_ihs ~ MA_var                                                                                         + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var | 0 | cluster_var, data = data)
        ols8 <- felm(globcover_cropland_sum_ihs ~ MA_var + MA_varXdmspols_harmon_1996_bin4_2 + MA_varXdmspols_harmon_1996_bin4_3 + MA_varXdmspols_harmon_1996_bin4_4  + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var | 0 | cluster_var, data = data) 
        ols9 <- felm(globcover_cropland_sum_ihs ~ MA_var + MA_varXdistance_city_addisababa                                                       + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | fe_var | 0 | cluster_var, data = data) 
        
        #### IV
        iv1 <- felm(dmspols_harmon_ihs         ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var | (MA_var ~ MA_var_exc)                                                                             | cluster_var, data = data)
        iv2 <- felm(dmspols_harmon_ihs         ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var | (MA_var|MA_varXdmspols_harmon_1996_bin4_2|MA_varXdmspols_harmon_1996_bin4_3|MA_varXdmspols_harmon_1996_bin4_4      ~ MA_var_exc + MA_var_excXdmspols_harmon_1996_bin4_2 + MA_var_excXdmspols_harmon_1996_bin4_3 + MA_var_excXdmspols_harmon_1996_bin4_4)      | cluster_var, data = data)
        iv3 <- felm(dmspols_harmon_ihs         ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var | (MA_var|MA_varXdistance_city_addisababa    ~ MA_var_exc + MA_var_excXdistance_city_addisababa)    | cluster_var, data = data) 
        
        iv4 <- felm(globcover_urban_sum_ihs    ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var | (MA_var ~ MA_var_exc)                                                                             | cluster_var, data = data)
        iv5 <- felm(globcover_urban_sum_ihs    ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var | (MA_var|MA_varXdmspols_harmon_1996_bin4_2|MA_varXdmspols_harmon_1996_bin4_3|MA_varXdmspols_harmon_1996_bin4_4      ~ MA_var_exc + MA_var_excXdmspols_harmon_1996_bin4_2 + MA_var_excXdmspols_harmon_1996_bin4_3 + MA_var_excXdmspols_harmon_1996_bin4_4)      | cluster_var, data = data)
        iv6 <- felm(globcover_urban_sum_ihs    ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var | (MA_var|MA_varXdistance_city_addisababa    ~ MA_var_exc + MA_var_excXdistance_city_addisababa)    | cluster_var, data = data) 
        
        iv7 <- felm(globcover_cropland_sum_ihs ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var | (MA_var ~ MA_var_exc)                                                                             | cluster_var, data = data)
        iv8 <- felm(globcover_cropland_sum_ihs ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var | (MA_var|MA_varXdmspols_harmon_1996_bin4_2|MA_varXdmspols_harmon_1996_bin4_3|MA_varXdmspols_harmon_1996_bin4_4      ~ MA_var_exc + MA_var_excXdmspols_harmon_1996_bin4_2 + MA_var_excXdmspols_harmon_1996_bin4_3 + MA_var_excXdmspols_harmon_1996_bin4_4)      | cluster_var, data = data)
        iv9 <- felm(globcover_cropland_sum_ihs ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | fe_var | (MA_var|MA_varXdistance_city_addisababa    ~ MA_var_exc + MA_var_excXdistance_city_addisababa)    | cluster_var, data = data) 
        
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
                  omit = c("Z_CODE", "Constant"),
                  #keep=c("MA_var", "MA_var_1996", "dmspols_ihs_1996", "dmspols_ihs_pretnd96_92", "globcover_urban_sum_pretnd96_92.y"),
                  covariate.labels = c("MA",
                                       "MA$\\times NTL_{96}$ Low",
                                       "MA$\\times NTL_{96}$ Med",
                                       "MA$\\times NTL_{96}$ High",
                                       "MA X Dist Addis",
                                       "MA, 1996",
                                       "Log mean light, 1996",
                                       "Pre-trend: log mean light",
                                       "Pre-trend: log N urban pixels"),
                  #covariate.labels = c("log(MA); $\\theta=1$",
                  #                     "log(MA); $\\theta=8$"),
                  dep.var.caption = "",
                  omit.stat = c("f","ser", "rsq"), 
                  align=TRUE,
                  no.space=TRUE,
                  float=FALSE,
                  column.sep.width = "8pt",
                  digits = 2,
                  omit.table.layout = "n",
                  #add.lines = list(
                  #  #c("Zone FEs", rep("Y", 10)),
                  #  c("MA IV, 50km Doughnut", rep("N", 5), rep("Y", 5))
                  #),
                  out=file.path(paper_tables,
                                paste0("MA","_table_longdiff_theta",theta,log,"_",unit,"_ols.tex")))
        
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
                  dep.var.labels   = c("NTL", "Urban", "Cropland"), #  "NTL$\\geq$2", "NTL$\\geq6$",
                  omit = c("Z_CODE", "Constant"),
                  order = c(5:9, 1:4),
                  #keep=c("MA_var", "MA_var_1996", "dmspols_ihs_1996", "dmspols_ihs_pretnd96_92", "globcover_urban_sum_pretnd96_92.y"),
                  covariate.labels = c("MA",
                                       "MA$\\times NTL_{96}$ Low",
                                       "MA$\\times NTL_{96}$ Med",
                                       "MA$\\times NTL_{96}$ High",
                                       "MA X Dist Addis",
                                       "MA, 1996",
                                       "Log mean light, 1996",
                                       "Pre-trend: log mean light",
                                       "Pre-trend: log N urban pixels"),
                  #covariate.labels = c("log(MA); $\\theta=1$",
                  #                     "log(MA); $\\theta=8$"),
                  dep.var.caption = "",
                  omit.stat = c("f","ser", "rsq"), 
                  align=TRUE,
                  no.space=TRUE,
                  float=FALSE,
                  column.sep.width = "8pt",
                  digits = 2,
                  omit.table.layout = "n",
                  #add.lines = list(
                  #  #c("Zone FEs", rep("Y", 10)),
                  #  c("MA IV, 50km Doughnut", rep("N", 5), rep("Y", 5))
                  #),
                  out=file.path(paper_tables,
                                paste0("MA","_table_longdiff_theta",theta,exclude,log,"_",unit,"_iv.tex")))
      }
    }
  }
}



