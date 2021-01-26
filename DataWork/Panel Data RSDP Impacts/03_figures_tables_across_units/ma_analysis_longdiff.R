# Market Access Analysis

# Regressions ------------------------------------------------------------------
for(unit in c("woreda")){ # "woreda", "clusters_of_ntl"
  for(theta in c(1,2,5,8)){
    for(log in c("_log")){
      for(exclude in c("", "_exclude20km", "_exclude50km", "_exclude100km")){ # "_exclude100km"
        
        ## Load Data
        data2012 <- readRDS(file.path(panel_rsdp_imp_data_file_path, unit, "merged_datasets",
                                      "longdiff_data_clean_base1996_end2012.Rds"))
        
        data2016 <- readRDS(file.path(panel_rsdp_imp_data_file_path, unit, "merged_datasets",
                                      "longdiff_data_clean_base1996_end2016.Rds"))
        
        ## MA Variable
        data2012$MA_var_theta1 <- data2012[[paste0("MA_pop2000_theta",1,exclude, log)]]
        data2016$MA_var_theta1 <- data2016[[paste0("MA_pop2000_theta",1,exclude, log)]]
        
        data2012$MA_var_theta8 <- data2012[[paste0("MA_pop2000_theta",8,exclude, log)]]
        data2016$MA_var_theta8 <- data2016[[paste0("MA_pop2000_theta",8,exclude, log)]]
        
        data2012$distance_city_addisababa <- data2012$distance_city_addisababa / 1000 / 100
        data2016$distance_city_addisababa <- data2016$distance_city_addisababa / 1000 / 100
        
        # Z_CODE
        lm_globcover_urban_theta1    <- felm(globcover_urban_sum ~ MA_var_theta1  | 0 | 0 | Z_CODE, data = data2016)
        lm_dmspols_zhang_2_theta1    <- felm(dmspols_zhang_sum2  ~ MA_var_theta1  | 0 | 0 | Z_CODE, data = data2012)
        lm_dmspols_zhang_6_theta1    <- felm(dmspols_zhang_sum6  ~ MA_var_theta1  | 0 | 0 | Z_CODE, data = data2012)
        lm_dmspols_zhang_ihs_theta1  <- felm(dmspols_zhang_ihs   ~ MA_var_theta1  | 0 | 0 | Z_CODE, data = data2012)
        
        lm_globcover_urban_theta8    <- felm(globcover_urban_sum ~ MA_var_theta8  | 0 | 0 | Z_CODE, data = data2016)
        lm_dmspols_zhang_2_theta8    <- felm(dmspols_zhang_sum2  ~ MA_var_theta8  | 0 | 0 | Z_CODE, data = data2012)
        lm_dmspols_zhang_6_theta8    <- felm(dmspols_zhang_sum6  ~ MA_var_theta8  | 0 | 0 | Z_CODE, data = data2012)
        lm_dmspols_zhang_ihs_theta8  <- felm(dmspols_zhang_ihs   ~ MA_var_theta8  | 0 | 0 | Z_CODE, data = data2012)
        
        stargazer(lm_globcover_urban_theta1,
                  lm_dmspols_zhang_2_theta1,
                  lm_dmspols_zhang_6_theta1,
                  lm_dmspols_zhang_ihs_theta1,
                  lm_globcover_urban_theta8,
                  lm_dmspols_zhang_2_theta8,
                  lm_dmspols_zhang_6_theta8,
                  lm_dmspols_zhang_ihs_theta8,
                  dep.var.labels.include = T,
                  dep.var.labels   = c("Urban", "NTL$>$2", "NTL$>$6", "IHS(NTL)",
                                       "Urban", "NTL$>$2", "NTL$>$6", "IHS(NTL)"),
                  #keep=c("MA_var_theta1", "MA_var_theta8"),
                  #covariate.labels = c("log(MA); $\\theta=1$",
                  #                     "log(MA); $\\theta=8$"),
                  dep.var.caption = "",
                  omit.stat = c("f","ser"), 
                  align=TRUE,
                  no.space=TRUE,
                  float=FALSE,
                  column.sep.width = "8pt",
                  digits = 2,
                  out=file.path(paper_tables,
                                paste0("MA_table_longdiff",log,exclude,"_",unit,".tex")))
      }
    }
  }
}


