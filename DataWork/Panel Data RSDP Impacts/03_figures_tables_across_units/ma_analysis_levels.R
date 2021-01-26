# Market Access Analysis

# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(panel_rsdp_imp_data_file_path, "woreda", "merged_datasets", "panel_data_clean.Rds"))

data <- data %>%
  filter(year >= 1996)

# Regressions ------------------------------------------------------------------
for(unit in c("woreda")){ # "clusters_of_ntl", "clusters_of_globcover_urban"
  for(theta in c(1,2,5,8)){
    for(log in c("_log")){
      for(exclude in c("", "_exclude20km", "_exclude50km", "_exclude100km")){ # "_exclude100km"
        
        ## Load/Subset Data
        data <- readRDS(file.path(panel_rsdp_imp_data_file_path, unit, "merged_datasets", "panel_data_clean.Rds"))
        
        data <- data %>%
          filter(year >= 1996)
        
        ## Prep MA Var
        data$MA_var_theta1 <- data[[paste0("MA_pop2000_theta",1,exclude, log)]]
        data$MA_var_theta8 <- data[[paste0("MA_pop2000_theta",8,exclude, log)]]
        
        lm_globcover_urban_theta1    <- felm(globcover_urban_sum ~ MA_var_theta1 + temp_avg + precipitation | year + cell_id | 0 | Z_CODE, data = data)
        lm_dmspols_zhang_2_theta1    <- felm(dmspols_zhang_sum2  ~ MA_var_theta1 + temp_avg + precipitation | year + cell_id | 0 | Z_CODE, data = data)
        lm_dmspols_zhang_6_theta1    <- felm(dmspols_zhang_sum6  ~ MA_var_theta1 + temp_avg + precipitation | year + cell_id | 0 | Z_CODE, data = data)
        lm_dmspols_zhang_ihs_theta1  <- felm(dmspols_zhang_ihs   ~ MA_var_theta1 + temp_avg + precipitation | year + cell_id | 0 | Z_CODE, data = data)

        lm_globcover_urban_theta8    <- felm(globcover_urban_sum ~ MA_var_theta8 + temp_avg + precipitation | year + cell_id | 0 | Z_CODE, data = data)
        lm_dmspols_zhang_2_theta8    <- felm(dmspols_zhang_sum2  ~ MA_var_theta8 + temp_avg + precipitation | year + cell_id | 0 | Z_CODE, data = data)
        lm_dmspols_zhang_6_theta8    <- felm(dmspols_zhang_sum6  ~ MA_var_theta8 + temp_avg + precipitation | year + cell_id | 0 | Z_CODE, data = data)
        lm_dmspols_zhang_ihs_theta8  <- felm(dmspols_zhang_ihs   ~ MA_var_theta8 + temp_avg + precipitation | year + cell_id | 0 | Z_CODE, data = data)
        
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
                  keep=c("MA_var_theta1", "MA_var_theta8"),
                  covariate.labels = c("log(MA); $\\theta=1$",
                                       "log(MA); $\\theta=8$"),
                  dep.var.caption = "",
                  omit.stat = c("f","ser"), 
                  align=TRUE,
                  no.space=TRUE,
                  float=FALSE,
                  column.sep.width = "8pt",
                  digits = 2,
                  add.lines = list(
                    c("Year   FE", "Y", "Y", "Y","Y", "Y", "Y"),
                    c("Woreda FE", "Y", "Y", "Y","Y", "Y", "Y")
                  ),
                  out=file.path(paper_tables,
                                paste0("MA_table",log,exclude,"_",unit,".tex")))
      }
    }
  }
}




