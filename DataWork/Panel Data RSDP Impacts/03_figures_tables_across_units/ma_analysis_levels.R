# Market Access Analysis

# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(panel_rsdp_imp_data_file_path, "woreda", "merged_datasets", "panel_data_clean.Rds"))

data <- data %>%
  filter(year >= 1996)

# Regressions ------------------------------------------------------------------
for(unit in c("woreda", "clusters_of_ntl", "clusters_of_globcover_urban")){
  for(theta in c(1,2,5,8)){
    for(log in c("_log")){
      for(exclude100 in c("")){ # "_exclude100km"
        
        ## Load/Subset Data
        data <- readRDS(file.path(panel_rsdp_imp_data_file_path, unit, "merged_datasets", "panel_data_clean.Rds"))
        
        data <- data %>%
          filter(year >= 1996)
        
        ## Prep MA Var
        data$MA_var <- data[[paste0("MA_pop2000_theta",theta,exclude100, log)]]
        
        data$globcover_urban_sum_log <- log(data$globcover_urban_sum + 1)
        data$dmspols_zhang_sum2_log <- log(data$dmspols_zhang_sum2 + 1)
        data$dmspols_zhang_sum6_log <- log(data$dmspols_zhang_sum6 + 1)
        
        lm_globcover_urban    <- felm(globcover_urban_sum     ~ MA_var + temp_avg + precipitation | year + cell_id | 0 | Z_CODE, data = data)
        lm_dmspols_zhang_2    <- felm(dmspols_zhang_sum2     ~ MA_var + temp_avg + precipitation | year + cell_id | 0 | Z_CODE, data = data)
        lm_dmspols_zhang_6    <- felm(dmspols_zhang_sum6     ~ MA_var + temp_avg + precipitation | year + cell_id | 0 | Z_CODE, data = data)
        lm_dmspols_zhang_ihs  <- felm(dmspols_zhang_ihs   ~ MA_var + temp_avg + precipitation | year + cell_id | 0 | Z_CODE, data = data)

        stargazer(lm_globcover_urban,
                  lm_dmspols_zhang_2,
                  lm_dmspols_zhang_6,
                  lm_dmspols_zhang_ihs,
                  dep.var.labels.include = T,
                  dep.var.labels   = c("Urban", "NTL$>$2", "NTL$>$6", "IHS(NTL)"),
                  keep=c("MA_var"),
                  covariate.labels = c("log(MA)"),
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
                                paste0("MA_table_theta",theta,log,exclude100,"_",unit,".tex")))
      }
    }
  }
}




