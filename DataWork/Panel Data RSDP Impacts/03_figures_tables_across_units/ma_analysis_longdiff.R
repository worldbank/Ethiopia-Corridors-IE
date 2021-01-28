# Market Access Analysis

unit  <- "woreda"
theta <- 1
log <- "_log"
exclude <- ""
ubanrural <- ""

# Regressions ------------------------------------------------------------------
for(unit in c("woreda")){ # "woreda", "clusters_of_ntl"
  for(log in c("_log")){
    for(exclude in c("", "_exclude20km", "_exclude50km", "_exclude100km")){ # "_exclude100km"
      for(MA_ubanrural in c("", "_urban2", "_rural2")){
        for(subset_urbanrural in c("", "_urban2", "rural2")){
          
          ## Load Data
          data2012 <- readRDS(file.path(panel_rsdp_imp_data_file_path, unit, "merged_datasets",
                                        "longdiff_data_clean_base1996_end2012.Rds"))
          
          data2016 <- readRDS(file.path(panel_rsdp_imp_data_file_path, unit, "merged_datasets",
                                        "longdiff_data_clean_base1996_end2016.Rds"))
          
          if(subset_urbanrural == "_urban2") data2012 <- data2012[data2012$dmspols_2bin_1996 %in% 1,]
          if(subset_urbanrural == "_urban2") data2016 <- data2016[data2016$dmspols_2bin_1996 %in% 1,]
          
          if(subset_urbanrural == "_rural2") data2012 <- data2012[data2012$dmspols_2bin_1996 %in% 0,]
          if(subset_urbanrural == "_rural2") data2016 <- data2016[data2016$dmspols_2bin_1996 %in% 0,]
          
          ## MA Variable
          data2012$MA_var_theta1 <- data2012[[paste0("MA_pop2000_tt_theta",1,exclude, MA_ubanrural, log)]]
          data2016$MA_var_theta1 <- data2016[[paste0("MA_pop2000_tt_theta",1,exclude, MA_ubanrural, log)]]
          
          data2012$MA_var_theta8 <- data2012[[paste0("MA_pop2000_tt_theta",8,exclude, MA_ubanrural, log)]]
          data2016$MA_var_theta8 <- data2016[[paste0("MA_pop2000_tt_theta",8,exclude, MA_ubanrural, log)]]
          
          data2012$distance_city_addisababa <- data2012$distance_city_addisababa / 1000 / 100
          data2016$distance_city_addisababa <- data2016$distance_city_addisababa / 1000 / 100
          
          # Z_CODE data2016$dmspols_ihs_pretnd96_92
          lm_globcover_urban_theta1    <- felm(globcover_urban_sum ~ MA_var_theta1 + factor(Z_CODE) + dmspols_ihs_pretnd96_92  | 0 | 0 | Z_CODE, data = data2016)
          lm_dmspols_zhang_2_theta1    <- felm(dmspols_zhang_sum2  ~ MA_var_theta1 + factor(Z_CODE) + dmspols_ihs_pretnd96_92  | 0 | 0 | Z_CODE, data = data2012)
          lm_dmspols_zhang_6_theta1    <- felm(dmspols_zhang_sum6  ~ MA_var_theta1 + factor(Z_CODE) + dmspols_ihs_pretnd96_92  | 0 | 0 | Z_CODE, data = data2012)
          lm_dmspols_zhang_ihs_theta1  <- felm(dmspols_zhang_ihs   ~ MA_var_theta1 + factor(Z_CODE) + dmspols_ihs_pretnd96_92  | 0 | 0 | Z_CODE, data = data2012)
          
          lm_globcover_urban_theta8    <- felm(globcover_urban_sum ~ MA_var_theta8 + factor(Z_CODE) + dmspols_ihs_pretnd96_92  | 0 | 0 | Z_CODE, data = data2016)
          lm_dmspols_zhang_2_theta8    <- felm(dmspols_zhang_sum2  ~ MA_var_theta8 + factor(Z_CODE) + dmspols_ihs_pretnd96_92  | 0 | 0 | Z_CODE, data = data2012)
          lm_dmspols_zhang_6_theta8    <- felm(dmspols_zhang_sum6  ~ MA_var_theta8 + factor(Z_CODE) + dmspols_ihs_pretnd96_92  | 0 | 0 | Z_CODE, data = data2012)
          lm_dmspols_zhang_ihs_theta8  <- felm(dmspols_zhang_ihs   ~ MA_var_theta8 + factor(Z_CODE) + dmspols_ihs_pretnd96_92  | 0 | 0 | Z_CODE, data = data2012)
          
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
                    out=file.path(paper_tables,
                                  paste0("MA_table_longdiff",log,exclude,MA_ubanrural,"_",unit,"_subset",subset_urbanrural,".tex")))
        }
      }
    }
  }
}



