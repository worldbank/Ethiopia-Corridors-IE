# Market Access Analysis

unit  <- "woreda"
theta <- 1
log <- "_log"
exclude <- ""
MA_ubanrural <- ""
subset_urbanrural <- ""

# Regressions ------------------------------------------------------------------
for(unit in c("woreda")){ # "woreda", "clusters_of_ntl"
  for(dv in c("globcover_urban_sum_ihs", "dmspols_zhang_sum2_ihs", "dmspols_zhang_sum6_ihs", "dmspols_zhang_ihs", "dmspols_zhang_sum_ihs")){
    for(log in c("_log")){
      for(theta in c(1,8)){ # 2
        #for(exclude in c("", "_exclude20km", "_exclude50km", "_exclude100km")){ # "_exclude100km"
        #for(MA_ubanrural in c("")){ # "_urban2", "_rural2"
        #  for(subset_urbanrural in c("")){ # "_urban2", "rural2"
        
        dv_name <- ""
        if(dv %in% "globcover_urban_sum") dv_name <- "N Urban Pixels"
        if(dv %in% "globcover_urban_sum_ihs") dv_name <- "N Urban Pixels, Logged"
        
        if(dv %in% "dmspols_zhang_sum2")  dv_name <- "N Pixels NTL$>$2"
        if(dv %in% "dmspols_zhang_sum2_ihs")  dv_name <- "N Pixels NTL$>$2, Logged"
        
        if(dv %in% "dmspols_zhang_sum6")  dv_name <- "N Pixels NTL$>$6"
        if(dv %in% "dmspols_zhang_sum6_ihs")  dv_name <- "N Pixels NTL$>$6, Logged"
        
        if(dv %in% "dmspols_zhang_ihs")   dv_name <- "Average NTL, logged"
        if(dv %in% "dmspols_zhang_sum_ihs")   dv_name <- "Sum NTL, logged"
        
        ## Load Data
        if(grepl("globcover", dv)){
          data <- readRDS(file.path(panel_rsdp_imp_data_file_path, unit, "merged_datasets",
                                    "longdiff_data_clean_base1996_end2016.Rds"))
        } else{
          data <- readRDS(file.path(panel_rsdp_imp_data_file_path, unit, "merged_datasets",
                                    "longdiff_data_clean_base1996_end2012.Rds"))
        }
        
        ## DV and othr vars
        data$DV <- data[[dv]]
        data$distance_city_addisababa <- data$distance_city_addisababa / 1000 / 100
        
        ## MA Variable
        data_MAall <- data
        data_MAexc <- data
        
        data_MAall$MA_var      <- data_MAall[[paste0("MA_pop2000_tt_theta",theta, log)]]
        data_MAall$MA_var_1996 <- data_MAall[[paste0("MA_pop2000_tt_theta",theta, log, "_1996")]]
        
        data_MAexc$MA_var      <- data_MAexc[[paste0("MA_pop2000_tt_theta",theta,"_exclude50km", log)]]
        data_MAexc$MA_var_1996 <- data_MAexc[[paste0("MA_pop2000_tt_theta",theta,"_exclude50km", log, "_1996")]]
        
        ## Regressions
        lm1   <- felm(DV ~ MA_var                                                     + dmspols_ihs_1996 + dmspols_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 + factor(Z_CODE)   | 0 | 0 | Z_CODE, data = data_MAall)
        lm2   <- felm(DV ~ MA_var*distance_city_addisababa - distance_city_addisababa + dmspols_ihs_1996 + dmspols_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 + factor(Z_CODE)   | 0 | 0 | Z_CODE, data = data_MAall)
        lm3   <- felm(DV ~ MA_var*dmspols_ihs_1996         - dmspols_ihs_1996         + dmspols_ihs_1996 + dmspols_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 + factor(Z_CODE)   | 0 | 0 | Z_CODE, data = data_MAall)
        lm4   <- felm(DV ~ MA_var*globcover_urban_1996     - globcover_urban_1996     + dmspols_ihs_1996 + dmspols_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 + factor(Z_CODE)   | 0 | 0 | Z_CODE, data = data_MAall)
        lm5   <- felm(DV ~ MA_var*globcover_cropland_1996  - globcover_cropland_1996  + dmspols_ihs_1996 + dmspols_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 + factor(Z_CODE)   | 0 | 0 | Z_CODE, data = data_MAall)
        
        lm6   <- felm(DV ~ MA_var                                                     + dmspols_ihs_1996 + dmspols_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 + factor(Z_CODE)   | 0 | 0 | Z_CODE, data = data_MAexc)
        lm7   <- felm(DV ~ MA_var*distance_city_addisababa - distance_city_addisababa + dmspols_ihs_1996 + dmspols_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 + factor(Z_CODE)   | 0 | 0 | Z_CODE, data = data_MAexc)
        lm8   <- felm(DV ~ MA_var*dmspols_ihs_1996         - dmspols_ihs_1996         + dmspols_ihs_1996 + dmspols_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 + factor(Z_CODE)   | 0 | 0 | Z_CODE, data = data_MAexc)
        lm9   <- felm(DV ~ MA_var*globcover_urban_1996     - globcover_urban_1996     + dmspols_ihs_1996 + dmspols_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 + factor(Z_CODE)   | 0 | 0 | Z_CODE, data = data_MAexc)
        lm10  <- felm(DV ~ MA_var*globcover_cropland_1996  - globcover_cropland_1996  + dmspols_ihs_1996 + dmspols_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 + factor(Z_CODE)   | 0 | 0 | Z_CODE, data = data_MAexc)
        
        stargazer(lm1,
                  lm2,
                  lm3,
                  lm4,
                  lm5,
                  lm6,
                  lm7,
                  lm8,
                  lm9,
                  lm10,
                  dep.var.labels.include = T,
                  dep.var.labels   = c(dv_name),
                  omit = c("Z_CODE", "Constant"),
                  #keep=c("MA_var", "MA_var_1996", "dmspols_ihs_1996", "dmspols_ihs_pretnd96_92", "globcover_urban_sum_pretnd96_92.y"),
                  covariate.labels = c("MA", 
                                       "Log mean light, 1996",
                                       "Pre-trend: log mean light",
                                       "Pre-trend: log N urban pixels",
                                       "MA$\\times$Dist Addis (100km)",
                                       "MA$\\times$Log mean light, 1996",
                                       "MA$\\times$Prop. Urban, 1996",
                                       "MA$\\times$Prop. Cropland, 1996"),
                  
                  #covariate.labels = c("log(MA); $\\theta=1$",
                  #                     "log(MA); $\\theta=8$"),
                  dep.var.caption = "",
                  omit.stat = c("f","ser"), 
                  align=TRUE,
                  no.space=TRUE,
                  float=FALSE,
                  column.sep.width = "8pt",
                  digits = 2,
                  add.lines = list(
                    c("MA Exclude", rep("N/A", 5), rep("50km", 5)),
                    c("Zone FEs", rep("Y", 10))
                  ),
                  out=file.path(paper_tables,
                                paste0("MA_table_longdiff_theta",theta,log,"_",unit,"_",dv,".tex")))
      }
    }
  }
}
#}
#  }
#}



