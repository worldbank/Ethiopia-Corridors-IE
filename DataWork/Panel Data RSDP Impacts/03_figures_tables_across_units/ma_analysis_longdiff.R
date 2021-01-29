# Market Access Analysis

unit  <- "woreda"
dv = "dmspols_zhang_ihs"
log = "_log"
theta = "3_8"

remove_fit_iv <- function(lm){
  rownames(lm$coefficients) <- rownames(lm$coefficients) %>% str_replace_all("\\(fit\\)|\\`", "")
  rownames(lm$beta) <- rownames(lm$beta) %>% str_replace_all("\\(fit\\)|\\`", "")
  return(lm)
}

# Regressions ------------------------------------------------------------------
for(unit in c("woreda")){ # "woreda", "clusters_of_ntl"
  for(dv in c("globcover_urban_sum_ihs", "dmspols_zhang_sum2_ihs", "dmspols_zhang_sum6_ihs", "dmspols_zhang_ihs", "dmspols_zhang_sum_ihs")){
    for(log in c("_log")){
      for(theta in c("1", "3_8", "8")){ # 2, 5
        #for(exclude in c("", "_exclude20km", "_exclude50km", "_exclude100km")){ # "_exclude100km"
        #for(MA_ubanrural in c("")){ # "_urban2", "_rural2"
        #  for(subset_urbanrural in c("")){ # "_urban2", "rural2"
        
        dv_name <- ""
        if(dv %in% "globcover_urban_sum") dv_name <- "N Urban Pixels"
        if(dv %in% "globcover_urban_sum_ihs") dv_name <- "N Urban Pixels, IHS Transformation"
        
        if(dv %in% "dmspols_zhang_sum2")  dv_name <- "N Pixels NTL$>$2"
        if(dv %in% "dmspols_zhang_sum2_ihs")  dv_name <- "N Pixels NTL$>$2, IHS Transformation"
        
        if(dv %in% "dmspols_zhang_sum6")  dv_name <- "N Pixels NTL$>$6"
        if(dv %in% "dmspols_zhang_sum6_ihs")  dv_name <- "N Pixels NTL$>$6, IHS Transformation"
        
        if(dv %in% "dmspols_zhang_ihs")   dv_name <- "Average NTL, IHS Transformation"
        if(dv %in% "dmspols_zhang_sum_ihs")   dv_name <- "Sum NTL, IHS Transformation"
        
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
        
        data_MAall$MA_var_exc      <- data_MAall[[paste0("MA_pop2000_tt_theta",theta, "_exclude50km", log)]]
        data_MAall$MA_var_exc_1996 <- data_MAall[[paste0("MA_pop2000_tt_theta",theta, "_exclude50km", log, "_1996")]]
        
        data_MAexc$MA_var      <- data_MAexc[[paste0("MA_pop2000_tt_theta",theta,"_exclude50km", log)]]
        data_MAexc$MA_var_1996 <- data_MAexc[[paste0("MA_pop2000_tt_theta",theta,"_exclude50km", log, "_1996")]]
        
        ## Interactions
        data_MAall$MA_varXdistance_city_addisababa     <- data_MAall$MA_var     * data_MAall$distance_city_addisababa
        data_MAall$MA_var_excXdistance_city_addisababa <- data_MAall$MA_var_exc * data_MAall$distance_city_addisababa
        
        data_MAall$MA_varXdmspols_ihs_1996     <- data_MAall$MA_var     * data_MAall$dmspols_ihs_1996
        data_MAall$MA_var_excXdmspols_ihs_1996 <- data_MAall$MA_var_exc * data_MAall$dmspols_ihs_1996
        
        data_MAall$MA_varXglobcover_urban_1996     <- data_MAall$MA_var     * data_MAall$globcover_urban_1996
        data_MAall$MA_var_excXglobcover_urban_1996 <- data_MAall$MA_var_exc * data_MAall$globcover_urban_1996
        
        data_MAall$MA_varXglobcover_cropland_1996     <- data_MAall$MA_var     * data_MAall$globcover_cropland_1996
        data_MAall$MA_var_excXglobcover_cropland_1996 <- data_MAall$MA_var_exc * data_MAall$globcover_cropland_1996
        
        ## OLS
        lm1   <- felm(DV ~ MA_var                                   + dmspols_ihs_1996 + dmspols_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | Z_CODE | 0 | Z_CODE, data = data_MAall)
        lm2   <- felm(DV ~ MA_var + MA_varXdistance_city_addisababa + dmspols_ihs_1996 + dmspols_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | Z_CODE | 0 | Z_CODE, data = data_MAall)
        lm3   <- felm(DV ~ MA_var + MA_varXdmspols_ihs_1996         + dmspols_ihs_1996 + dmspols_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | Z_CODE | 0 | Z_CODE, data = data_MAall)
        lm4   <- felm(DV ~ MA_var + MA_varXglobcover_urban_1996     + dmspols_ihs_1996 + dmspols_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | Z_CODE | 0 | Z_CODE, data = data_MAall)
        lm5   <- felm(DV ~ MA_var + MA_varXglobcover_cropland_1996  + dmspols_ihs_1996 + dmspols_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | Z_CODE | 0 | Z_CODE, data = data_MAall)
        
        ## OLS - Exclude
        lm6   <- felm(DV ~ MA_var                                                     + dmspols_ihs_1996 + dmspols_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | Z_CODE | 0 | Z_CODE, data = data_MAexc)
        lm7   <- felm(DV ~ MA_var*distance_city_addisababa - distance_city_addisababa + dmspols_ihs_1996 + dmspols_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | Z_CODE | 0 | Z_CODE, data = data_MAexc)
        lm8   <- felm(DV ~ MA_var*dmspols_ihs_1996         - dmspols_ihs_1996         + dmspols_ihs_1996 + dmspols_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | Z_CODE | 0 | Z_CODE, data = data_MAexc)
        lm9   <- felm(DV ~ MA_var*globcover_urban_1996     - globcover_urban_1996     + dmspols_ihs_1996 + dmspols_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | Z_CODE | 0 | Z_CODE, data = data_MAexc)
        lm10  <- felm(DV ~ MA_var*globcover_cropland_1996  - globcover_cropland_1996  + dmspols_ihs_1996 + dmspols_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | Z_CODE | 0 | Z_CODE, data = data_MAexc)
        
        ## IV
        lm6iv   <- felm(DV ~ dmspols_ihs_1996 + dmspols_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | Z_CODE | (MA_var ~ MA_var_exc) | Z_CODE, data = data_MAall) %>% remove_fit_iv()
        lm7iv   <- felm(DV ~ dmspols_ihs_1996 + dmspols_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | Z_CODE | (MA_var|MA_varXdistance_city_addisababa ~ MA_var_exc + MA_var_excXdistance_city_addisababa) | Z_CODE, data = data_MAall) %>% remove_fit_iv()
        lm8iv   <- felm(DV ~ dmspols_ihs_1996 + dmspols_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | Z_CODE | (MA_var|MA_varXdmspols_ihs_1996         ~ MA_var_exc + MA_var_excXdmspols_ihs_1996) | Z_CODE, data = data_MAall) %>% remove_fit_iv()
        lm9iv   <- felm(DV ~ dmspols_ihs_1996 + dmspols_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | Z_CODE | (MA_var|MA_varXglobcover_urban_1996     ~ MA_var_exc + MA_var_excXglobcover_urban_1996) | Z_CODE, data = data_MAall) %>% remove_fit_iv()
        lm10iv  <- felm(DV ~ dmspols_ihs_1996 + dmspols_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | Z_CODE | (MA_var|MA_varXglobcover_cropland_1996  ~ MA_var_exc + MA_var_excXglobcover_cropland_1996) | Z_CODE, data = data_MAall) %>% remove_fit_iv()
        
        stargazer(lm1,
                  lm2,
                  lm3,
                  lm4,
                  lm5,
                  lm6iv,
                  lm7iv,
                  lm8iv,
                  lm9iv,
                  lm10iv,
                  dep.var.labels.include = T,
                  dep.var.labels   = c(dv_name),
                  omit = c("Z_CODE", "Constant"),
                  #keep=c("MA_var", "MA_var_1996", "dmspols_ihs_1996", "dmspols_ihs_pretnd96_92", "globcover_urban_sum_pretnd96_92.y"),
                  covariate.labels = c("MA",
                                       "MA$\\times$Dist Addis (100km)",
                                       "MA$\\times$Log mean light, 1996",
                                       "MA$\\times$Prop. Urban, 1996",
                                       "MA$\\times$Prop. Cropland, 1996",
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
                  add.lines = list(
                    #c("Zone FEs", rep("Y", 10)),
                    c("MA IV, 50km Doughnut", rep("N", 5), rep("Y", 5))
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



