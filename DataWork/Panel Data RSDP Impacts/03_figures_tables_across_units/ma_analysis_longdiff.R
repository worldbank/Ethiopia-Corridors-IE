# Market Access Analysis

unit  <- "woreda"
log = "_log"
theta = "3_8"
exclude = "_exclude50km"
MA_ubanrural <- ""

# Regressions ------------------------------------------------------------------
for(unit in c("woreda")){ # "woreda", "clusters_of_ntl"
  for(log in c("_log")){
    for(theta in c("1", "2", "3_8", "5", "8")){ # 
      for(exclude in c("_exclude20km", "_exclude50km", "_exclude100km")){ # "_exclude20km", "_exclude50km", "_exclude100km"
        for(MA_ubanrural in c("")){ # "_urban2", "_rural2"
          
          # dv_name <- ""
          # if(dv %in% "globcover_urban_sum") dv_name <- "N Urban Pixels"
          # if(dv %in% "globcover_urban_sum_ihs") dv_name <- "N Urban Pixels, IHS Transformation"
          # 
          # if(dv %in% "dmspols_zhang_sum2")  dv_name <- "N Pixels NTL$>$2"
          # if(dv %in% "dmspols_zhang_sum2_ihs")  dv_name <- "N Pixels NTL$>$2, IHS Transformation"
          # 
          # if(dv %in% "dmspols_zhang_sum6")  dv_name <- "N Pixels NTL$>$6"
          # if(dv %in% "dmspols_zhang_sum6_ihs")  dv_name <- "N Pixels NTL$>$6, IHS Transformation"
          # 
          # if(dv %in% "dmspols_zhang_ihs")   dv_name <- "Average NTL, IHS Transformation"
          # if(dv %in% "dmspols_zhang_sum_ihs")   dv_name <- "Sum NTL, IHS Transformation"
          # 
          ## Load Data
          data96 <- readRDS(file.path(panel_rsdp_imp_data_file_path, unit, "merged_datasets",
                                      "longdiff_data_clean_base1996_end2016.Rds"))
          data92 <- readRDS(file.path(panel_rsdp_imp_data_file_path, unit, "merged_datasets",
                                      "longdiff_data_clean_base1996_end2012.Rds"))
          
          prep_data <- function(data, theta, MA_ubanrural, log, unit){
            
            ## DV and othr vars
            data$distance_city_addisababa <- data$distance_city_addisababa / 1000 / 100
            
            ## MA Variable
            data$MA_var      <- data[[paste0("MA_pop2000_tt_theta",theta, MA_ubanrural, log)]]
            data$MA_var_1996 <- data[[paste0("MA_pop2000_tt_theta",theta, MA_ubanrural, log, "_1996")]]
            
            data$MA_var_exc      <- data[[paste0("MA_pop2000_tt_theta",theta, exclude, MA_ubanrural, log)]]
            data$MA_var_exc_1996 <- data[[paste0("MA_pop2000_tt_theta",theta, exclude, MA_ubanrural, log, "_1996")]]
            
            ## Interactions
            data$MA_varXdistance_city_addisababa <- data$MA_var * data$distance_city_addisababa
            #data$MA_varXdmspols_ihs_1996         <- data$MA_var * data$dmspols_ihs_1996
            data$MA_varXglobcover_urban_1996     <- data$MA_var * data$globcover_urban_1996
            #data$MA_varXglobcover_cropland_1996  <- data$MA_var * data$globcover_cropland_1996
            #data$MA_varXdmspols_2bin_1996        <- data$MA_var * data$dmspols_2bin_1996
            #data$MA_varXdmspols_6bin_1996        <- data$MA_var * data$dmspols_6bin_1996
            data$MA_varXdmspols_zhang_sum2_ihs_1996 <- data$MA_var * data$dmspols_zhang_sum2_ihs_1996
            data$MA_varXdmspols_zhang_ihs_1996 <- data$MA_var * data$dmspols_zhang_ihs_1996
            # data$MA_varXdmspols_1996_bin3_1 <- data$MA_var * data$dmspols_1996_bin3_1
            # data$MA_varXdmspols_1996_bin3_2 <- data$MA_var * data$dmspols_1996_bin3_2
            # data$MA_varXdmspols_1996_bin3_3 <- data$MA_var * data$dmspols_1996_bin3_3
            data$MA_varXdmspols_1996_bin4_1 <- data$MA_var * data$dmspols_1996_bin4_1
            data$MA_varXdmspols_1996_bin4_2 <- data$MA_var * data$dmspols_1996_bin4_2
            data$MA_varXdmspols_1996_bin4_3 <- data$MA_var * data$dmspols_1996_bin4_3
            data$MA_varXdmspols_1996_bin4_4 <- data$MA_var * data$dmspols_1996_bin4_4
            
            data$MA_var_excXdistance_city_addisababa <- data$MA_var_exc * data$distance_city_addisababa
            #data$MA_var_excXdmspols_ihs_1996         <- data$MA_var_exc * data$dmspols_ihs_1996
            data$MA_var_excXglobcover_urban_1996     <- data$MA_var_exc * data$globcover_urban_1996
            #data$MA_var_excXglobcover_cropland_1996  <- data$MA_var_exc * data$globcover_cropland_1996
            #data$MA_var_excXdmspols_2bin_1996        <- data$MA_var_exc * data$dmspols_2bin_1996
            #data$MA_var_excXdmspols_6bin_1996        <- data$MA_var_exc * data$dmspols_6bin_1996
            data$MA_var_excXdmspols_zhang_sum2_ihs_1996 <- data$MA_var_exc * data$dmspols_zhang_sum2_ihs_1996
            data$MA_var_excXdmspols_zhang_ihs_1996 <- data$MA_var_exc * data$dmspols_zhang_ihs_1996
            # data$MA_var_excXdmspols_1996_bin3_1 <- data$MA_var_exc * data$dmspols_1996_bin3_1
            # data$MA_var_excXdmspols_1996_bin3_2 <- data$MA_var_exc * data$dmspols_1996_bin3_2
            # data$MA_var_excXdmspols_1996_bin3_3 <- data$MA_var_exc * data$dmspols_1996_bin3_3
            data$MA_var_excXdmspols_1996_bin4_1 <- data$MA_var_exc * data$dmspols_1996_bin4_1
            data$MA_var_excXdmspols_1996_bin4_2 <- data$MA_var_exc * data$dmspols_1996_bin4_2
            data$MA_var_excXdmspols_1996_bin4_3 <- data$MA_var_exc * data$dmspols_1996_bin4_3
            data$MA_var_excXdmspols_1996_bin4_4 <- data$MA_var_exc * data$dmspols_1996_bin4_4
            
            return(data)
          }
          
          data92 <- prep_data(data92, theta, MA_ubanrural, log)
          data96 <- prep_data(data96, theta, MA_ubanrural, log)
          
          ## OLS
          ols1   <- felm(dmspols_harmon_ihs ~ MA_var                                                                                             + MA_var_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | Z_CODE | 0 | Z_CODE, data = data96)
          ols2   <- felm(dmspols_harmon_ihs ~ MA_var + MA_varXdmspols_1996_bin4_2 + MA_varXdmspols_1996_bin4_3 + MA_varXdmspols_1996_bin4_4      + MA_var_1996 +  dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | Z_CODE | 0 | Z_CODE, data = data96) 
          ols3   <- felm(dmspols_harmon_ihs ~ MA_var + MA_varXdistance_city_addisababa                                                           + MA_var_1996 +  dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | Z_CODE | 0 | Z_CODE, data = data96) 
        
          #ols7   <- felm(dmspols_harmon_sum6_ihs ~ MA_var                                                                                         + MA_var_1996 +  dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | Z_CODE | 0 | Z_CODE, data = data96)
          #ols8   <- felm(dmspols_harmon_sum6_ihs ~ MA_var + MA_varXdmspols_1996_bin4_2 + MA_varXdmspols_1996_bin4_3 + MA_varXdmspols_1996_bin4_4  + MA_var_1996 +  dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | Z_CODE | 0 | Z_CODE, data = data96) 
          #ols9  <- felm(dmspols_harmon_sum6_ihs ~ MA_var + MA_varXdistance_city_addisababa                                                        + MA_var_1996 +  dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | Z_CODE | 0 | Z_CODE, data = data96) 
          
          ols4  <- felm(globcover_urban_sum_ihs ~ MA_var                                                                                         + MA_var_1996 +  dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | Z_CODE | 0 | Z_CODE, data = data96)
          ols5   <- felm(globcover_urban_sum_ihs ~ MA_var + MA_varXdmspols_1996_bin4_2 + MA_varXdmspols_1996_bin4_3 + MA_varXdmspols_1996_bin4_4 + MA_var_1996 +  dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | Z_CODE | 0 | Z_CODE, data = data96) 
          ols6  <- felm(globcover_urban_sum_ihs ~ MA_var + MA_varXdistance_city_addisababa                                                       + MA_var_1996 +  dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | Z_CODE | 0 | Z_CODE, data = data96) 
        
          ols7   <- felm(globcover_cropland_sum_ihs ~ MA_var                                                                                        + MA_var_1996 +  dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | Z_CODE | 0 | Z_CODE, data = data96)
          ols8   <- felm(globcover_cropland_sum_ihs ~ MA_var + MA_varXdmspols_1996_bin4_2 + MA_varXdmspols_1996_bin4_3 + MA_varXdmspols_1996_bin4_4 + MA_var_1996 +  dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | Z_CODE | 0 | Z_CODE, data = data96) 
          ols9   <- felm(globcover_cropland_sum_ihs ~ MA_var + MA_varXdistance_city_addisababa                                                      + MA_var_1996 +  dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92   | Z_CODE | 0 | Z_CODE, data = data96) 
          
          ## IV
          iv1  <- felm(dmspols_harmon_ihs ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | Z_CODE | (MA_var ~ MA_var_exc)                                                                             | Z_CODE, data = data96)
          iv2  <- felm(dmspols_harmon_ihs ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | Z_CODE | (MA_var|MA_varXdmspols_1996_bin4_2|MA_varXdmspols_1996_bin4_3|MA_varXdmspols_1996_bin4_4      ~ MA_var_exc + MA_var_excXdmspols_1996_bin4_2 + MA_var_excXdmspols_1996_bin4_3 + MA_var_excXdmspols_1996_bin4_4)      | Z_CODE, data = data96)
          iv3  <- felm(dmspols_harmon_ihs ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | Z_CODE | (MA_var|MA_varXdistance_city_addisababa    ~ MA_var_exc + MA_var_excXdistance_city_addisababa)    | Z_CODE, data = data96) 
          
          #iv4  <- felm(dmspols_harmon_sum2_ihs ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | Z_CODE | (MA_var ~ MA_var_exc)                                                                             | Z_CODE, data = data96)
          #iv5  <- felm(dmspols_harmon_sum2_ihs ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | Z_CODE | (MA_var|MA_varXdmspols_1996_bin4_2|MA_varXdmspols_1996_bin4_3|MA_varXdmspols_1996_bin4_4      ~ MA_var_exc + MA_var_excXdmspols_1996_bin4_2 + MA_var_excXdmspols_1996_bin4_3 + MA_var_excXdmspols_1996_bin4_4)      | Z_CODE, data = data96)
          #iv6  <- felm(dmspols_harmon_sum2_ihs ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | Z_CODE | (MA_var|MA_varXdistance_city_addisababa    ~ MA_var_exc + MA_var_excXdistance_city_addisababa)    | Z_CODE, data = data96) 
          
          #iv7  <- felm(dmspols_harmon_sum6_ihs ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | Z_CODE | (MA_var ~ MA_var_exc)                                                                             | Z_CODE, data = data96)
          #iv8  <- felm(dmspols_harmon_sum6_ihs ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | Z_CODE | (MA_var|MA_varXdmspols_1996_bin4_2|MA_varXdmspols_1996_bin4_3|MA_varXdmspols_1996_bin4_4      ~ MA_var_exc + MA_var_excXdmspols_1996_bin4_2 + MA_var_excXdmspols_1996_bin4_3 + MA_var_excXdmspols_1996_bin4_4)      | Z_CODE, data = data96)
          #iv9 <- felm(dmspols_harmon_sum6_ihs ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | Z_CODE | (MA_var|MA_varXdistance_city_addisababa    ~ MA_var_exc + MA_var_excXdistance_city_addisababa)    | Z_CODE, data = data96) 
          
          iv4 <- felm(globcover_urban_sum_ihs ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | Z_CODE | (MA_var ~ MA_var_exc)                                                                             | Z_CODE, data = data96)
          iv5  <- felm(globcover_urban_sum_ihs ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | Z_CODE | (MA_var|MA_varXdmspols_1996_bin4_2|MA_varXdmspols_1996_bin4_3|MA_varXdmspols_1996_bin4_4      ~ MA_var_exc + MA_var_excXdmspols_1996_bin4_2 + MA_var_excXdmspols_1996_bin4_3 + MA_var_excXdmspols_1996_bin4_4)      | Z_CODE, data = data96)
          iv6 <- felm(globcover_urban_sum_ihs ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | Z_CODE | (MA_var|MA_varXdistance_city_addisababa    ~ MA_var_exc + MA_var_excXdistance_city_addisababa)    | Z_CODE, data = data96) 
          
          iv7 <- felm(globcover_cropland_sum_ihs ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | Z_CODE | (MA_var ~ MA_var_exc)                                                                             | Z_CODE, data = data96)
          iv8  <- felm(globcover_cropland_sum_ihs ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | Z_CODE | (MA_var|MA_varXdmspols_1996_bin4_2|MA_varXdmspols_1996_bin4_3|MA_varXdmspols_1996_bin4_4      ~ MA_var_exc + MA_var_excXdmspols_1996_bin4_2 + MA_var_excXdmspols_1996_bin4_3 + MA_var_excXdmspols_1996_bin4_4)      | Z_CODE, data = data96)
          iv9 <- felm(globcover_cropland_sum_ihs ~ MA_var_exc_1996 + dmspols_harmon_ihs_1996 + dmspols_harmon_ihs_pretnd96_92 + globcover_urban_sum_ihs_pretnd96_92 | Z_CODE | (MA_var|MA_varXdistance_city_addisababa    ~ MA_var_exc + MA_var_excXdistance_city_addisababa)    | Z_CODE, data = data96) 
          
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
                                  paste0("MA",MA_ubanrural,"_table_longdiff_theta",theta,log,"_",unit,"_ols.tex")))
          
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
                                  paste0("MA",MA_ubanrural,"_table_longdiff_theta",theta,exclude,log,"_",unit,"_iv.tex")))
        }
      }
    }
  }
}


