# Market Access Analysis

unit <- "woreda"
theta <- "3_8"
log <- "_log"
exclude <- "_exclude50km"
dv <- "dmspols_harmon_ihs"
MA_ubanrural <- ""

# Regressions ------------------------------------------------------------------
for(unit in c("woreda")){ # "woreda ,"clusters_of_ntl", "clusters_of_globcover_urban"
  for(theta in c("1", "2", "3_8", "5", "8")){ # "1","2","5","8"
    for(log in c("_log")){
      for(exclude in c("", "_exclude20km", "_exclude50km", "_exclude100km")){ # "_exclude20km", "_exclude100km","_exclude100km"
        for(MA_ubanrural in c("")){ # "_urban2", "_rural2"
          
          ## Load/Subset Data
          data <- readRDS(file.path(panel_rsdp_imp_data_file_path, unit, "merged_datasets", "panel_data_clean.Rds"))
          
          data <- data %>%
            filter(year >= 1996)
          
          data$dmspols_1996_sq <- data$dmspols_1996*2
          
          ## Prep Vars
          data$MA_var     <- data[[paste0("MA_pop2000_tt_theta",theta,MA_ubanrural, log)]]
          data$MA_var_exc <- data[[paste0("MA_pop2000_tt_theta",theta,exclude,MA_ubanrural, log)]]
          
          #if(unit == "clusters_of_ntl") data$MA_var     <- data[[paste0("MA_ntl2000_tt_theta",theta,MA_ubanrural, log)]]
        #  if(unit == "clusters_of_ntl") data$MA_var_exc <- data[[paste0("MA_ntl2000_tt_theta",theta,exclude,MA_ubanrural, log)]]
          
         # if(unit == "clusters_of_globcover_urban") data$MA_var     <- data[[paste0("MA_gcu2000_tt_theta",theta,MA_ubanrural, log)]]
        #  if(unit == "clusters_of_globcover_urban") data$MA_var_exc <- data[[paste0("MA_gcu2000_tt_theta",theta,exclude,MA_ubanrural, log)]]
          data$dv         <- data[[dv]]
          
          data$distance_city_addisababa <- data$distance_city_addisababa / 1000 / 100
          data$globcover_urban_1996 <- as.numeric(data$globcover_urban_1996 > 0)
          
          ## Prep Interaction Vars
          data$MA_varXdmspols_zhang_ihs_1996   <- data$MA_var * data$dmspols_zhang_ihs_1996
          #data$MA_varXdmspols_2bin_1996        <- data$MA_var * data$dmspols_2bin_1996
          #data$MA_varXdmspols_6bin_1996        <- data$MA_var * data$dmspols_6bin_1996
          data$MA_varXglobcover_urban_1996     <- data$MA_var * data$globcover_urban_1996
          data$MA_varXdistance_city_addisababa <- data$MA_var * data$distance_city_addisababa
          data$MA_varXglobcover_urban_sum_ihs_1996 <- data$MA_var * data$globcover_urban_sum_ihs_1996
          data$MA_varXdmspols_zhang_sum2_ihs_1996 <- data$MA_var * data$dmspols_zhang_sum2_ihs_1996
          data$MA_varXdmspols_zhang_sum6_ihs_1996 <- data$MA_var * data$dmspols_zhang_sum6_ihs_1996
          data$MA_varXdmspols_zhang_ihs_1996 <- data$MA_var * data$dmspols_zhang_ihs_1996
          data$MA_varXdmspols_1996_bin3_1 <- data$MA_var * data$dmspols_1996_bin3_1
          data$MA_varXdmspols_1996_bin3_2 <- data$MA_var * data$dmspols_1996_bin3_2
          data$MA_varXdmspols_1996_bin3_3 <- data$MA_var * data$dmspols_1996_bin3_3
          data$MA_varXdmspols_1996_bin4_1 <- data$MA_var * data$dmspols_1996_bin4_1
          data$MA_varXdmspols_1996_bin4_2 <- data$MA_var * data$dmspols_1996_bin4_2
          data$MA_varXdmspols_1996_bin4_3 <- data$MA_var * data$dmspols_1996_bin4_3
          data$MA_varXdmspols_1996_bin4_4 <- data$MA_var * data$dmspols_1996_bin4_4
          #data$MA_varXdmspols_1996_bin42_1 <- data$MA_var * data$dmspols_1996_bin42_1
          #data$MA_varXdmspols_1996_bin42_2 <- data$MA_var * data$dmspols_1996_bin42_2
          #data$MA_varXdmspols_1996_bin42_3 <- data$MA_var * data$dmspols_1996_bin42_3
          #data$MA_varXdmspols_1996_bin42_4 <- data$MA_var * data$dmspols_1996_bin42_4
          data$MA_varXdmspols_1996 <- data$MA_var * data$dmspols_1996
          data$MA_varXdmspols_1996_sq <- data$MA_var * data$dmspols_1996_sq
          
          data$MA_var_excXdmspols_zhang_ihs_1996   <- data$MA_var_exc * data$dmspols_zhang_ihs_1996
          #data$MA_var_excXdmspols_2bin_1996        <- data$MA_var_exc * data$dmspols_2bin_1996
          #data$MA_var_excXdmspols_6bin_1996        <- data$MA_var_exc * data$dmspols_6bin_1996
          data$MA_var_excXglobcover_urban_1996     <- data$MA_var_exc * data$globcover_urban_1996
          data$MA_var_excXdistance_city_addisababa <- data$MA_var_exc * data$distance_city_addisababa
          data$MA_var_excXglobcover_urban_sum_ihs_1996 <- data$MA_var_exc * data$globcover_urban_sum_ihs_1996
          data$MA_var_excXdmspols_zhang_sum2_ihs_1996  <- data$MA_var_exc * data$dmspols_zhang_sum2_ihs_1996
          data$MA_var_excXdmspols_zhang_sum6_ihs_1996  <- data$MA_var_exc * data$dmspols_zhang_sum6_ihs_1996
          data$MA_var_excXdmspols_zhang_ihs_1996       <- data$MA_var_exc * data$dmspols_zhang_ihs_1996
          data$MA_var_excXdmspols_1996_bin3_1 <- data$MA_var_exc * data$dmspols_1996_bin3_1
          data$MA_var_excXdmspols_1996_bin3_2 <- data$MA_var_exc * data$dmspols_1996_bin3_2
          data$MA_var_excXdmspols_1996_bin3_3 <- data$MA_var_exc * data$dmspols_1996_bin3_3
          data$MA_var_excXdmspols_1996_bin4_1 <- data$MA_var_exc * data$dmspols_1996_bin4_1
          data$MA_var_excXdmspols_1996_bin4_2 <- data$MA_var_exc * data$dmspols_1996_bin4_2
          data$MA_var_excXdmspols_1996_bin4_3 <- data$MA_var_exc * data$dmspols_1996_bin4_3
          data$MA_var_excXdmspols_1996_bin4_4 <- data$MA_var_exc * data$dmspols_1996_bin4_4
          #data$MA_var_excXdmspols_1996_bin42_1 <- data$MA_var_exc * data$dmspols_1996_bin42_1
          #data$MA_var_excXdmspols_1996_bin42_2 <- data$MA_var_exc * data$dmspols_1996_bin42_2
          #data$MA_var_excXdmspols_1996_bin42_3 <- data$MA_var_exc * data$dmspols_1996_bin42_3
          #data$MA_var_excXdmspols_1996_bin42_4 <- data$MA_var_exc * data$dmspols_1996_bin42_4
          data$MA_var_excXdmspols_1996 <- data$MA_var_exc * data$dmspols_1996
          data$MA_var_excXdmspols_1996_sq <- data$MA_var_exc * data$dmspols_1996_sq

          
          ## OLS Regressions
          ols1  <- felm(dmspols_harmon_ihs ~ MA_var                                       + temp_avg + precipitation | year + cell_id | 0                     | Z_CODE, data = data)
          ols2  <- felm(dmspols_harmon_ihs ~ MA_var + MA_varXdmspols_1996_bin4_2 + MA_varXdmspols_1996_bin4_3 + MA_varXdmspols_1996_bin4_4 + temp_avg + precipitation | year + cell_id | 0  | Z_CODE, data = data)
          ols3  <- felm(dmspols_harmon_ihs ~ MA_var + MA_varXdistance_city_addisababa + temp_avg + precipitation | year + cell_id | 0                     | Z_CODE, data = data) 
          
          ols4  <- felm(dmspols_harmon_sum2_ihs ~ MA_var                                       + temp_avg + precipitation | year + cell_id | 0                     | Z_CODE, data = data)
          ols5  <- felm(dmspols_harmon_sum2_ihs ~ MA_var + MA_varXdmspols_1996_bin4_2 + MA_varXdmspols_1996_bin4_3 + MA_varXdmspols_1996_bin4_4 + temp_avg + precipitation | year + cell_id | 0  | Z_CODE, data = data)
          ols6  <- felm(dmspols_harmon_sum2_ihs ~ MA_var + MA_varXdistance_city_addisababa + temp_avg + precipitation | year + cell_id | 0                     | Z_CODE, data = data) 
          
          ols7  <- felm(dmspols_harmon_sum6_ihs ~ MA_var                                       + temp_avg + precipitation | year + cell_id | 0                     | Z_CODE, data = data)
          ols8  <- felm(dmspols_harmon_sum6_ihs ~ MA_var + MA_varXdmspols_1996_bin4_2 + MA_varXdmspols_1996_bin4_3 + MA_varXdmspols_1996_bin4_4 + temp_avg + precipitation | year + cell_id | 0  | Z_CODE, data = data)
          ols9 <- felm(dmspols_harmon_sum6_ihs ~ MA_var + MA_varXdistance_city_addisababa + temp_avg + precipitation | year + cell_id | 0                     | Z_CODE, data = data) 
          
          ols10 <- felm(globcover_urban_sum_ihs ~ MA_var                                       + temp_avg + precipitation | year + cell_id | 0                     | Z_CODE, data = data)
          ols11  <- felm(globcover_urban_sum_ihs ~ MA_var + MA_varXdmspols_1996_bin4_2 + MA_varXdmspols_1996_bin4_3 + MA_varXdmspols_1996_bin4_4 + temp_avg + precipitation | year + cell_id | 0  | Z_CODE, data = data)
          ols12 <- felm(globcover_urban_sum_ihs ~ MA_var + MA_varXdistance_city_addisababa + temp_avg + precipitation | year + cell_id | 0                     | Z_CODE, data = data) 
          
          ## IV Regressions
          iv1  <- felm(dmspols_harmon_ihs ~ temp_avg + precipitation   | year + cell_id | (MA_var ~ MA_var_exc) |                                                                       Z_CODE, data = data) 
          iv2  <- felm(dmspols_harmon_ihs ~ temp_avg + precipitation   | year + cell_id | (MA_var|MA_varXdmspols_1996_bin4_2|MA_varXdmspols_1996_bin4_3|MA_varXdmspols_1996_bin4_4 ~ MA_var_exc + MA_var_excXdmspols_1996_bin4_2 + MA_var_excXdmspols_1996_bin4_3 + MA_var_excXdmspols_1996_bin4_4) | Z_CODE, data = data) 
          iv3  <- felm(dmspols_harmon_ihs ~ temp_avg + precipitation   | year + cell_id | (MA_var|MA_varXdistance_city_addisababa ~ MA_var_exc + MA_var_excXdistance_city_addisababa) | Z_CODE, data = data) 
          
          iv4  <- felm(dmspols_harmon_sum2_ihs ~ temp_avg + precipitation   | year + cell_id | (MA_var ~ MA_var_exc) |                                                                       Z_CODE, data = data) 
          iv5  <- felm(dmspols_harmon_sum2_ihs ~ temp_avg + precipitation   | year + cell_id | (MA_var|MA_varXdmspols_1996_bin4_2|MA_varXdmspols_1996_bin4_3|MA_varXdmspols_1996_bin4_4 ~ MA_var_exc + MA_var_excXdmspols_1996_bin4_2 + MA_var_excXdmspols_1996_bin4_3 + MA_var_excXdmspols_1996_bin4_4) | Z_CODE, data = data) 
          iv6  <- felm(dmspols_harmon_sum2_ihs ~ temp_avg + precipitation   | year + cell_id | (MA_var|MA_varXdistance_city_addisababa ~ MA_var_exc + MA_var_excXdistance_city_addisababa) | Z_CODE, data = data) 
          
          iv7  <- felm(dmspols_harmon_sum6_ihs ~ temp_avg + precipitation   | year + cell_id | (MA_var ~ MA_var_exc) |                                                                       Z_CODE, data = data)
          iv8  <- felm(dmspols_harmon_sum6_ihs ~ temp_avg + precipitation   | year + cell_id | (MA_var|MA_varXdmspols_1996_bin4_2|MA_varXdmspols_1996_bin4_3|MA_varXdmspols_1996_bin4_4 ~ MA_var_exc + MA_var_excXdmspols_1996_bin4_2 + MA_var_excXdmspols_1996_bin4_3 + MA_var_excXdmspols_1996_bin4_4) | Z_CODE, data = data) 
          iv9  <- felm(dmspols_harmon_sum6_ihs ~ temp_avg + precipitation   | year + cell_id | (MA_var|MA_varXdistance_city_addisababa ~ MA_var_exc + MA_var_excXdistance_city_addisababa) | Z_CODE, data = data) 
          
          iv10 <- felm(globcover_urban_sum_ihs ~ temp_avg + precipitation   | year + cell_id | (MA_var ~ MA_var_exc) |                                                                       Z_CODE, data = data) 
          iv11  <- felm(globcover_urban_sum_ihs ~ temp_avg + precipitation   | year + cell_id | (MA_var|MA_varXdmspols_1996_bin4_2|MA_varXdmspols_1996_bin4_3|MA_varXdmspols_1996_bin4_4 ~ MA_var_exc + MA_var_excXdmspols_1996_bin4_2 + MA_var_excXdmspols_1996_bin4_3 + MA_var_excXdmspols_1996_bin4_4) | Z_CODE, data = data) 
          iv12 <- felm(globcover_urban_sum_ihs ~ temp_avg + precipitation   | year + cell_id | (MA_var|MA_varXdistance_city_addisababa ~ MA_var_exc + MA_var_excXdistance_city_addisababa) | Z_CODE, data = data)
          
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
                    ols10,
                    ols11,
                    ols12,
                    dep.var.labels.include = T,
                    dep.var.labels   = c("NTL", "NTL$\\geq$2", "NTL$\\geq6$", "GlobCover-Urban"),
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
                      c("Year   FE", rep("Y", 16)),
                      c("Unit FE", rep("Y", 16))
                    ),
                    out=file.path(paper_tables,
                                  paste0("MA_table",log,"_theta",theta,exclude,"_",unit,MA_ubanrural,"_ols.tex")))
          
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
                    iv10,
                    iv11,
                    iv12,
                    dep.var.labels.include = T,
                    dep.var.labels   = c("NTL", "NTL$\\geq$2", "NTL$\\geq6$", "GlobCover-Urban"),
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
                      c("Year   FE", rep("Y", 16)),
                      c("Unit FE", rep("Y", 16))
                    ),
                    out=file.path(paper_tables,
                                  paste0("MA_table",log,"_theta",theta,exclude,"_",unit,MA_ubanrural,"_iv.tex")))
          
        }
      }
    }
  }
}




