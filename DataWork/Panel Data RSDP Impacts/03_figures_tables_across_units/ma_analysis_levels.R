# Market Access Analysis

unit <- "clusters_of_ntl"
theta <- "1"
log <- "_log"
exclude <- "_exclude50km"
dv <- "dmspols_zhang_ihs"
MA_ubanrural <- ""

# Regressions ------------------------------------------------------------------
for(unit in c("woreda", "clusters_of_ntl", "clusters_of_globcover_urban")){ # "woreda ,"clusters_of_ntl", "clusters_of_globcover_urban"
  for(theta in c("3_8")){ # "1","2","5","8"
    for(log in c("_log")){
      for(exclude in c("_exclude50km")){ # "_exclude20km", "_exclude100km","_exclude100km"
        for(MA_ubanrural in c("")){ # "_urban2", "_rural2"
          
          ## Load/Subset Data
          data <- readRDS(file.path(panel_rsdp_imp_data_file_path, unit, "merged_datasets", "panel_data_clean.Rds"))
          
          data <- data %>%
            filter(year >= 1996)
          
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
          data$MA_varXdmspols_1996_bin3_4 <- data$MA_var * data$dmspols_1996_bin3_4
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
          data$MA_var_excXdmspols_1996_bin3_4 <- data$MA_var_exc * data$dmspols_1996_bin3_4
          data$MA_var_excXdmspols_1996 <- data$MA_var_exc * data$dmspols_1996
          data$MA_var_excXdmspols_1996_sq <- data$MA_var_exc * data$dmspols_1996_sq

          ## OLS Regressions
          ols1  <- felm(dmspols_zhang_ihs ~ MA_var_exc                                       + temp_avg + precipitation | year + cell_id | 0                     | Z_CODE, data = data)
          #ols2  <- felm(dmspols_zhang_ihs ~ MA_var_exc + MA_var_excXdmspols_1996_bin3_2 + MA_var_excXdmspols_1996_bin3_3 + temp_avg + precipitation | year + cell_id | 0  | Z_CODE, data = data)
          ols2  <- felm(dmspols_zhang_ihs ~ MA_var_exc + MA_var_excXdmspols_zhang_ihs_1996   + temp_avg + precipitation | year + cell_id | 0                     | Z_CODE, data = data)
          ols3  <- felm(dmspols_zhang_ihs ~ MA_var_exc + MA_var_excXdmspols_zhang_sum2_ihs_1996 + temp_avg + precipitation | year + cell_id | 0                     | Z_CODE, data = data) 
          ols4  <- felm(dmspols_zhang_ihs ~ MA_var_exc + MA_var_excXdistance_city_addisababa + temp_avg + precipitation | year + cell_id | 0                     | Z_CODE, data = data) 
          
          ols5  <- felm(dmspols_zhang_sum2_ihs ~ MA_var_exc                                       + temp_avg + precipitation | year + cell_id | 0                     | Z_CODE, data = data)
          #ols5  <- felm(dmspols_zhang_sum2_ihs ~ MA_var_exc + MA_var_excXdmspols_1996_bin3_2 + MA_var_excXdmspols_1996_bin3_3 + temp_avg + precipitation | year + cell_id | 0  | Z_CODE, data = data)
          ols6  <- felm(dmspols_zhang_sum2_ihs ~ MA_var_exc + MA_var_excXdmspols_zhang_ihs_1996   + temp_avg + precipitation | year + cell_id | 0                     | Z_CODE, data = data)
          ols7  <- felm(dmspols_zhang_sum2_ihs ~ MA_var_exc + MA_var_excXdmspols_zhang_sum2_ihs_1996 + temp_avg + precipitation | year + cell_id | 0                     | Z_CODE, data = data) 
          ols8  <- felm(dmspols_zhang_sum2_ihs ~ MA_var_exc + MA_var_excXdistance_city_addisababa + temp_avg + precipitation | year + cell_id | 0                     | Z_CODE, data = data) 
          
          ols9  <- felm(dmspols_zhang_sum6_ihs ~ MA_var_exc                                       + temp_avg + precipitation | year + cell_id | 0                     | Z_CODE, data = data)
          #ols8  <- felm(dmspols_zhang_sum6_ihs ~ MA_var_exc + MA_var_excXdmspols_1996_bin3_2 + MA_var_excXdmspols_1996_bin3_3 + temp_avg + precipitation | year + cell_id | 0  | Z_CODE, data = data)
          ols10 <- felm(dmspols_zhang_sum6_ihs ~ MA_var_exc + MA_var_excXdmspols_zhang_ihs_1996   + temp_avg + precipitation | year + cell_id | 0                     | Z_CODE, data = data)
          ols11 <- felm(dmspols_zhang_sum6_ihs ~ MA_var_exc + MA_var_excXdmspols_zhang_sum2_ihs_1996 + temp_avg + precipitation | year + cell_id | 0                     | Z_CODE, data = data) 
          ols12 <- felm(dmspols_zhang_sum6_ihs ~ MA_var_exc + MA_var_excXdistance_city_addisababa + temp_avg + precipitation | year + cell_id | 0                     | Z_CODE, data = data) 
          
          ols13 <- felm(globcover_urban_sum_ihs ~ MA_var_exc                                       + temp_avg + precipitation | year + cell_id | 0                     | Z_CODE, data = data)
          #ols11  <- felm(globcover_urban_sum_ihs ~ MA_var_exc + MA_var_excXdmspols_1996_bin3_2 + MA_var_excXdmspols_1996_bin3_3 + temp_avg + precipitation | year + cell_id | 0  | Z_CODE, data = data)
          ols14 <- felm(globcover_urban_sum_ihs ~ MA_var_exc + MA_var_excXdmspols_zhang_ihs_1996   + temp_avg + precipitation | year + cell_id | 0                     | Z_CODE, data = data)
          ols15 <- felm(globcover_urban_sum_ihs ~ MA_var_exc + MA_var_excXdmspols_zhang_sum2_ihs_1996 + temp_avg + precipitation | year + cell_id | 0                     | Z_CODE, data = data) 
          ols16 <- felm(globcover_urban_sum_ihs ~ MA_var_exc + MA_var_excXdistance_city_addisababa + temp_avg + precipitation | year + cell_id | 0                     | Z_CODE, data = data) 
          
          ## IV Regressions
          iv1  <- felm(dmspols_zhang_ihs ~ temp_avg + precipitation   | year + cell_id | (MA_var ~ MA_var_exc) |                                                                       Z_CODE, data = data) 
          #iv2  <- felm(dmspols_zhang_ihs ~ temp_avg + precipitation   | year + cell_id | (MA_var|MA_varXdmspols_1996_bin3_2|MA_varXdmspols_1996_bin3_3   ~ MA_var_exc + MA_var_excXdmspols_1996_bin3_2 + MA_var_excXdmspols_1996_bin3_3 )   | Z_CODE, data = data) 
          iv2  <- felm(dmspols_zhang_ihs ~ temp_avg + precipitation   | year + cell_id | (MA_var|MA_varXdmspols_zhang_ihs_1996   ~ MA_var_exc + MA_var_excXdmspols_zhang_ihs_1996)   | Z_CODE, data = data) 
          iv3  <- felm(dmspols_zhang_ihs ~ temp_avg + precipitation   | year + cell_id | (MA_var|MA_varXdmspols_zhang_sum2_ihs_1996 ~ MA_var_exc + MA_var_excXdmspols_zhang_sum2_ihs_1996) | Z_CODE, data = data) 
          iv4  <- felm(dmspols_zhang_ihs ~ temp_avg + precipitation   | year + cell_id | (MA_var|MA_varXdistance_city_addisababa ~ MA_var_exc + MA_var_excXdistance_city_addisababa) | Z_CODE, data = data) 
          
          iv5  <- felm(dmspols_zhang_sum2_ihs ~ temp_avg + precipitation   | year + cell_id | (MA_var ~ MA_var_exc) |                                                                       Z_CODE, data = data) 
          #iv5  <- felm(dmspols_zhang_sum2_ihs ~ temp_avg + precipitation   | year + cell_id | (MA_var|MA_varXdmspols_1996_bin3_2|MA_varXdmspols_1996_bin3_3   ~ MA_var_exc + MA_var_excXdmspols_1996_bin3_2 + MA_var_excXdmspols_1996_bin3_3 )   | Z_CODE, data = data) 
          iv6  <- felm(dmspols_zhang_sum2_ihs ~ temp_avg + precipitation   | year + cell_id | (MA_var|MA_varXdmspols_zhang_ihs_1996   ~ MA_var_exc + MA_var_excXdmspols_zhang_ihs_1996)   | Z_CODE, data = data) 
          iv7  <- felm(dmspols_zhang_sum2_ihs ~ temp_avg + precipitation   | year + cell_id | (MA_var|MA_varXdmspols_zhang_sum2_ihs_1996 ~ MA_var_exc + MA_var_excXdmspols_zhang_sum2_ihs_1996) | Z_CODE, data = data) 
          iv8  <- felm(dmspols_zhang_sum2_ihs ~ temp_avg + precipitation   | year + cell_id | (MA_var|MA_varXdistance_city_addisababa ~ MA_var_exc + MA_var_excXdistance_city_addisababa) | Z_CODE, data = data) 
          
          iv9  <- felm(dmspols_zhang_sum6_ihs ~ temp_avg + precipitation   | year + cell_id | (MA_var ~ MA_var_exc) |                                                                       Z_CODE, data = data)
          #iv8  <- felm(dmspols_zhang_sum6_ihs ~ temp_avg + precipitation   | year + cell_id | (MA_var|MA_varXdmspols_1996_bin3_2|MA_varXdmspols_1996_bin3_3   ~ MA_var_exc + MA_var_excXdmspols_1996_bin3_2 + MA_var_excXdmspols_1996_bin3_3 )   | Z_CODE, data = data) 
          iv10  <- felm(dmspols_zhang_sum6_ihs ~ temp_avg + precipitation   | year + cell_id | (MA_var|MA_varXdmspols_zhang_ihs_1996   ~ MA_var_exc + MA_var_excXdmspols_zhang_ihs_1996)   | Z_CODE, data = data) 
          iv11  <- felm(dmspols_zhang_sum6_ihs ~ temp_avg + precipitation   | year + cell_id | (MA_var|MA_varXdmspols_zhang_sum2_ihs_1996 ~ MA_var_exc + MA_var_excXdmspols_zhang_sum2_ihs_1996) | Z_CODE, data = data) 
          iv12  <- felm(dmspols_zhang_sum6_ihs ~ temp_avg + precipitation   | year + cell_id | (MA_var|MA_varXdistance_city_addisababa ~ MA_var_exc + MA_var_excXdistance_city_addisababa) | Z_CODE, data = data) 
          
          iv13 <- felm(globcover_urban_sum_ihs ~ temp_avg + precipitation   | year + cell_id | (MA_var ~ MA_var_exc) |                                                                       Z_CODE, data = data) 
          #iv11  <- felm(globcover_urban_sum_ihs ~ temp_avg + precipitation   | year + cell_id | (MA_var|MA_varXdmspols_1996_bin3_2|MA_varXdmspols_1996_bin3_3   ~ MA_var_exc + MA_var_excXdmspols_1996_bin3_2 + MA_var_excXdmspols_1996_bin3_3 )   | Z_CODE, data = data) 
          iv14 <- felm(globcover_urban_sum_ihs ~ temp_avg + precipitation   | year + cell_id | (MA_var|MA_varXdmspols_zhang_ihs_1996   ~ MA_var_exc + MA_var_excXdmspols_zhang_ihs_1996)   | Z_CODE, data = data) 
          iv15 <- felm(globcover_urban_sum_ihs ~ temp_avg + precipitation   | year + cell_id | (MA_var|MA_varXdmspols_zhang_sum2_ihs_1996 ~ MA_var_exc + MA_var_excXdmspols_zhang_sum2_ihs_1996) | Z_CODE, data = data)
          iv16 <- felm(globcover_urban_sum_ihs ~ temp_avg + precipitation   | year + cell_id | (MA_var|MA_varXdistance_city_addisababa ~ MA_var_exc + MA_var_excXdistance_city_addisababa) | Z_CODE, data = data)
          
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
                    ols13,
                    ols14,
                    ols15,
                    ols16,
                    dep.var.labels.include = T,
                    dep.var.labels   = c("NTL", "NTL$>2$", "NTL$>6$", "Urban"),
                    #keep=c("MA_var"),
                    omit = c("temp_avg", "precipitation"),
                    covariate.labels = c("MA",
                                         "MA$\\times NTL_{96}$ Low",
                                         "MA$\\times NTL_{96}$ High",
                                         "MA X Dist Addis"),
                    dep.var.caption = "",
                    omit.stat = c("f","ser"), 
                    align=TRUE,
                    no.space=TRUE,
                    float=FALSE,
                    column.sep.width = "8pt",
                    digits = 2,
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
                    iv13,
                    iv14,
                    iv15,
                    iv16,
                    dep.var.labels.include = T,
                    dep.var.labels   = c("NTL", "NTL$>2$", "NTL$>6$", "Urban"),
                    #keep=c("MA_var"),
                    omit = c("temp_avg", "precipitation"),
                    covariate.labels = c("MA",
                                         "MA$\\times NTL_{96}$ Low",
                                         "MA$\\times NTL_{96}$ High",
                                         "MA X Dist Addis"),
                    dep.var.caption = "",
                    omit.stat = c("f","ser"), 
                    align=TRUE,
                    no.space=TRUE,
                    float=FALSE,
                    column.sep.width = "8pt",
                    digits = 2,
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




