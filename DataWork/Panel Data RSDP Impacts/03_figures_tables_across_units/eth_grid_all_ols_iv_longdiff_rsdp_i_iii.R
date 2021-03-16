# Instrumental Variables

# Resources for IV in R
# https://rpubs.com/wsundstrom/t_ivreg
# http://eclr.humanities.manchester.ac.uk/index.php/IV_in_R
# MA_varXdmspols_zhang_sum2_ihs_1996
# W_CODE

ROUND_NUM <- 1 # number of digits to round numbers

# dmspols_grid_ethiopia, woreda, clusters_of_ntlall, clusters_of_globcover_urban
DATASET_TYPE <- "clusters_of_ntlall"

for(DATASET_TYPE in c("woreda",
                      "dmspols_grid_ethiopia")){
  
  # Load Data --------------------------------------------------------------------
  df <- readRDS(file.path(panel_rsdp_imp_data_file_path, 
                          DATASET_TYPE, 
                          "merged_datasets", 
                          paste0("longdiff_data_clean_base", 
                                 1996,
                                 "_end",
                                 2009,
                                 ".Rds")))
  
  # Dataset specific parameters and variables ------------------------------------
  # Distances in meters
  
  if(DATASET_TYPE %in% "dmspols_grid_ethiopia"){
    CLUSTER_VAR <- "W_CODE"
    
    NEAR_TARGETTED_LOCATION <- 5000
    RM_DISTANE_ADDIS <- 5000
    NEAR_ROAD <- 5000
    
    df$dv_dmspols <- df$dmspols_harmon_ihs
    df$dv_gcurban <- df$globcover_urban
    df$dv_gccrop  <- df$globcover_cropland
    
    df$dv_dmspols_1996 <- df$dmspols_harmon_ihs_1996
    df$dv_gcurban_1996 <- df$globcover_urban_1996
    df$dv_gccrop_1996  <- df$globcover_cropland_1996
  
    
  } else{
    CLUSTER_VAR <- 0
    
    NEAR_TARGETTED_LOCATION <- 0
    RM_DISTANE_ADDIS <- 20000
    NEAR_ROAD <- 0
    
    df$dv_dmspols <- df$dmspols_harmon_ihs
    df$dv_gcurban <- df$globcover_urban_sum_ihs
    df$dv_gccrop  <- df$globcover_cropland_sum_ihs
    
    df$dv_dmspols_1996 <- df$dmspols_harmon_ihs_1996
    df$dv_gcurban_1996 <- df$globcover_urban_1996
    df$dv_gccrop_1996  <- df$globcover_cropland_1996
    
  }
  
  # Prep Data ------------------------------------------------------------------
  df <- df %>%
    filter(distance_rsdp123_targettedlocs > NEAR_TARGETTED_LOCATION) %>%
    filter(distance_city_addisababa       > RM_DISTANE_ADDIS) %>%
    mutate(near_rsdp123         = as.numeric(distance_rsdp123 <= NEAR_ROAD),
           near_mst_euc        = as.numeric(distance_rsdp123_mst_euc        <= NEAR_ROAD),
           near_mst_euc_region = as.numeric(distance_rsdp123_mst_euc_region <= NEAR_ROAD),
           near_mst_lc        = as.numeric(distance_rsdp123_mst_lc        <= NEAR_ROAD),
           near_mst_lc_region = as.numeric(distance_rsdp123_mst_lc_region <= NEAR_ROAD)) %>%
    
    mutate(near_rsdp123Xdmspols_1996_bin4_1      = near_rsdp123 * dmspols_1996_bin4_1,
           near_rsdp123Xdmspols_1996_bin4_2      = near_rsdp123 * dmspols_1996_bin4_2,
           near_rsdp123Xdmspols_1996_bin4_3      = near_rsdp123 * dmspols_1996_bin4_3,
           near_rsdp123Xdmspols_1996_bin4_4      = near_rsdp123 * dmspols_1996_bin4_4,
           near_rsdp123Xdistance_city_addisababa = near_rsdp123 * distance_city_addisababa,
           
           near_mst_eucXdmspols_1996_bin4_1      = near_mst_euc * dmspols_1996_bin4_1,
           near_mst_eucXdmspols_1996_bin4_2      = near_mst_euc * dmspols_1996_bin4_2,
           near_mst_eucXdmspols_1996_bin4_3      = near_mst_euc * dmspols_1996_bin4_3,
           near_mst_eucXdmspols_1996_bin4_4      = near_mst_euc * dmspols_1996_bin4_4,
           near_mst_eucXdistance_city_addisababa = near_mst_euc * distance_city_addisababa,
           
           near_mst_euc_regionXdmspols_1996_bin4_1      = near_mst_euc_region * dmspols_1996_bin4_1,
           near_mst_euc_regionXdmspols_1996_bin4_2      = near_mst_euc_region * dmspols_1996_bin4_2,
           near_mst_euc_regionXdmspols_1996_bin4_3      = near_mst_euc_region * dmspols_1996_bin4_3,
           near_mst_euc_regionXdmspols_1996_bin4_4      = near_mst_euc_region * dmspols_1996_bin4_4,
           near_mst_euc_regionXdistance_city_addisababa = near_mst_euc_region * distance_city_addisababa,
           
           near_mst_lcXdmspols_1996_bin4_1      = near_mst_lc * dmspols_1996_bin4_1,
           near_mst_lcXdmspols_1996_bin4_2      = near_mst_lc * dmspols_1996_bin4_2,
           near_mst_lcXdmspols_1996_bin4_3      = near_mst_lc * dmspols_1996_bin4_3,
           near_mst_lcXdmspols_1996_bin4_4      = near_mst_lc * dmspols_1996_bin4_4,
           near_mst_lcXdistance_city_addisababa = near_mst_lc * distance_city_addisababa,
           
           near_mst_lc_regionXdmspols_1996_bin4_1      = near_mst_lc_region * dmspols_1996_bin4_1,
           near_mst_lc_regionXdmspols_1996_bin4_2      = near_mst_lc_region * dmspols_1996_bin4_2,
           near_mst_lc_regionXdmspols_1996_bin4_3      = near_mst_lc_region * dmspols_1996_bin4_3,
           near_mst_lc_regionXdmspols_1996_bin4_4      = near_mst_lc_region * dmspols_1996_bin4_4,
           near_mst_lc_regionXdistance_city_addisababa = near_mst_lc_region * distance_city_addisababa)
  
  df$distance_city_addisababa <- df$distance_city_addisababa / 1000 / 100 # 100km scale
  df$distance_rsdp123_targettedlocs_log         <- log(df$distance_rsdp123_targettedlocs + 1)

  # Summary Stats ----------------------------------------------------------------
  df_neartc <- df %>%
    mutate(N_units = 1) %>%
    group_by(near_rsdp123) %>%
    dplyr::summarise(N_units = sum(N_units))
  
  df_mean <- df %>%
    group_by(near_rsdp123) %>%
    dplyr::summarise_at(vars(c(dv_dmspols,
                               dv_gcurban,
                               dv_gccrop,
                               dv_dmspols_1996,
                               dv_gcurban_1996,
                               dv_gccrop_1996)),
                        mean, na.rm = T)
  
  df_sum_nonzero <- df %>%
    mutate(dv_dmspols_1996_g0 = as.numeric(dv_dmspols_1996 > 0),
           dv_gcurban_1996_g0 = as.numeric(dv_gcurban_1996 > 0),
           dv_gccrop_1996_g0 = as.numeric(dv_gccrop_1996 > 0)) %>%
    dplyr::group_by(near_rsdp123) %>%
    dplyr::summarise_at(vars(c(dv_dmspols_1996_g0,
                               dv_gcurban_1996_g0,
                               dv_gccrop_1996_g0)),
                        sum, na.rm = T)
  
  df_tc_coll <- df_mean %>%
    left_join(df_sum_nonzero, by = "near_rsdp123") %>%
    pivot_longer(cols = -c(near_rsdp123)) 
  
  df_tc_coll$type <- "mean_change"
  df_tc_coll$type[grepl("_1996$", df_tc_coll$name)] <- "mean_1996"
  df_tc_coll$type[grepl("_1996_g0$", df_tc_coll$name)] <- "g0_1996"
  
  df_tc_coll$name <- df_tc_coll$name %>% str_replace_all("_1996_g0", "") %>% str_replace_all("_1996", "")
  
  #df_tc_coll <- df_tc_coll %>%
  #  pivot_wider(values_from = value,
  #              names_from = type)
  
  make_sum_stat <- function(dvname, df_tc_coll){
    df_dv_i <- df_tc_coll[df_tc_coll$name %in% dvname,]
    
    paste(
      df_dv_i$value[(df_dv_i$near_rsdp123 %in% T) & (df_dv_i$type %in% "mean_change")] %>% round(4),
      df_dv_i$value[(df_dv_i$near_rsdp123 %in% F) & (df_dv_i$type %in% "mean_change")] %>% round(4),
      
      df_dv_i$value[(df_dv_i$near_rsdp123 %in% T) & (df_dv_i$type %in% "mean_1996")] %>% round(3),
      df_dv_i$value[(df_dv_i$near_rsdp123 %in% F) & (df_dv_i$type %in% "mean_1996")] %>% round(3),
      
      df_dv_i$value[(df_dv_i$near_rsdp123 %in% T) & (df_dv_i$type %in% "g0_1996")] %>% round(3),
      df_dv_i$value[(df_dv_i$near_rsdp123 %in% F) & (df_dv_i$type %in% "g0_1996")] %>% round(3),
      sep = " & "
    )
    
  }
  
  sink(file.path(paper_tables,
                 paste0("iv_rsdp123_sumstat_",DATASET_TYPE,".tex")))
  
  cat("\\begin{tabular}{l | cc | cc | cc} ")
  cat("\\hline ")
  
  cat("Variable & \\multicolumn{2}{c|}{Mean Change} & \\multicolumn{2}{c|}{Mean, 1996} & \\multicolumn{2}{c}{N Units $>0$, 1996} \\\\ ")
  #cat("\\hline ")
  cat(" & Treated & Control & Treated & Control & Treated & Control \\\\ ")
  cat("\\hline ")
  
  cat("DMSP-OLS & ")
  cat(make_sum_stat("dv_dmspols", df_tc_coll))
  cat("\\\\ ")
  
  cat("Urban & ")
  cat(make_sum_stat("dv_gcurban", df_tc_coll))
  cat("\\\\ ")
  
  cat("Cropland & ")
  cat(make_sum_stat("dv_gccrop", df_tc_coll))
  cat("\\\\ ")
  cat("\\hline ")
  cat("\\multicolumn{7}{l}{N Units = ",
      nrow(df) %>% prettyNum(big.mark=",",scientific=FALSE),
      "; N Treated = ", df_neartc$N_units[df_neartc$near_rsdp123 %in% 1] %>% prettyNum(big.mark=",",scientific=FALSE),
      "; N Control = ", df_neartc$N_units[df_neartc$near_rsdp123 %in% 0] %>% prettyNum(big.mark=",",scientific=FALSE),
      "} ", sep = "")
  
  cat("\\end{tabular} ")
  
  sink()
  
  # OLS --------------------------------------------------------------------------
  # Need to do as.formula %>% felm(), as if do felm(as.formula), then dep var heading 
  # in stargazer is the full formula. Piping formula in solves.
  
  lm_dmspols_ihs     <- as.formula(paste("dv_dmspols ~ near_rsdp123 + distance_rsdp123_targettedlocs_log | 0 | 0 | ", CLUSTER_VAR)) %>% felm(data = df)
  lm_globcover_urban <- as.formula(paste("dv_gcurban ~ near_rsdp123 + distance_rsdp123_targettedlocs_log | 0 | 0 | ", CLUSTER_VAR)) %>% felm(data = df)
  lm_globcover_crop  <- as.formula(paste("dv_gccrop  ~ near_rsdp123 + distance_rsdp123_targettedlocs_log | 0 | 0 | ", CLUSTER_VAR)) %>% felm(data = df)
  
  lm_dmspols_ihs_addis     <- as.formula(paste("dv_dmspols ~ near_rsdp123 + distance_rsdp123_targettedlocs_log + near_rsdp123Xdistance_city_addisababa | 0 | 0 | ", CLUSTER_VAR)) %>% felm(data = df)
  lm_globcover_urban_addis <- as.formula(paste("dv_gcurban ~ near_rsdp123 + distance_rsdp123_targettedlocs_log + near_rsdp123Xdistance_city_addisababa | 0 | 0 | ", CLUSTER_VAR)) %>% felm(data = df)
  lm_globcover_crop_addis  <- as.formula(paste("dv_gccrop  ~ near_rsdp123 + distance_rsdp123_targettedlocs_log + near_rsdp123Xdistance_city_addisababa | 0 | 0 | ", CLUSTER_VAR)) %>% felm(data = df)
  
  lm_dmspols_ihs_basentl     <- as.formula(paste("dv_dmspols ~ near_rsdp123 + distance_rsdp123_targettedlocs_log + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4 | 0 | 0 | ", CLUSTER_VAR)) %>% felm(data = df)
  lm_globcover_urban_basentl <- as.formula(paste("dv_gcurban ~ near_rsdp123 + distance_rsdp123_targettedlocs_log + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4 | 0 | 0 | ", CLUSTER_VAR)) %>% felm(data = df)
  lm_globcover_crop_basentl  <- as.formula(paste("dv_gccrop  ~ near_rsdp123 + distance_rsdp123_targettedlocs_log + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4 | 0 | 0 | ", CLUSTER_VAR)) %>% felm(data = df)
  
  # MST - Cost Distance --------------------------------------------------------
  iv_cd_dmspols_ihs     <- as.formula(paste("dv_dmspols ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123 ~ near_mst_lc) | ", CLUSTER_VAR)) %>% felm(data = df)
  iv_cd_globcover_urban <- as.formula(paste("dv_gcurban ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123 ~ near_mst_lc) | ", CLUSTER_VAR)) %>% felm(data = df)
  iv_cd_globcover_crop  <- as.formula(paste("dv_gccrop  ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123 ~ near_mst_lc) | ", CLUSTER_VAR)) %>% felm(data = df)
  
  iv_cd_dmspols_ihs_addis     <- as.formula(paste("dv_dmspols ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdistance_city_addisababa ~ near_mst_lc + near_mst_lcXdistance_city_addisababa) | ", CLUSTER_VAR)) %>% felm(data = df)
  iv_cd_globcover_urban_addis <- as.formula(paste("dv_gcurban ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdistance_city_addisababa ~ near_mst_lc + near_mst_lcXdistance_city_addisababa) | ", CLUSTER_VAR)) %>% felm(data = df)
  iv_cd_globcover_crop_addis  <- as.formula(paste("dv_gccrop  ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdistance_city_addisababa ~ near_mst_lc + near_mst_lcXdistance_city_addisababa) | ", CLUSTER_VAR)) %>% felm(data = df)
  
  iv_cd_dmspols_ihs_basentl     <- as.formula(paste("dv_dmspols ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdmspols_1996_bin4_2|near_rsdp123Xdmspols_1996_bin4_3|near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_lc + near_mst_lcXdmspols_1996_bin4_2 + near_mst_lcXdmspols_1996_bin4_3 + near_mst_lcXdmspols_1996_bin4_4) | ", CLUSTER_VAR)) %>% felm(data = df)
  iv_cd_globcover_urban_basentl <- as.formula(paste("dv_gcurban ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdmspols_1996_bin4_2|near_rsdp123Xdmspols_1996_bin4_3|near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_lc + near_mst_lcXdmspols_1996_bin4_2 + near_mst_lcXdmspols_1996_bin4_3 + near_mst_lcXdmspols_1996_bin4_4) | ", CLUSTER_VAR)) %>% felm(data = df)
  iv_cd_globcover_crop_basentl  <- as.formula(paste("dv_gccrop  ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdmspols_1996_bin4_2|near_rsdp123Xdmspols_1996_bin4_3|near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_lc + near_mst_lcXdmspols_1996_bin4_2 + near_mst_lcXdmspols_1996_bin4_3 + near_mst_lcXdmspols_1996_bin4_4) | ", CLUSTER_VAR)) %>% felm(data = df)
  
  # MST - Least Distance -------------------------------------------------------
  iv_ld_dmspols_ihs     <- as.formula(paste("dv_dmspols ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123 ~ near_mst_euc) | ", CLUSTER_VAR)) %>% felm(data = df)
  iv_ld_globcover_urban <- as.formula(paste("dv_gcurban ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123 ~ near_mst_euc) | ", CLUSTER_VAR)) %>% felm(data = df)
  iv_ld_globcover_crop  <- as.formula(paste("dv_gccrop  ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123 ~ near_mst_euc) | ", CLUSTER_VAR)) %>% felm(data = df)
  
  iv_ld_dmspols_ihs_addis     <- as.formula(paste("dv_dmspols ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdistance_city_addisababa ~ near_mst_euc + near_mst_eucXdistance_city_addisababa) | ", CLUSTER_VAR)) %>% felm(data = df)
  iv_ld_globcover_urban_addis <- as.formula(paste("dv_gcurban ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdistance_city_addisababa ~ near_mst_euc + near_mst_eucXdistance_city_addisababa) | ", CLUSTER_VAR)) %>% felm(data = df)
  iv_ld_globcover_crop_addis  <- as.formula(paste("dv_gccrop  ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdistance_city_addisababa ~ near_mst_euc + near_mst_eucXdistance_city_addisababa) | ", CLUSTER_VAR)) %>% felm(data = df)
  
  iv_ld_dmspols_ihs_basentl     <- as.formula(paste("dv_dmspols ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdmspols_1996_bin4_2|near_rsdp123Xdmspols_1996_bin4_3|near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_euc + near_mst_eucXdmspols_1996_bin4_2 + near_mst_eucXdmspols_1996_bin4_3 + near_mst_eucXdmspols_1996_bin4_4) | ", CLUSTER_VAR)) %>% felm(data = df)
  iv_ld_globcover_urban_basentl <- as.formula(paste("dv_gcurban ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdmspols_1996_bin4_2|near_rsdp123Xdmspols_1996_bin4_3|near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_euc + near_mst_eucXdmspols_1996_bin4_2 + near_mst_eucXdmspols_1996_bin4_3 + near_mst_eucXdmspols_1996_bin4_4) | ", CLUSTER_VAR)) %>% felm(data = df)
  iv_ld_globcover_crop_basentl  <- as.formula(paste("dv_gccrop  ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdmspols_1996_bin4_2|near_rsdp123Xdmspols_1996_bin4_3|near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_euc + near_mst_eucXdmspols_1996_bin4_2 + near_mst_eucXdmspols_1996_bin4_3 + near_mst_eucXdmspols_1996_bin4_4) | ", CLUSTER_VAR)) %>% felm(data = df)
  
  # MST Regions - Cost Distance ------------------------------------------------
  iv_cd_dmspols_ihs_regions     <- as.formula(paste("dv_dmspols ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123 ~ near_mst_lc_region) | ", CLUSTER_VAR)) %>% felm(data = df)
  iv_cd_globcover_urban_regions <- as.formula(paste("dv_gcurban ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123 ~ near_mst_lc_region) | ", CLUSTER_VAR)) %>% felm(data = df)
  iv_cd_globcover_crop_regions  <- as.formula(paste("dv_gccrop  ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123 ~ near_mst_lc_region) | ", CLUSTER_VAR)) %>% felm(data = df)
  
  iv_cd_dmspols_ihs_addis_regions     <- as.formula(paste("dv_dmspols ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdistance_city_addisababa ~ near_mst_lc_region + near_mst_lc_regionXdistance_city_addisababa) | ", CLUSTER_VAR)) %>% felm(data = df)
  iv_cd_globcover_urban_addis_regions <- as.formula(paste("dv_gcurban ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdistance_city_addisababa ~ near_mst_lc_region + near_mst_lc_regionXdistance_city_addisababa) | ", CLUSTER_VAR)) %>% felm(data = df)
  iv_cd_globcover_crop_addis_regions  <- as.formula(paste("dv_gccrop  ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdistance_city_addisababa ~ near_mst_lc_region + near_mst_lc_regionXdistance_city_addisababa) | ", CLUSTER_VAR)) %>% felm(data = df)
  
  iv_cd_dmspols_ihs_basentl_regions     <- as.formula(paste("dv_dmspols ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdmspols_1996_bin4_2|near_rsdp123Xdmspols_1996_bin4_3|near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_lc_region + near_mst_lc_regionXdmspols_1996_bin4_2 + near_mst_lc_regionXdmspols_1996_bin4_3 + near_mst_lc_regionXdmspols_1996_bin4_4) | ", CLUSTER_VAR)) %>% felm(data = df)
  iv_cd_globcover_urban_basentl_regions <- as.formula(paste("dv_gcurban ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdmspols_1996_bin4_2|near_rsdp123Xdmspols_1996_bin4_3|near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_lc_region + near_mst_lc_regionXdmspols_1996_bin4_2 + near_mst_lc_regionXdmspols_1996_bin4_3 + near_mst_lc_regionXdmspols_1996_bin4_4) | ", CLUSTER_VAR)) %>% felm(data = df)
  iv_cd_globcover_crop_basentl_regions  <- as.formula(paste("dv_gccrop  ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdmspols_1996_bin4_2|near_rsdp123Xdmspols_1996_bin4_3|near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_lc_region + near_mst_lc_regionXdmspols_1996_bin4_2 + near_mst_lc_regionXdmspols_1996_bin4_3 + near_mst_lc_regionXdmspols_1996_bin4_4) | ", CLUSTER_VAR)) %>% felm(data = df)
  
  # MST Regions - Least Distance -------------------------------------------------------
  iv_ld_dmspols_ihs_regions     <- as.formula(paste("dv_dmspols ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123 ~ near_mst_euc_region) | ", CLUSTER_VAR)) %>% felm(data = df)
  iv_ld_globcover_urban_regions <- as.formula(paste("dv_gcurban ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123 ~ near_mst_euc_region) | ", CLUSTER_VAR)) %>% felm(data = df)
  iv_ld_globcover_crop_regions  <- as.formula(paste("dv_gccrop  ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123 ~ near_mst_euc_region) | ", CLUSTER_VAR)) %>% felm(data = df)
  
  iv_ld_dmspols_ihs_addis_regions     <- as.formula(paste("dv_dmspols ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdistance_city_addisababa ~ near_mst_euc_region + near_mst_euc_regionXdistance_city_addisababa) | ", CLUSTER_VAR)) %>% felm(data = df)
  iv_ld_globcover_urban_addis_regions <- as.formula(paste("dv_gcurban ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdistance_city_addisababa ~ near_mst_euc_region + near_mst_euc_regionXdistance_city_addisababa) | ", CLUSTER_VAR)) %>% felm(data = df)
  iv_ld_globcover_crop_addis_regions  <- as.formula(paste("dv_gccrop  ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdistance_city_addisababa ~ near_mst_euc_region + near_mst_euc_regionXdistance_city_addisababa) | ", CLUSTER_VAR)) %>% felm(data = df)
  
  iv_ld_dmspols_ihs_basentl_regions     <- as.formula(paste("dv_dmspols ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdmspols_1996_bin4_2|near_rsdp123Xdmspols_1996_bin4_3|near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_euc_region + near_mst_euc_regionXdmspols_1996_bin4_2 + near_mst_euc_regionXdmspols_1996_bin4_3 + near_mst_euc_regionXdmspols_1996_bin4_4) | ", CLUSTER_VAR)) %>% felm(data = df)
  iv_ld_globcover_urban_basentl_regions <- as.formula(paste("dv_gcurban ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdmspols_1996_bin4_2|near_rsdp123Xdmspols_1996_bin4_3|near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_euc_region + near_mst_euc_regionXdmspols_1996_bin4_2 + near_mst_euc_regionXdmspols_1996_bin4_3 + near_mst_euc_regionXdmspols_1996_bin4_4) | ", CLUSTER_VAR)) %>% felm(data = df)
  iv_ld_globcover_crop_basentl_regions  <- as.formula(paste("dv_gccrop  ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdmspols_1996_bin4_2|near_rsdp123Xdmspols_1996_bin4_3|near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_euc_region + near_mst_euc_regionXdmspols_1996_bin4_2 + near_mst_euc_regionXdmspols_1996_bin4_3 + near_mst_euc_regionXdmspols_1996_bin4_4) | ", CLUSTER_VAR)) %>% felm(data = df)
  
  # OLS - Stargazer --------------------------------------------------------------
  make_stargzer <- function(lm_dmspols_ihs,
                            lm_dmspols_ihs_basentl,
                            lm_dmspols_ihs_addis,
                            
                            lm_globcover_urban,
                            lm_globcover_urban_basentl,
                            lm_globcover_urban_addis,
                            
                            lm_globcover_crop,
                            lm_globcover_crop_basentl,
                            lm_globcover_crop_addis,
                            file_name){
    
    stargazer(lm_dmspols_ihs,
              lm_dmspols_ihs_basentl,
              lm_dmspols_ihs_addis,
              
              lm_globcover_urban,
              lm_globcover_urban_basentl,
              lm_globcover_urban_addis,
              
              lm_globcover_crop,
              lm_globcover_crop_basentl,
              lm_globcover_crop_addis,
              
              dep.var.labels.include = T,
              dep.var.labels = c("NTL", "Urban", "Cropland"), #  "NTL $\\geq$ 2", "NTL $\\geq$ 6",
              dep.var.caption = "",
              omit = c("temp_avg", "precipitation", "distance_rsdp123_targettedlocs_log"),
              covariate.labels = c("Imp Rd.",
                                   "Imp Rd.$\\times NTL_{96}$ Low",
                                   "Imp Rd.$\\times NTL_{96}$ Med",
                                   "Imp Rd.$\\times NTL_{96}$ High",
                                   "Imp Rd. X Dist Addis"),
              omit.stat = c("f","ser", "rsq"),
              align=TRUE,
              no.space=TRUE,
              float=FALSE,
              column.sep.width="-15pt",
              digits=2,
              omit.table.layout = "n",
              out = file.path(paper_tables, 
                              file_name))
    
  }
  
  # OLS ------------------------------------------------------------------------
  make_stargzer(lm_dmspols_ihs,
                lm_dmspols_ihs_basentl,
                lm_dmspols_ihs_addis,
                
                lm_globcover_urban,
                lm_globcover_urban_basentl,
                lm_globcover_urban_addis,
                
                lm_globcover_crop,
                lm_globcover_crop_basentl,
                lm_globcover_crop_addis,
                
                paste0("ols_near_road_5km_",DATASET_TYPE,"_results_rsdp123.tex"))
  
  
  
  # MST - Cost Distance - Stargazer ----------------------------------------------
  make_stargzer(iv_cd_dmspols_ihs,
                iv_cd_dmspols_ihs_basentl,
                iv_cd_dmspols_ihs_addis,
                
                iv_cd_globcover_urban,
                iv_cd_globcover_urban_basentl,
                iv_cd_globcover_urban_addis,
                
                iv_cd_globcover_crop,
                iv_cd_globcover_crop_basentl,
                iv_cd_globcover_crop_addis,
                
                paste0("iv_near_mst_cost_distance_5km_",DATASET_TYPE,"_results_rsdp123.tex"))
  
  # MST - Least Distance - Stargazer ---------------------------------------------
  make_stargzer(iv_ld_dmspols_ihs,
                iv_ld_dmspols_ihs_basentl,
                iv_ld_dmspols_ihs_addis,
                
                iv_ld_globcover_urban,
                iv_ld_globcover_urban_basentl,
                iv_ld_globcover_urban_addis,
                
                iv_ld_globcover_crop,
                iv_ld_globcover_crop_basentl,
                iv_ld_globcover_crop_addis,
                
                paste0("iv_near_mst_least_distance_5km_",DATASET_TYPE,"_results_rsdp123.tex"))
  
  # MST Regions - Cost Distance - Stargazer ----------------------------------------------
  make_stargzer(iv_cd_dmspols_ihs_regions,
                iv_cd_dmspols_ihs_basentl_regions,
                iv_cd_dmspols_ihs_addis_regions,
                
                iv_cd_globcover_urban_regions,
                iv_cd_globcover_urban_basentl_regions,
                iv_cd_globcover_urban_addis_regions,
                
                iv_cd_globcover_crop_regions,
                iv_cd_globcover_crop_basentl_regions,
                iv_cd_globcover_crop_addis_regions,
                
                paste0("iv_near_mst_cost_distance_5km_",DATASET_TYPE,"_results_rsdp123_regions.tex"))
  
  # MST Regions - Least Distance - Stargazer ---------------------------------------------
  make_stargzer(iv_ld_dmspols_ihs_regions,
                iv_ld_dmspols_ihs_basentl_regions,
                iv_ld_dmspols_ihs_addis_regions,
                
                iv_ld_globcover_urban_regions,
                iv_ld_globcover_urban_basentl_regions,
                iv_ld_globcover_urban_addis_regions,
                
                iv_ld_globcover_crop_regions,
                iv_ld_globcover_crop_basentl_regions,
                iv_ld_globcover_crop_addis_regions,
                
                paste0("iv_near_mst_least_distance_5km_",DATASET_TYPE,"_results_rsdp123_regions.tex"))
  
  
  # First Stage - Stargazer --------------------------------------
  stargazer(iv_cd_dmspols_ihs$stage1,
            iv_ld_dmspols_ihs$stage1,
            dep.var.labels.include = T,
            dep.var.labels = c("Near Improved Road"),
            dep.var.caption = "",
            covariate.labels = c("Near Least Cost MST",
                                 "Near Min. Distance MST"),
            omit = "distance_rsdp123_targettedlocs_log",
            omit.stat = c("f","ser", "rsq"),
            align=TRUE,
            no.space=TRUE,
            float=FALSE,
            column.sep.width="-15pt",
            digits=2,
            omit.table.layout = "n",
            add.lines = list(
              c("1st Stage F-Stat", 
                lfe::waldtest(iv_cd_dmspols_ihs$stage1, ~near_mst_lc, lhs=iv_cd_dmspols_ihs$stage1$lhs)[5] %>% round(ROUND_NUM),
                lfe::waldtest(iv_ld_dmspols_ihs$stage1, ~near_mst_euc, lhs=iv_ld_dmspols_ihs$stage1$lhs)[5] %>% round(ROUND_NUM)
              )
            ),
            out = file.path(paper_tables, 
                            paste0("iv_near_mst_5km_1ststage_",DATASET_TYPE,"_results_rsdp123.tex")))
  
  
  # First Stage: Regions - Stargazer -------------------------------------------
  stargazer(iv_cd_dmspols_ihs_regions$stage1,
            iv_ld_dmspols_ihs_regions$stage1,
            dep.var.labels.include = T,
            dep.var.labels = c("Near Improved Road"),
            dep.var.caption = "",
            covariate.labels = c("Near Least Cost MST",
                                 "Near Min. Distance MST"),
            omit = "distance_rsdp123_targettedlocs_log",
            omit.stat = c("f","ser", "rsq"),
            align=TRUE,
            no.space=TRUE,
            float=FALSE,
            column.sep.width="-15pt",
            digits=2,
            omit.table.layout = "n",
            add.lines = list(
              c("1st Stage F-Stat", 
                lfe::waldtest(iv_cd_dmspols_ihs_regions$stage1, ~near_mst_lc_region, lhs=iv_cd_dmspols_ihs_regions$stage1$lhs)[5] %>% round(ROUND_NUM),
                lfe::waldtest(iv_ld_dmspols_ihs_regions$stage1, ~near_mst_euc_region, lhs=iv_ld_dmspols_ihs_regions$stage1$lhs)[5] %>% round(ROUND_NUM)
              )
            ),
            out = file.path(paper_tables, 
                            paste0("iv_near_mst_5km_1ststage_",DATASET_TYPE,"_results_rsdp123_regions.tex")))
  
}

