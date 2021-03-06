# Instrumental Variables

# Resources for IV in R
# https://rpubs.com/wsundstrom/t_ivreg
# http://eclr.humanities.manchester.ac.uk/index.php/IV_in_R

NEAR_TARGETTED_LOCATION <- 5000
RM_DISTANE_ADDIS <- 0 # (100km scale; so 1 = 100km)
NEAR_ROAD <- 5000

ROUND_NUM <- 1 # number of digits to round numbers

DATASET_TYPE <- "kebele" 
rsdp_type    <- "rsdp123"

prep_data <- function(DATASET_TYPE, rsdp_type){
  
  # Load Data ----------------------------------------------------------------
  if(rsdp_type %in% "rsdp123")  rsdp_end_year <- 2009
  if(rsdp_type %in% "rsdp1234") rsdp_end_year <- 2016
  
  df <- readRDS(file.path(panel_rsdp_imp_data_file_path, DATASET_TYPE, "merged_datasets", 
                          paste0("longdiff_data_clean_base", 
                                 1996,
                                 "_end",
                                 rsdp_end_year,
                                 ".Rds")))
  
  df_NROW_ORIGINAL <- nrow(df)
  
  # Parameters and Variables -------------------------------------------------
  #### Parameters
  #CLUSTER_VAR <- "woreda_id"
  df$cluster_var <- df$woreda_id
  
  
  
  #### Dep Vars
  if(DATASET_TYPE %in% "dmspols_grid_ethiopia"){
    df$dv_dmspols <- df$dmspols_harmon_ihs
    df$dv_gcurban <- df$globcover_urban
    df$dv_gccrop  <- df$globcover_cropland
    
    df$dv_dmspols_1996 <- df$dmspols_harmon_ihs_1996
    df$dv_gcurban_1996 <- df$globcover_urban_1996
    df$dv_gccrop_1996  <- df$globcover_cropland_1996
  }
  
  if(DATASET_TYPE %in% "kebele"){
    df$dv_dmspols <- df$dmspols_harmon_ihs
    df$dv_gcurban <- df$globcover_urban_sum_ihs
    df$dv_gccrop  <- df$globcover_cropland_sum_ihs
    
    df$dv_dmspols_1996 <- df$dmspols_harmon_ihs_1996
    df$dv_gcurban_1996 <- df$globcover_urban_sum_ihs_1996
    df$dv_gccrop_1996  <- df$globcover_cropland_sum_ihs_1996
  }
  
  #### Distance Addis/Targetted Locs
  df$distance_city_addisababa           <- df$distance_city_addisababa / 1000 / 100 # 100km scale
  df$distance_rsdp123_targettedlocs_log <- log(df$distance_rsdp123_targettedlocs + 1)
  
  #### Distance RSDP Vars
  if(rsdp_type %in% "rsdp1234"){
    # Override variables so uses rsdp1234 variables
    df$distance_rsdp123                <- df$distance_rsdp1234
    df$distance_rsdp123_mst_euc        <- df$distance_rsdp1234_mst_euc
    df$distance_rsdp123_mst_euc_region <- df$distance_rsdp1234_mst_euc_region
    df$distance_rsdp123_mst_lc         <- df$distance_rsdp1234_mst_lc
    df$distance_rsdp123_mst_lc_region  <- df$distance_rsdp1234_mst_lc_region
    
    df$distance_rsdp123_targettedlocs  <- df$distance_rsdp1234_targettedlocs
  }
  
  #### Prep Data
  df <- df %>%
    
    dplyr::filter(distance_rsdp123_targettedlocs > NEAR_TARGETTED_LOCATION,
                  distance_city_addisababa       > RM_DISTANE_ADDIS) %>%
    
    dplyr::mutate(near_rsdp123         = as.numeric(distance_rsdp123 <= NEAR_ROAD),
                  near_mst_euc         = as.numeric(distance_rsdp123_mst_euc        <= NEAR_ROAD),
                  near_mst_euc_region  = as.numeric(distance_rsdp123_mst_euc_region <= NEAR_ROAD),
                  near_mst_lc          = as.numeric(distance_rsdp123_mst_lc        <= NEAR_ROAD),
                  near_mst_lc_region   = as.numeric(distance_rsdp123_mst_lc_region <= NEAR_ROAD),
                  
                  near_rsdp123Xdmspols_1996_bin4_1      = near_rsdp123 * dmspols_harmon_1996_bin4_1,
                  near_rsdp123Xdmspols_1996_bin4_2      = near_rsdp123 * dmspols_harmon_1996_bin4_2,
                  near_rsdp123Xdmspols_1996_bin4_3      = near_rsdp123 * dmspols_harmon_1996_bin4_3,
                  near_rsdp123Xdmspols_1996_bin4_4      = near_rsdp123 * dmspols_harmon_1996_bin4_4,
                  near_rsdp123Xdistance_city_addisababa = near_rsdp123 * distance_city_addisababa,
                  
                  near_mst_eucXdmspols_1996_bin4_1      = near_mst_euc * dmspols_harmon_1996_bin4_1,
                  near_mst_eucXdmspols_1996_bin4_2      = near_mst_euc * dmspols_harmon_1996_bin4_2,
                  near_mst_eucXdmspols_1996_bin4_3      = near_mst_euc * dmspols_harmon_1996_bin4_3,
                  near_mst_eucXdmspols_1996_bin4_4      = near_mst_euc * dmspols_harmon_1996_bin4_4,
                  near_mst_eucXdistance_city_addisababa = near_mst_euc * distance_city_addisababa,
                  
                  near_mst_euc_regionXdmspols_1996_bin4_1      = near_mst_euc_region * dmspols_harmon_1996_bin4_1,
                  near_mst_euc_regionXdmspols_1996_bin4_2      = near_mst_euc_region * dmspols_harmon_1996_bin4_2,
                  near_mst_euc_regionXdmspols_1996_bin4_3      = near_mst_euc_region * dmspols_harmon_1996_bin4_3,
                  near_mst_euc_regionXdmspols_1996_bin4_4      = near_mst_euc_region * dmspols_harmon_1996_bin4_4,
                  near_mst_euc_regionXdistance_city_addisababa = near_mst_euc_region * distance_city_addisababa,
                  
                  near_mst_lcXdmspols_1996_bin4_1      = near_mst_lc * dmspols_harmon_1996_bin4_1,
                  near_mst_lcXdmspols_1996_bin4_2      = near_mst_lc * dmspols_harmon_1996_bin4_2,
                  near_mst_lcXdmspols_1996_bin4_3      = near_mst_lc * dmspols_harmon_1996_bin4_3,
                  near_mst_lcXdmspols_1996_bin4_4      = near_mst_lc * dmspols_harmon_1996_bin4_4,
                  near_mst_lcXdistance_city_addisababa = near_mst_lc * distance_city_addisababa,
                  
                  near_mst_lc_regionXdmspols_1996_bin4_1      = near_mst_lc_region * dmspols_harmon_1996_bin4_1,
                  near_mst_lc_regionXdmspols_1996_bin4_2      = near_mst_lc_region * dmspols_harmon_1996_bin4_2,
                  near_mst_lc_regionXdmspols_1996_bin4_3      = near_mst_lc_region * dmspols_harmon_1996_bin4_3,
                  near_mst_lc_regionXdmspols_1996_bin4_4      = near_mst_lc_region * dmspols_harmon_1996_bin4_4,
                  near_mst_lc_regionXdistance_city_addisababa = near_mst_lc_region * distance_city_addisababa)
  
  # Summary Stats ----------------------------------------------------------------
  # df_neartc <- df %>%
  #   mutate(N_units = 1) %>%
  #   group_by(near_rsdp123) %>%
  #   dplyr::summarise(N_units = sum(N_units))
  # 
  # df_mean <- df %>%
  #   group_by(near_rsdp123) %>%
  #   dplyr::summarise_at(vars(c(dv_dmspols,
  #                              dv_gcurban,
  #                              dv_gccrop,
  #                              dv_dmspols_1996,
  #                              dv_gcurban_1996,
  #                              dv_gccrop_1996)),
  #                       mean, na.rm = T)
  # 
  # df_sum_nonzero <- df %>%
  #   mutate(dv_dmspols_1996_g0 = as.numeric(dv_dmspols_1996 > 0),
  #          dv_gcurban_1996_g0 = as.numeric(dv_gcurban_1996 > 0),
  #          dv_gccrop_1996_g0 = as.numeric(dv_gccrop_1996 > 0)) %>%
  #   dplyr::group_by(near_rsdp123) %>%
  #   dplyr::summarise_at(vars(c(dv_dmspols_1996_g0,
  #                              dv_gcurban_1996_g0,
  #                              dv_gccrop_1996_g0)),
  #                       sum, na.rm = T)
  # 
  # df_tc_coll <- df_mean %>%
  #   left_join(df_sum_nonzero, by = "near_rsdp123") %>%
  #   pivot_longer(cols = -c(near_rsdp123)) 
  # 
  # df_tc_coll$type <- "mean_change"
  # df_tc_coll$type[grepl("_1996$", df_tc_coll$name)] <- "mean_1996"
  # df_tc_coll$type[grepl("_1996_g0$", df_tc_coll$name)] <- "g0_1996"
  # 
  # df_tc_coll$name <- df_tc_coll$name %>% str_replace_all("_1996_g0", "") %>% str_replace_all("_1996", "")
  # 
  # make_sum_stat <- function(dvname, df_tc_coll){
  #   df_dv_i <- df_tc_coll[df_tc_coll$name %in% dvname,]
  #   
  #   paste(
  #     df_dv_i$value[(df_dv_i$near_rsdp123 %in% T) & (df_dv_i$type %in% "mean_change")] %>% round(4),
  #     df_dv_i$value[(df_dv_i$near_rsdp123 %in% F) & (df_dv_i$type %in% "mean_change")] %>% round(4),
  #     
  #     df_dv_i$value[(df_dv_i$near_rsdp123 %in% T) & (df_dv_i$type %in% "mean_1996")] %>% round(4),
  #     df_dv_i$value[(df_dv_i$near_rsdp123 %in% F) & (df_dv_i$type %in% "mean_1996")] %>% round(4),
  #     
  #     df_dv_i$value[(df_dv_i$near_rsdp123 %in% T) & (df_dv_i$type %in% "g0_1996")] %>% round(3),
  #     df_dv_i$value[(df_dv_i$near_rsdp123 %in% F) & (df_dv_i$type %in% "g0_1996")] %>% round(3),
  #     sep = " & "
  #   )
  #   
  # }
  # 
  # sink(file.path(paper_tables,
  #                paste0("iv_",rsdp_type,"_sumstat_",DATASET_TYPE,".tex")))
  # 
  # cat("\\begin{tabular}{l | cc | cc | cc} ")
  # cat("\\hline ")
  # 
  # cat("Variable & \\multicolumn{2}{c|}{Mean Change} & \\multicolumn{2}{c|}{Mean, 1996} & \\multicolumn{2}{c}{N Units $>0$, 1996} \\\\ ")
  # #cat("\\hline ")
  # cat(" & Treated & Control & Treated & Control & Treated & Control \\\\ ")
  # cat("\\hline ")
  # 
  # cat("Avg. NTL & ")
  # cat(make_sum_stat("dv_dmspols", df_tc_coll))
  # cat("\\\\ ")
  # 
  # cat("Prop. Urban & ")
  # cat(make_sum_stat("dv_gcurban", df_tc_coll))
  # cat("\\\\ ")
  # 
  # cat("Prop. Cropland & ")
  # cat(make_sum_stat("dv_gccrop", df_tc_coll))
  # cat("\\\\ ")
  # cat("\\hline ")
  # cat("\\multicolumn{7}{l}{N Units = ",
  #     nrow(df) %>% prettyNum(big.mark=",",scientific=FALSE),
  #     "; N Treated = ", df_neartc$N_units[df_neartc$near_rsdp123 %in% 1] %>% prettyNum(big.mark=",",scientific=FALSE),
  #     "; N Control = ", df_neartc$N_units[df_neartc$near_rsdp123 %in% 0] %>% prettyNum(big.mark=",",scientific=FALSE),
  #     "} ", sep = "")
  # 
  # cat("\\end{tabular} ")
  # 
  # sink()
  # 
  # # Grab Stats as Dataframe ---------------------------------------------------
  # summary_stats_df <- data.frame(n_original = df_NROW_ORIGINAL,
  #                                n = nrow(df),
  #                                n_treated = sum(df$near_rsdp123 %in% 1),
  #                                n_control = sum(df$near_rsdp123 %in% 0),
  #                                
  #                                near_mst_euc_1 = sum(df$near_mst_euc %in% 1),
  #                                near_mst_euc_0 = sum(df$near_mst_euc %in% 0),
  #                                
  #                                near_mst_euc_region_1 = sum(df$near_mst_euc_region %in% 1),
  #                                near_mst_euc_region_0 = sum(df$near_mst_euc_region %in% 0),
  #                                
  #                                near_mst_lc_1 = sum(df$near_mst_lc %in% 1),
  #                                near_mst_lc_0 = sum(df$near_mst_lc %in% 0),
  #                                
  #                                near_mst_lc_region_1 = sum(df$near_mst_lc_region %in% 1),
  #                                near_mst_lc_region_0 = sum(df$near_mst_lc_region %in% 0)) %>%
  #   mutate(dataset = DATASET_TYPE,
  #          rsdp = rsdp_type)
  # 
  # saveRDS(summary_stats_df,
  #         file.path(project_file_path, "Data", "Panel Data RSDP Impacts",
  #                   "Data", DATASET_TYPE, "results_datasets", 
  #                   paste0("iv_",rsdp_type,"_summary_stats.Rds")))
  # ---
  return(df)
}

#for(DATASET_TYPE in c("dmspols_grid_ethiopia", "kebele")){
DATASET_TYPE <- "1kmkebele"
for(rsdp_type in c("rsdp123", "rsdp1234")){
  for(rmrsdp1234targeted in c(T, F)){
    
    df_grid   <- prep_data("dmspols_grid_ethiopia", rsdp_type)
    df_kebele <- prep_data("kebele", rsdp_type)
    
    if(rmrsdp1234targeted){
      df_grid <- df_grid[df_grid$distance_rsdp1234_targettedlocs >= 5000,]
      df_kebele <- df_kebele[df_kebele$distance_rsdp1234_targettedlocs >= 5000,]
    }
    
    # OLS --------------------------------------------------------------------------
    # Need to do as.formula %>% felm(), as if do felm(as.formula), then dep var heading 
    # in stargazer is the full formula. Piping formula in solves.
    
    ## Grid
    lm_dmspols_ihs_g     <- as.formula(paste("dv_dmspols ~ near_rsdp123 + distance_rsdp123_targettedlocs_log | 0 | 0 | ", "cluster_var")) %>% felm(data = df_grid)
    lm_globcover_urban_g <- as.formula(paste("dv_gcurban ~ near_rsdp123 + distance_rsdp123_targettedlocs_log | 0 | 0 | ", "cluster_var")) %>% felm(data = df_grid)
    lm_globcover_crop_g  <- as.formula(paste("dv_gccrop  ~ near_rsdp123 + distance_rsdp123_targettedlocs_log | 0 | 0 | ", "cluster_var")) %>% felm(data = df_grid)
    
    lm_dmspols_ihs_basentl_g     <- as.formula(paste("dv_dmspols ~ near_rsdp123 + distance_rsdp123_targettedlocs_log + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4 | 0 | 0 | ", "cluster_var")) %>% felm(data = df_grid)
    lm_globcover_urban_basentl_g <- as.formula(paste("dv_gcurban ~ near_rsdp123 + distance_rsdp123_targettedlocs_log + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4 | 0 | 0 | ", "cluster_var")) %>% felm(data = df_grid)
    lm_globcover_crop_basentl_g  <- as.formula(paste("dv_gccrop  ~ near_rsdp123 + distance_rsdp123_targettedlocs_log + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4 | 0 | 0 | ", "cluster_var")) %>% felm(data = df_grid)
    
    ## Kebele
    lm_dmspols_ihs_k     <- as.formula(paste("dv_dmspols ~ near_rsdp123 + distance_rsdp123_targettedlocs_log | 0 | 0 | ", "cluster_var")) %>% felm(data = df_kebele)
    lm_globcover_urban_k <- as.formula(paste("dv_gcurban ~ near_rsdp123 + distance_rsdp123_targettedlocs_log | 0 | 0 | ", "cluster_var")) %>% felm(data = df_kebele)
    lm_globcover_crop_k  <- as.formula(paste("dv_gccrop  ~ near_rsdp123 + distance_rsdp123_targettedlocs_log | 0 | 0 | ", "cluster_var")) %>% felm(data = df_kebele)
    
    lm_dmspols_ihs_basentl_k     <- as.formula(paste("dv_dmspols ~ near_rsdp123 + distance_rsdp123_targettedlocs_log + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4 | 0 | 0 | ", "cluster_var")) %>% felm(data = df_kebele)
    lm_globcover_urban_basentl_k <- as.formula(paste("dv_gcurban ~ near_rsdp123 + distance_rsdp123_targettedlocs_log + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4 | 0 | 0 | ", "cluster_var")) %>% felm(data = df_kebele)
    lm_globcover_crop_basentl_k  <- as.formula(paste("dv_gccrop  ~ near_rsdp123 + distance_rsdp123_targettedlocs_log + near_rsdp123Xdmspols_1996_bin4_2 + near_rsdp123Xdmspols_1996_bin4_3 + near_rsdp123Xdmspols_1996_bin4_4 | 0 | 0 | ", "cluster_var")) %>% felm(data = df_kebele)
    
    # MST - Cost Distance --------------------------------------------------------
    ## Grid
    iv_cd_dmspols_ihs_g     <- as.formula(paste("dv_dmspols ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123 ~ near_mst_lc) | ", "cluster_var")) %>% felm(data = df_grid)
    iv_cd_globcover_urban_g <- as.formula(paste("dv_gcurban ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123 ~ near_mst_lc) | ", "cluster_var")) %>% felm(data = df_grid)
    iv_cd_globcover_crop_g  <- as.formula(paste("dv_gccrop  ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123 ~ near_mst_lc) | ", "cluster_var")) %>% felm(data = df_grid)
    
    iv_cd_dmspols_ihs_basentl_g     <- as.formula(paste("dv_dmspols ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdmspols_1996_bin4_2|near_rsdp123Xdmspols_1996_bin4_3|near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_lc + near_mst_lcXdmspols_1996_bin4_2 + near_mst_lcXdmspols_1996_bin4_3 + near_mst_lcXdmspols_1996_bin4_4) | ", "cluster_var")) %>% felm(data = df_grid)
    iv_cd_globcover_urban_basentl_g <- as.formula(paste("dv_gcurban ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdmspols_1996_bin4_2|near_rsdp123Xdmspols_1996_bin4_3|near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_lc + near_mst_lcXdmspols_1996_bin4_2 + near_mst_lcXdmspols_1996_bin4_3 + near_mst_lcXdmspols_1996_bin4_4) | ", "cluster_var")) %>% felm(data = df_grid)
    iv_cd_globcover_crop_basentl_g  <- as.formula(paste("dv_gccrop  ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdmspols_1996_bin4_2|near_rsdp123Xdmspols_1996_bin4_3|near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_lc + near_mst_lcXdmspols_1996_bin4_2 + near_mst_lcXdmspols_1996_bin4_3 + near_mst_lcXdmspols_1996_bin4_4) | ", "cluster_var")) %>% felm(data = df_grid)
    
    ## Kebele
    iv_cd_dmspols_ihs_k     <- as.formula(paste("dv_dmspols ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123 ~ near_mst_lc) | ", "cluster_var")) %>% felm(data = df_kebele)
    iv_cd_globcover_urban_k <- as.formula(paste("dv_gcurban ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123 ~ near_mst_lc) | ", "cluster_var")) %>% felm(data = df_kebele)
    iv_cd_globcover_crop_k  <- as.formula(paste("dv_gccrop  ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123 ~ near_mst_lc) | ", "cluster_var")) %>% felm(data = df_kebele)
    
    iv_cd_dmspols_ihs_basentl_k     <- as.formula(paste("dv_dmspols ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdmspols_1996_bin4_2|near_rsdp123Xdmspols_1996_bin4_3|near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_lc + near_mst_lcXdmspols_1996_bin4_2 + near_mst_lcXdmspols_1996_bin4_3 + near_mst_lcXdmspols_1996_bin4_4) | ", "cluster_var")) %>% felm(data = df_kebele)
    iv_cd_globcover_urban_basentl_k <- as.formula(paste("dv_gcurban ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdmspols_1996_bin4_2|near_rsdp123Xdmspols_1996_bin4_3|near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_lc + near_mst_lcXdmspols_1996_bin4_2 + near_mst_lcXdmspols_1996_bin4_3 + near_mst_lcXdmspols_1996_bin4_4) | ", "cluster_var")) %>% felm(data = df_kebele)
    iv_cd_globcover_crop_basentl_k  <- as.formula(paste("dv_gccrop  ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdmspols_1996_bin4_2|near_rsdp123Xdmspols_1996_bin4_3|near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_lc + near_mst_lcXdmspols_1996_bin4_2 + near_mst_lcXdmspols_1996_bin4_3 + near_mst_lcXdmspols_1996_bin4_4) | ", "cluster_var")) %>% felm(data = df_kebele)
    
    # MST - Least Distance -------------------------------------------------------
    ## Grid
    iv_ld_dmspols_ihs_g     <- as.formula(paste("dv_dmspols ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123 ~ near_mst_euc) | ", "cluster_var")) %>% felm(data = df_grid)
    iv_ld_globcover_urban_g <- as.formula(paste("dv_gcurban ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123 ~ near_mst_euc) | ", "cluster_var")) %>% felm(data = df_grid)
    iv_ld_globcover_crop_g  <- as.formula(paste("dv_gccrop  ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123 ~ near_mst_euc) | ", "cluster_var")) %>% felm(data = df_grid)
    
    iv_ld_dmspols_ihs_basentl_g     <- as.formula(paste("dv_dmspols ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdmspols_1996_bin4_2|near_rsdp123Xdmspols_1996_bin4_3|near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_euc + near_mst_eucXdmspols_1996_bin4_2 + near_mst_eucXdmspols_1996_bin4_3 + near_mst_eucXdmspols_1996_bin4_4) | ", "cluster_var")) %>% felm(data = df_grid)
    iv_ld_globcover_urban_basentl_g <- as.formula(paste("dv_gcurban ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdmspols_1996_bin4_2|near_rsdp123Xdmspols_1996_bin4_3|near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_euc + near_mst_eucXdmspols_1996_bin4_2 + near_mst_eucXdmspols_1996_bin4_3 + near_mst_eucXdmspols_1996_bin4_4) | ", "cluster_var")) %>% felm(data = df_grid)
    iv_ld_globcover_crop_basentl_g  <- as.formula(paste("dv_gccrop  ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdmspols_1996_bin4_2|near_rsdp123Xdmspols_1996_bin4_3|near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_euc + near_mst_eucXdmspols_1996_bin4_2 + near_mst_eucXdmspols_1996_bin4_3 + near_mst_eucXdmspols_1996_bin4_4) | ", "cluster_var")) %>% felm(data = df_grid)
    
    ## Kebele
    iv_ld_dmspols_ihs_k     <- as.formula(paste("dv_dmspols ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123 ~ near_mst_euc) | ", "cluster_var")) %>% felm(data = df_kebele)
    iv_ld_globcover_urban_k <- as.formula(paste("dv_gcurban ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123 ~ near_mst_euc) | ", "cluster_var")) %>% felm(data = df_kebele)
    iv_ld_globcover_crop_k  <- as.formula(paste("dv_gccrop  ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123 ~ near_mst_euc) | ", "cluster_var")) %>% felm(data = df_kebele)
    
    iv_ld_dmspols_ihs_basentl_k     <- as.formula(paste("dv_dmspols ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdmspols_1996_bin4_2|near_rsdp123Xdmspols_1996_bin4_3|near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_euc + near_mst_eucXdmspols_1996_bin4_2 + near_mst_eucXdmspols_1996_bin4_3 + near_mst_eucXdmspols_1996_bin4_4) | ", "cluster_var")) %>% felm(data = df_kebele)
    iv_ld_globcover_urban_basentl_k <- as.formula(paste("dv_gcurban ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdmspols_1996_bin4_2|near_rsdp123Xdmspols_1996_bin4_3|near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_euc + near_mst_eucXdmspols_1996_bin4_2 + near_mst_eucXdmspols_1996_bin4_3 + near_mst_eucXdmspols_1996_bin4_4) | ", "cluster_var")) %>% felm(data = df_kebele)
    iv_ld_globcover_crop_basentl_k  <- as.formula(paste("dv_gccrop  ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdmspols_1996_bin4_2|near_rsdp123Xdmspols_1996_bin4_3|near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_euc + near_mst_eucXdmspols_1996_bin4_2 + near_mst_eucXdmspols_1996_bin4_3 + near_mst_eucXdmspols_1996_bin4_4) | ", "cluster_var")) %>% felm(data = df_kebele)
    
    # MST Regions - Cost Distance ------------------------------------------------
    ## Grid
    iv_cd_dmspols_ihs_regions_g     <- as.formula(paste("dv_dmspols ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123 ~ near_mst_lc_region) | ", "cluster_var")) %>% felm(data = df_grid)
    iv_cd_globcover_urban_regions_g <- as.formula(paste("dv_gcurban ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123 ~ near_mst_lc_region) | ", "cluster_var")) %>% felm(data = df_grid)
    iv_cd_globcover_crop_regions_g  <- as.formula(paste("dv_gccrop  ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123 ~ near_mst_lc_region) | ", "cluster_var")) %>% felm(data = df_grid)
    
    iv_cd_dmspols_ihs_basentl_regions_g     <- as.formula(paste("dv_dmspols ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdmspols_1996_bin4_2|near_rsdp123Xdmspols_1996_bin4_3|near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_lc_region + near_mst_lc_regionXdmspols_1996_bin4_2 + near_mst_lc_regionXdmspols_1996_bin4_3 + near_mst_lc_regionXdmspols_1996_bin4_4) | ", "cluster_var")) %>% felm(data = df_grid)
    iv_cd_globcover_urban_basentl_regions_g <- as.formula(paste("dv_gcurban ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdmspols_1996_bin4_2|near_rsdp123Xdmspols_1996_bin4_3|near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_lc_region + near_mst_lc_regionXdmspols_1996_bin4_2 + near_mst_lc_regionXdmspols_1996_bin4_3 + near_mst_lc_regionXdmspols_1996_bin4_4) | ", "cluster_var")) %>% felm(data = df_grid)
    iv_cd_globcover_crop_basentl_regions_g  <- as.formula(paste("dv_gccrop  ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdmspols_1996_bin4_2|near_rsdp123Xdmspols_1996_bin4_3|near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_lc_region + near_mst_lc_regionXdmspols_1996_bin4_2 + near_mst_lc_regionXdmspols_1996_bin4_3 + near_mst_lc_regionXdmspols_1996_bin4_4) | ", "cluster_var")) %>% felm(data = df_grid)
    
    ## Kebele
    iv_cd_dmspols_ihs_regions_k     <- as.formula(paste("dv_dmspols ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123 ~ near_mst_lc_region) | ", "cluster_var")) %>% felm(data = df_kebele)
    iv_cd_globcover_urban_regions_k <- as.formula(paste("dv_gcurban ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123 ~ near_mst_lc_region) | ", "cluster_var")) %>% felm(data = df_kebele)
    iv_cd_globcover_crop_regions_k  <- as.formula(paste("dv_gccrop  ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123 ~ near_mst_lc_region) | ", "cluster_var")) %>% felm(data = df_kebele)
    
    iv_cd_dmspols_ihs_basentl_regions_k     <- as.formula(paste("dv_dmspols ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdmspols_1996_bin4_2|near_rsdp123Xdmspols_1996_bin4_3|near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_lc_region + near_mst_lc_regionXdmspols_1996_bin4_2 + near_mst_lc_regionXdmspols_1996_bin4_3 + near_mst_lc_regionXdmspols_1996_bin4_4) | ", "cluster_var")) %>% felm(data = df_kebele)
    iv_cd_globcover_urban_basentl_regions_k <- as.formula(paste("dv_gcurban ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdmspols_1996_bin4_2|near_rsdp123Xdmspols_1996_bin4_3|near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_lc_region + near_mst_lc_regionXdmspols_1996_bin4_2 + near_mst_lc_regionXdmspols_1996_bin4_3 + near_mst_lc_regionXdmspols_1996_bin4_4) | ", "cluster_var")) %>% felm(data = df_kebele)
    iv_cd_globcover_crop_basentl_regions_k  <- as.formula(paste("dv_gccrop  ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdmspols_1996_bin4_2|near_rsdp123Xdmspols_1996_bin4_3|near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_lc_region + near_mst_lc_regionXdmspols_1996_bin4_2 + near_mst_lc_regionXdmspols_1996_bin4_3 + near_mst_lc_regionXdmspols_1996_bin4_4) | ", "cluster_var")) %>% felm(data = df_kebele)
    
    # MST Regions - Least Distance -------------------------------------------------------
    ## Grid
    iv_ld_dmspols_ihs_regions_g     <- as.formula(paste("dv_dmspols ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123 ~ near_mst_euc_region) | ", "cluster_var")) %>% felm(data = df_grid)
    iv_ld_globcover_urban_regions_g <- as.formula(paste("dv_gcurban ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123 ~ near_mst_euc_region) | ", "cluster_var")) %>% felm(data = df_grid)
    iv_ld_globcover_crop_regions_g  <- as.formula(paste("dv_gccrop  ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123 ~ near_mst_euc_region) | ", "cluster_var")) %>% felm(data = df_grid)
    
    iv_ld_dmspols_ihs_basentl_regions_g     <- as.formula(paste("dv_dmspols ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdmspols_1996_bin4_2|near_rsdp123Xdmspols_1996_bin4_3|near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_euc_region + near_mst_euc_regionXdmspols_1996_bin4_2 + near_mst_euc_regionXdmspols_1996_bin4_3 + near_mst_euc_regionXdmspols_1996_bin4_4) | ", "cluster_var")) %>% felm(data = df_grid)
    iv_ld_globcover_urban_basentl_regions_g <- as.formula(paste("dv_gcurban ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdmspols_1996_bin4_2|near_rsdp123Xdmspols_1996_bin4_3|near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_euc_region + near_mst_euc_regionXdmspols_1996_bin4_2 + near_mst_euc_regionXdmspols_1996_bin4_3 + near_mst_euc_regionXdmspols_1996_bin4_4) | ", "cluster_var")) %>% felm(data = df_grid)
    iv_ld_globcover_crop_basentl_regions_g  <- as.formula(paste("dv_gccrop  ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdmspols_1996_bin4_2|near_rsdp123Xdmspols_1996_bin4_3|near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_euc_region + near_mst_euc_regionXdmspols_1996_bin4_2 + near_mst_euc_regionXdmspols_1996_bin4_3 + near_mst_euc_regionXdmspols_1996_bin4_4) | ", "cluster_var")) %>% felm(data = df_grid)
    
    ## Kebele
    iv_ld_dmspols_ihs_regions_k     <- as.formula(paste("dv_dmspols ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123 ~ near_mst_euc_region) | ", "cluster_var")) %>% felm(data = df_kebele)
    iv_ld_globcover_urban_regions_k <- as.formula(paste("dv_gcurban ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123 ~ near_mst_euc_region) | ", "cluster_var")) %>% felm(data = df_kebele)
    iv_ld_globcover_crop_regions_k  <- as.formula(paste("dv_gccrop  ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123 ~ near_mst_euc_region) | ", "cluster_var")) %>% felm(data = df_kebele)
    
    iv_ld_dmspols_ihs_basentl_regions_k     <- as.formula(paste("dv_dmspols ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdmspols_1996_bin4_2|near_rsdp123Xdmspols_1996_bin4_3|near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_euc_region + near_mst_euc_regionXdmspols_1996_bin4_2 + near_mst_euc_regionXdmspols_1996_bin4_3 + near_mst_euc_regionXdmspols_1996_bin4_4) | ", "cluster_var")) %>% felm(data = df_kebele)
    iv_ld_globcover_urban_basentl_regions_k <- as.formula(paste("dv_gcurban ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdmspols_1996_bin4_2|near_rsdp123Xdmspols_1996_bin4_3|near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_euc_region + near_mst_euc_regionXdmspols_1996_bin4_2 + near_mst_euc_regionXdmspols_1996_bin4_3 + near_mst_euc_regionXdmspols_1996_bin4_4) | ", "cluster_var")) %>% felm(data = df_kebele)
    iv_ld_globcover_crop_basentl_regions_k  <- as.formula(paste("dv_gccrop  ~ distance_rsdp123_targettedlocs_log | 0 | (near_rsdp123|near_rsdp123Xdmspols_1996_bin4_2|near_rsdp123Xdmspols_1996_bin4_3|near_rsdp123Xdmspols_1996_bin4_4 ~ near_mst_euc_region + near_mst_euc_regionXdmspols_1996_bin4_2 + near_mst_euc_regionXdmspols_1996_bin4_3 + near_mst_euc_regionXdmspols_1996_bin4_4) | ", "cluster_var")) %>% felm(data = df_kebele)
    
    # OLS - Stargazer --------------------------------------------------------------
    make_stargzer <- function(lm_dmspols_ihs_g,
                              lm_dmspols_ihs_basentl_g,
                              lm_globcover_urban_g,
                              lm_globcover_urban_basentl_g,
                              lm_globcover_crop_g,
                              lm_globcover_crop_basentl_g,
                              
                              lm_dmspols_ihs_k,
                              lm_dmspols_ihs_basentl_k,
                              lm_globcover_urban_k,
                              lm_globcover_urban_basentl_k,
                              lm_globcover_crop_k,
                              lm_globcover_crop_basentl_k,
                              iv_model,
                              file_name){
      
      ROUND_NUM = 2
      
      if(iv_model %in% T){
        add_lines <- list(
          c("Unit", rep("1km", 6), rep("Keb.", 6)),
          c("1st Stage F-Stat",
            lm_dmspols_ihs_g$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM),
            lm_dmspols_ihs_basentl_g$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM),
            lm_globcover_urban_g$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM),
            lm_globcover_urban_basentl_g$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM),
            lm_globcover_crop_g$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM),
            lm_globcover_crop_basentl_g$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM),
            
            lm_dmspols_ihs_k$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM),
            lm_dmspols_ihs_basentl_k$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM),
            lm_globcover_urban_k$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM),
            lm_globcover_urban_basentl_k$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM),
            lm_globcover_crop_k$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM),
            lm_globcover_crop_basentl_k$stage1$iv1fstat[[1]][5] %>% round(ROUND_NUM))
        )
        
      } else{
        add_lines <- list(
          c("Unit", rep("1km", 6), rep("Keb.", 6))
        )
      }

      stargazer(lm_dmspols_ihs_g,
                lm_dmspols_ihs_basentl_g,
                lm_globcover_urban_g,
                lm_globcover_urban_basentl_g,
                lm_globcover_crop_g,
                lm_globcover_crop_basentl_g,
                
                lm_dmspols_ihs_k,
                lm_dmspols_ihs_basentl_k,
                lm_globcover_urban_k,
                lm_globcover_urban_basentl_k,
                lm_globcover_crop_k,
                lm_globcover_crop_basentl_k,
                
                dep.var.labels.include = T,
                dep.var.labels = c("NTL", "Urban", "Cropland",
                                   "NTL", "Urban", "Cropland"), #  "NTL $\\geq$ 2", "NTL $\\geq$ 6",
                dep.var.caption = "",
                omit = c("temp_avg", "precipitation", "distance_rsdp123_targettedlocs_log"),
                covariate.labels = c("Imp Rd.",
                                     "Imp Rd.$\\times NTL_{96}$ Low",
                                     "Imp Rd.$\\times NTL_{96}$ Med",
                                     "Imp Rd.$\\times NTL_{96}$ High"),
                omit.stat = c("f","ser", "rsq"),
                align=TRUE,
                no.space=TRUE,
                float=FALSE,
                column.sep.width="-15pt",
                digits=2,
                omit.table.layout = "n",
                add.lines = add_lines,
                out = file.path(paper_tables, 
                                file_name))
      
    }
    
    # OLS ------------------------------------------------------------------------
    make_stargzer(lm_dmspols_ihs_g,
                  lm_dmspols_ihs_basentl_g,
                  lm_globcover_urban_g,
                  lm_globcover_urban_basentl_g,
                  lm_globcover_crop_g,
                  lm_globcover_crop_basentl_g,
                  
                  lm_dmspols_ihs_k,
                  lm_dmspols_ihs_basentl_k,
                  lm_globcover_urban_k,
                  lm_globcover_urban_basentl_k,
                  lm_globcover_crop_k,
                  lm_globcover_crop_basentl_k,
                  F,
                  paste0("ols_near_road_5km_",DATASET_TYPE,"_results_",rsdp_type,"_rmrsdp1234targeted",rmrsdp1234targeted,".tex"))
    
    # MST - Cost Distance - Stargazer ----------------------------------------------
    make_stargzer(iv_cd_dmspols_ihs_g,
                  iv_cd_dmspols_ihs_basentl_g,
                  iv_cd_globcover_urban_g,
                  iv_cd_globcover_urban_basentl_g,
                  iv_cd_globcover_crop_g,
                  iv_cd_globcover_crop_basentl_g,
                  
                  iv_cd_dmspols_ihs_k,
                  iv_cd_dmspols_ihs_basentl_k,
                  iv_cd_globcover_urban_k,
                  iv_cd_globcover_urban_basentl_k,
                  iv_cd_globcover_crop_k,
                  iv_cd_globcover_crop_basentl_k,
                  T,
                  paste0("iv_near_mst_cost_distance_5km_",DATASET_TYPE,"_results_",rsdp_type,"_rmrsdp1234targeted",rmrsdp1234targeted,".tex"))
    
    # MST - Least Distance - Stargazer ---------------------------------------------
    make_stargzer(iv_ld_dmspols_ihs_g,
                  iv_ld_dmspols_ihs_basentl_g,
                  iv_ld_globcover_urban_g,
                  iv_ld_globcover_urban_basentl_g,
                  iv_ld_globcover_crop_g,
                  iv_ld_globcover_crop_basentl_g,
                  
                  iv_ld_dmspols_ihs_k,
                  iv_ld_dmspols_ihs_basentl_k,
                  iv_ld_globcover_urban_k,
                  iv_ld_globcover_urban_basentl_k,
                  iv_ld_globcover_crop_k,
                  iv_ld_globcover_crop_basentl_k,
                  T,
                  paste0("iv_near_mst_least_distance_5km_",DATASET_TYPE,"_results_",rsdp_type,"_rmrsdp1234targeted",rmrsdp1234targeted,".tex"))
    
    # MST Regions - Cost Distance - Stargazer ----------------------------------------------
    make_stargzer(iv_cd_dmspols_ihs_regions_g,
                  iv_cd_dmspols_ihs_basentl_regions_g,
                  iv_cd_globcover_urban_regions_g,
                  iv_cd_globcover_urban_basentl_regions_g,
                  iv_cd_globcover_crop_regions_g,
                  iv_cd_globcover_crop_basentl_regions_g,
                  
                  iv_cd_dmspols_ihs_regions_k,
                  iv_cd_dmspols_ihs_basentl_regions_k,
                  iv_cd_globcover_urban_regions_k,
                  iv_cd_globcover_urban_basentl_regions_k,
                  iv_cd_globcover_crop_regions_k,
                  iv_cd_globcover_crop_basentl_regions_k,
                  T,
                  paste0("iv_near_mst_cost_distance_5km_",DATASET_TYPE,"_results_",rsdp_type,"_regions",rmrsdp1234targeted,".tex"))
    
    # MST Regions - Least Distance - Stargazer ---------------------------------------------
    make_stargzer(iv_ld_dmspols_ihs_regions_g,
                  iv_ld_dmspols_ihs_basentl_regions_g,
                  iv_ld_globcover_urban_regions_g,
                  iv_ld_globcover_urban_basentl_regions_g,
                  iv_ld_globcover_crop_regions_g,
                  iv_ld_globcover_crop_basentl_regions_g,
                  
                  iv_ld_dmspols_ihs_regions_k,
                  iv_ld_dmspols_ihs_basentl_regions_k,
                  iv_ld_globcover_urban_regions_k,
                  iv_ld_globcover_urban_basentl_regions_k,
                  iv_ld_globcover_crop_regions_k,
                  iv_ld_globcover_crop_basentl_regions_k,
                  T,
                  paste0("iv_near_mst_least_distance_5km_",DATASET_TYPE,"_results_",rsdp_type,"_regions_rmrsdp1234targeted",rmrsdp1234targeted,".tex"))
    
    # First Stage - Stargazer --------------------------------------
    stargazer(iv_cd_dmspols_ihs_g$stage1,
              iv_ld_dmspols_ihs_g$stage1,
              iv_cd_dmspols_ihs_regions_g$stage1,
              iv_ld_dmspols_ihs_regions_g$stage1,
              
              iv_cd_dmspols_ihs_k$stage1,
              iv_ld_dmspols_ihs_k$stage1,
              iv_cd_dmspols_ihs_regions_k$stage1,
              iv_ld_dmspols_ihs_regions_k$stage1,
              dep.var.labels.include = T,
              dep.var.labels = c("Near Improved Road"),
              dep.var.caption = "",
              covariate.labels = c("Near Least Cost MST",
                                   "Near Min. Distance MST",
                                   "Near Least Cost MST: Regional",
                                   "Near Min. Distance MST: Regional"),
              omit = "distance_rsdp123_targettedlocs_log",
              omit.stat = c("f","ser", "rsq"),
              align=TRUE,
              no.space=TRUE,
              float=FALSE,
              column.sep.width="-15pt",
              digits=2,
              omit.table.layout = "n",
              add.lines = list(
                c("Unit", rep("1km", 4), rep("Keb.", 4)),
                c("1st Stage F-Stat", 
                  lfe::waldtest(iv_cd_dmspols_ihs_g$stage1,         ~near_mst_lc,         lhs=iv_cd_dmspols_ihs_g$stage1$lhs)[5] %>% round(ROUND_NUM),
                  lfe::waldtest(iv_ld_dmspols_ihs_g$stage1,         ~near_mst_euc,        lhs=iv_ld_dmspols_ihs_g$stage1$lhs)[5] %>% round(ROUND_NUM),
                  lfe::waldtest(iv_cd_dmspols_ihs_regions_g$stage1, ~near_mst_lc_region,  lhs=iv_cd_dmspols_ihs_regions_g$stage1$lhs)[5] %>% round(ROUND_NUM),
                  lfe::waldtest(iv_ld_dmspols_ihs_regions_g$stage1, ~near_mst_euc_region, lhs=iv_ld_dmspols_ihs_regions_g$stage1$lhs)[5] %>% round(ROUND_NUM),
                  
                  lfe::waldtest(iv_cd_dmspols_ihs_k$stage1,         ~near_mst_lc,         lhs=iv_cd_dmspols_ihs_k$stage1$lhs)[5] %>% round(ROUND_NUM),
                  lfe::waldtest(iv_ld_dmspols_ihs_k$stage1,         ~near_mst_euc,        lhs=iv_ld_dmspols_ihs_k$stage1$lhs)[5] %>% round(ROUND_NUM),
                  lfe::waldtest(iv_cd_dmspols_ihs_regions_k$stage1, ~near_mst_lc_region,  lhs=iv_cd_dmspols_ihs_regions_k$stage1$lhs)[5] %>% round(ROUND_NUM),
                  lfe::waldtest(iv_ld_dmspols_ihs_regions_k$stage1, ~near_mst_euc_region, lhs=iv_ld_dmspols_ihs_regions_k$stage1$lhs)[5] %>% round(ROUND_NUM)
                )
              ),
              out = file.path(paper_tables, 
                              paste0("iv_near_mst_5km_1ststage_",DATASET_TYPE,"_results_",rsdp_type,"_rmrsdp1234targeted",rmrsdp1234targeted,".tex")))
    
    
  }
}
#}

