# Instrumental Variables

# Resources for IV in R
# https://rpubs.com/wsundstrom/t_ivreg
# http://eclr.humanities.manchester.ac.uk/index.php/IV_in_R

# Load Data --------------------------------------------------------------------
# Dataframe using 2012 as endline
df2012 <- readRDS(file.path(panel_rsdp_imp_data_file_path, 
                            "dmspols_grid_ethiopia", 
                            "merged_datasets", 
                            paste0("longdiff_data_clean_base", 
                                   1996,
                                   "_end",
                                   2012,
                                   ".Rds")))

# Dataframe using 2016 as endline
df2016 <- readRDS(file.path(panel_rsdp_imp_data_file_path, 
                            "dmspols_grid_ethiopia", 
                            "merged_datasets", 
                            paste0("longdiff_data_clean_base", 
                                   1996,
                                   "_end",
                                   2016,
                                   ".Rds")))


df2012$near_mst_5km <- df2012$near_mst_5km %>% as.numeric()

iv2 <- felm(dmspols_zhang_ihs ~ 1 | 0 | (distance_anyimproved_by2012 ~ distance_mst) | W_CODE, data = df2012)
waldtest(iv2$stage1, ~distance_mst, lhs=iv2$stage1$lhs)[5] # F-Stat

iv2 <- felm(dmspols_zhang_ihs ~ 1 | 0 | (near_anyimproved_by2012_5km ~ near_mst_5km) | W_CODE, data = df2012)
waldtest(iv2$stage1, ~near_mst_5km, lhs=iv2$stage1$lhs)[5] # F-Stat


