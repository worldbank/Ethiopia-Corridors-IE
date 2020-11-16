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

df2016$dmspols_zhang_ihs_base0na <- df2016$dmspols_zhang_ihs
df2016$dmspols_zhang_ihs_base0na[df2016$dmspols_zhang_1996 %in% 0] <- NA

# Estimate Models --------------------------------------------------------------
## 2012 as Endline
iv_dmspols_zhang_ihs  <- felm(dmspols_zhang_ihs  ~ 1 | 0 | (near_anyimproved_by2012 ~ near_mst_5km) | W_CODE, data = df2012)
iv_dmspols_zhang_2    <- felm(dmspols_zhang_2    ~ 1 | 0 | (near_anyimproved_by2012 ~ near_mst_5km) | W_CODE, data = df2012)
iv_dmspols_zhang_6    <- felm(dmspols_zhang_6    ~ 1 | 0 | (near_anyimproved_by2012 ~ near_mst_5km) | W_CODE, data = df2012)

## 2016 as Endline
iv_ndvi               <- felm(ndvi               ~ 1 | 0 | (near_anyimproved_ever_5km ~ near_mst_5km) | W_CODE, data = df2016)
iv_ndvi_cropland      <- felm(ndvi_cropland      ~ 1 | 0 | (near_anyimproved_ever_5km ~ near_mst_5km) | W_CODE, data = df2016)
iv_globcover_urban    <- felm(globcover_urban    ~ 1 | 0 | (near_anyimproved_ever_5km ~ near_mst_5km) | W_CODE, data = df2016)
iv_globcover_cropland <- felm(globcover_cropland ~ 1 | 0 | (near_anyimproved_ever_5km ~ near_mst_5km) | W_CODE, data = df2016)



waldtest(iv2$stage1, ~near_mst_5km, lhs=iv2$stage1$lhs)[5] # F-Stat






for(dep_var in c("dmspols_zhang_base0na","dmspols_zhang_ihs_base0na", "ndvi","ndvi_cropland", "globcover_urban", "globcover_cropland", "dmspols_zhang", "dmspols_zhang_ihs",  "dmspols_zhang_2", "dmspols_zhang_6", "dmspols", "dmspols_ihs")){
  



