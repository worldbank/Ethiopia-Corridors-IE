# Instrumental Variables

# Resources for IV in R
# https://rpubs.com/wsundstrom/t_ivreg
# http://eclr.humanities.manchester.ac.uk/index.php/IV_in_R
# MA_varXdmspols_zhang_sum2_ihs_1996

ROUND_NUM <- 1 # number of digits to round numbers

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


## NTL Baseline Bins
data <- data %>%
  group_by(cell_id) %>%
  dplyr::mutate(dmspols_sum2_1996 = dmspols_sum2[year== 1996],
                dmspols_sum6_1996 = dmspols_sum6[year== 1996]) %>%
  ungroup()

# Prep Variables ---------------------------------------------------------------

## Standardize improved variable names
# Do this so variable is the same row in the output table
df2012 <- df2012 %>%
  dplyr::rename(near_anyimproved_5km = near_anyimproved_by2012_5km)

df2016 <- df2016 %>%
  dplyr::rename(near_anyimproved_5km = near_anyimproved_ever_5km)

## Distance Addis - Units of 100km
df2012$distance_city_addisababa <- (df2012$distance_city_addisababa/1000)/100
df2016$distance_city_addisababa <- (df2016$distance_city_addisababa/1000)/100

## Interactions
df2012 <- df2012 %>%
  mutate(near_anyimproved_5kmXdistance_city_addisababa      = near_anyimproved_5km * distance_city_addisababa,
         near_anyimproved_5kmXdmspols_zhang_ihs_1996_woreda = near_anyimproved_5km * dmspols_zhang_ihs_1996_woreda,
         near_anyimproved_5kmXdmspols_zhang_ihs_sum2_1996_woreda = near_anyimproved_5km * dmspols_zhang_ihs_sum2_1996_woreda,
         
         near_mst_5kmXdistance_city_addisababa      = near_mst_5km * distance_city_addisababa,
         near_mst_5kmXdmspols_zhang_ihs_1996_woreda = near_mst_5km * dmspols_zhang_ihs_1996_woreda,
         near_mst_5kmXdmspols_zhang_ihs_sum2_1996_woreda = near_mst_5km * dmspols_zhang_ihs_sum2_1996_woreda,

         near_mst_mindist_5kmXdistance_city_addisababa      = near_mst_mindist_5km * distance_city_addisababa,
         near_mst_mindist_5kmXdmspols_zhang_ihs_1996_woreda = near_mst_mindist_5km * dmspols_zhang_ihs_1996_woreda,
         near_mst_mindist_5kmXdmspols_zhang_ihs_sum2_1996_woreda = near_mst_mindist_5km * dmspols_zhang_ihs_sum2_1996_woreda)

df2016 <- df2016 %>%
  mutate(near_anyimproved_5kmXdistance_city_addisababa      = near_anyimproved_5km * distance_city_addisababa,
         near_anyimproved_5kmXdmspols_zhang_ihs_1996_woreda = near_anyimproved_5km * dmspols_zhang_ihs_1996_woreda,
         near_anyimproved_5kmXdmspols_zhang_ihs_sum2_1996_woreda = near_anyimproved_5km * dmspols_zhang_ihs_sum2_1996_woreda,
         
         near_mst_5kmXdistance_city_addisababa      = near_mst_5km * distance_city_addisababa,
         near_mst_5kmXdmspols_zhang_ihs_1996_woreda = near_mst_5km * dmspols_zhang_ihs_1996_woreda,
         near_mst_5kmXdmspols_zhang_ihs_sum2_1996_woreda = near_mst_5km * dmspols_zhang_ihs_sum2_1996_woreda,
         
         near_mst_mindist_5kmXdistance_city_addisababa      = near_mst_mindist_5km * distance_city_addisababa,
         near_mst_mindist_5kmXdmspols_zhang_ihs_1996_woreda = near_mst_mindist_5km * dmspols_zhang_ihs_1996_woreda,
         near_mst_mindist_5kmXdmspols_zhang_ihs_sum2_1996_woreda = near_mst_mindist_5km * dmspols_zhang_ihs_sum2_1996_woreda)

# OLS --------------------------------------------------------------------------
lm_dmspols_zhang_ihs <- felm(dmspols_zhang_ihs ~ near_anyimproved_5km | 0 | 0 | W_CODE, data = df2012)
lm_dmspols_zhang_2   <- felm(dmspols_zhang_2   ~ near_anyimproved_5km | 0 | 0 | W_CODE, data = df2012)
lm_dmspols_zhang_6   <- felm(dmspols_zhang_6   ~ near_anyimproved_5km | 0 | 0 | W_CODE, data = df2012)
lm_globcover_urban   <- felm(globcover_urban   ~ near_anyimproved_5km | 0 | 0 | W_CODE, data = df2016)

lm_dmspols_zhang_ihs_addis <- felm(dmspols_zhang_ihs ~ near_anyimproved_5km + near_anyimproved_5kmXdistance_city_addisababa | 0 | 0 | W_CODE, data = df2012)
lm_dmspols_zhang_2_addis   <- felm(dmspols_zhang_2   ~ near_anyimproved_5km + near_anyimproved_5kmXdistance_city_addisababa | 0 | 0 | W_CODE, data = df2012)
lm_dmspols_zhang_6_addis   <- felm(dmspols_zhang_6   ~ near_anyimproved_5km + near_anyimproved_5kmXdistance_city_addisababa | 0 | 0 | W_CODE, data = df2012)
lm_globcover_urban_addis   <- felm(globcover_urban   ~ near_anyimproved_5km + near_anyimproved_5kmXdistance_city_addisababa | 0 | 0 | W_CODE, data = df2016)

lm_dmspols_zhang_ihs_basentl <- felm(dmspols_zhang_ihs ~ near_anyimproved_5km + near_anyimproved_5kmXdmspols_zhang_ihs_1996_woreda | 0 | 0 | W_CODE, data = df2012)
lm_dmspols_zhang_2_basentl   <- felm(dmspols_zhang_2   ~ near_anyimproved_5km + near_anyimproved_5kmXdmspols_zhang_ihs_1996_woreda | 0 | 0 | W_CODE, data = df2012)
lm_dmspols_zhang_6_basentl   <- felm(dmspols_zhang_6   ~ near_anyimproved_5km + near_anyimproved_5kmXdmspols_zhang_ihs_1996_woreda | 0 | 0 | W_CODE, data = df2012)
lm_globcover_urban_basentl   <- felm(globcover_urban   ~ near_anyimproved_5km + near_anyimproved_5kmXdmspols_zhang_ihs_1996_woreda | 0 | 0 | W_CODE, data = df2016)

lm_dmspols_zhang_ihs_basentl2 <- felm(dmspols_zhang_ihs ~ near_anyimproved_5km + near_anyimproved_5kmXdmspols_zhang_ihs_sum2_1996_woreda | 0 | 0 | W_CODE, data = df2012)
lm_dmspols_zhang_2_basentl2   <- felm(dmspols_zhang_2   ~ near_anyimproved_5km + near_anyimproved_5kmXdmspols_zhang_ihs_sum2_1996_woreda | 0 | 0 | W_CODE, data = df2012)
lm_dmspols_zhang_6_basentl2   <- felm(dmspols_zhang_6   ~ near_anyimproved_5km + near_anyimproved_5kmXdmspols_zhang_ihs_sum2_1996_woreda | 0 | 0 | W_CODE, data = df2012)
lm_globcover_urban_basentl2   <- felm(globcover_urban   ~ near_anyimproved_5km + near_anyimproved_5kmXdmspols_zhang_ihs_sum2_1996_woreda | 0 | 0 | W_CODE, data = df2016)

# MST - Cost Distance ----------------------------------------------------------
iv_cd_dmspols_zhang_ihs <- felm(dmspols_zhang_ihs ~ 1 | 0 | (near_anyimproved_5km ~ near_mst_5km) | W_CODE, data = df2012)
iv_cd_dmspols_zhang_2   <- felm(dmspols_zhang_2   ~ 1 | 0 | (near_anyimproved_5km ~ near_mst_5km) | W_CODE, data = df2012)
iv_cd_dmspols_zhang_6   <- felm(dmspols_zhang_6   ~ 1 | 0 | (near_anyimproved_5km ~ near_mst_5km) | W_CODE, data = df2012)
iv_cd_globcover_urban   <- felm(globcover_urban   ~ 1 | 0 | (near_anyimproved_5km ~ near_mst_5km) | W_CODE, data = df2016)

iv_cd_dmspols_zhang_ihs_addis <- felm(dmspols_zhang_ihs ~ 1 | 0 | (near_anyimproved_5km|near_anyimproved_5kmXdistance_city_addisababa ~ near_mst_5km + near_mst_5kmXdistance_city_addisababa) | W_CODE, data = df2012)
iv_cd_dmspols_zhang_2_addis   <- felm(dmspols_zhang_2   ~ 1 | 0 | (near_anyimproved_5km|near_anyimproved_5kmXdistance_city_addisababa ~ near_mst_5km + near_mst_5kmXdistance_city_addisababa) | W_CODE, data = df2012)
iv_cd_dmspols_zhang_6_addis   <- felm(dmspols_zhang_6   ~ 1 | 0 | (near_anyimproved_5km|near_anyimproved_5kmXdistance_city_addisababa ~ near_mst_5km + near_mst_5kmXdistance_city_addisababa) | W_CODE, data = df2012)
iv_cd_globcover_urban_addis   <- felm(globcover_urban   ~ 1 | 0 | (near_anyimproved_5km|near_anyimproved_5kmXdistance_city_addisababa ~ near_mst_5km + near_mst_5kmXdistance_city_addisababa) | W_CODE, data = df2016)

iv_cd_dmspols_zhang_ihs_basentl <- felm(dmspols_zhang_ihs ~ 1 | 0 | (near_anyimproved_5km|near_anyimproved_5kmXdmspols_zhang_ihs_1996_woreda ~ near_mst_5km + near_mst_5kmXdmspols_zhang_ihs_1996_woreda) | W_CODE, data = df2012)
iv_cd_dmspols_zhang_2_basentl   <- felm(dmspols_zhang_2   ~ 1 | 0 | (near_anyimproved_5km|near_anyimproved_5kmXdmspols_zhang_ihs_1996_woreda ~ near_mst_5km + near_mst_5kmXdmspols_zhang_ihs_1996_woreda) | W_CODE, data = df2012)
iv_cd_dmspols_zhang_6_basentl   <- felm(dmspols_zhang_6   ~ 1 | 0 | (near_anyimproved_5km|near_anyimproved_5kmXdmspols_zhang_ihs_1996_woreda ~ near_mst_5km + near_mst_5kmXdmspols_zhang_ihs_1996_woreda) | W_CODE, data = df2012)
iv_cd_globcover_urban_basentl   <- felm(globcover_urban   ~ 1 | 0 | (near_anyimproved_5km|near_anyimproved_5kmXdmspols_zhang_ihs_1996_woreda ~ near_mst_5km + near_mst_5kmXdmspols_zhang_ihs_1996_woreda) | W_CODE, data = df2016)

iv_cd_dmspols_zhang_ihs_basentl2 <- felm(dmspols_zhang_ihs ~ 1 | 0 | (near_anyimproved_5km|near_anyimproved_5kmXdmspols_zhang_ihs_sum2_1996_woreda ~ near_mst_5km + near_mst_5kmXdmspols_zhang_ihs_sum2_1996_woreda) | W_CODE, data = df2012)
iv_cd_dmspols_zhang_2_basentl2   <- felm(dmspols_zhang_2   ~ 1 | 0 | (near_anyimproved_5km|near_anyimproved_5kmXdmspols_zhang_ihs_sum2_1996_woreda ~ near_mst_5km + near_mst_5kmXdmspols_zhang_ihs_sum2_1996_woreda) | W_CODE, data = df2012)
iv_cd_dmspols_zhang_6_basentl2   <- felm(dmspols_zhang_6   ~ 1 | 0 | (near_anyimproved_5km|near_anyimproved_5kmXdmspols_zhang_ihs_sum2_1996_woreda ~ near_mst_5km + near_mst_5kmXdmspols_zhang_ihs_sum2_1996_woreda) | W_CODE, data = df2012)
iv_cd_globcover_urban_basentl2   <- felm(globcover_urban   ~ 1 | 0 | (near_anyimproved_5km|near_anyimproved_5kmXdmspols_zhang_ihs_sum2_1996_woreda ~ near_mst_5km + near_mst_5kmXdmspols_zhang_ihs_sum2_1996_woreda) | W_CODE, data = df2016)

# MST - Least Distance ---------------------------------------------------------
iv_ld_dmspols_zhang_ihs <- felm(dmspols_zhang_ihs ~ 1 | 0 | (near_anyimproved_5km ~ near_mst_mindist_5km) | W_CODE, data = df2012)
iv_ld_dmspols_zhang_2   <- felm(dmspols_zhang_2   ~ 1 | 0 | (near_anyimproved_5km ~ near_mst_mindist_5km) | W_CODE, data = df2012)
iv_ld_dmspols_zhang_6   <- felm(dmspols_zhang_6   ~ 1 | 0 | (near_anyimproved_5km ~ near_mst_mindist_5km) | W_CODE, data = df2012)
iv_ld_globcover_urban   <- felm(globcover_urban   ~ 1 | 0 | (near_anyimproved_5km ~ near_mst_mindist_5km) | W_CODE, data = df2016)

iv_ld_dmspols_zhang_ihs_addis <- felm(dmspols_zhang_ihs ~ 1 | 0 | (near_anyimproved_5km|near_anyimproved_5kmXdistance_city_addisababa ~ near_mst_mindist_5km + near_mst_mindist_5kmXdistance_city_addisababa) | W_CODE, data = df2012)
iv_ld_dmspols_zhang_2_addis   <- felm(dmspols_zhang_2   ~ 1 | 0 | (near_anyimproved_5km|near_anyimproved_5kmXdistance_city_addisababa ~ near_mst_mindist_5km + near_mst_mindist_5kmXdistance_city_addisababa) | W_CODE, data = df2012)
iv_ld_dmspols_zhang_6_addis   <- felm(dmspols_zhang_6   ~ 1 | 0 | (near_anyimproved_5km|near_anyimproved_5kmXdistance_city_addisababa ~ near_mst_mindist_5km + near_mst_mindist_5kmXdistance_city_addisababa) | W_CODE, data = df2012)
iv_ld_globcover_urban_addis   <- felm(globcover_urban   ~ 1 | 0 | (near_anyimproved_5km|near_anyimproved_5kmXdistance_city_addisababa ~ near_mst_mindist_5km + near_mst_mindist_5kmXdistance_city_addisababa) | W_CODE, data = df2016)

iv_ld_dmspols_zhang_ihs_basentl <- felm(dmspols_zhang_ihs ~ 1 | 0 | (near_anyimproved_5km|near_anyimproved_5kmXdmspols_zhang_ihs_1996_woreda ~ near_mst_mindist_5km + near_mst_mindist_5kmXdmspols_zhang_ihs_1996_woreda) | W_CODE, data = df2012)
iv_ld_dmspols_zhang_2_basentl   <- felm(dmspols_zhang_2   ~ 1 | 0 | (near_anyimproved_5km|near_anyimproved_5kmXdmspols_zhang_ihs_1996_woreda ~ near_mst_mindist_5km + near_mst_mindist_5kmXdmspols_zhang_ihs_1996_woreda) | W_CODE, data = df2012)
iv_ld_dmspols_zhang_6_basentl   <- felm(dmspols_zhang_6   ~ 1 | 0 | (near_anyimproved_5km|near_anyimproved_5kmXdmspols_zhang_ihs_1996_woreda ~ near_mst_mindist_5km + near_mst_mindist_5kmXdmspols_zhang_ihs_1996_woreda) | W_CODE, data = df2012)
iv_ld_globcover_urban_basentl   <- felm(globcover_urban   ~ 1 | 0 | (near_anyimproved_5km|near_anyimproved_5kmXdmspols_zhang_ihs_1996_woreda ~ near_mst_mindist_5km + near_mst_mindist_5kmXdmspols_zhang_ihs_1996_woreda) | W_CODE, data = df2016)

iv_ld_dmspols_zhang_ihs_basentl2 <- felm(dmspols_zhang_ihs ~ 1 | 0 | (near_anyimproved_5km|near_anyimproved_5kmXdmspols_zhang_ihs_sum2_1996_woreda ~ near_mst_mindist_5km + near_mst_mindist_5kmXdmspols_zhang_ihs_sum2_1996_woreda) | W_CODE, data = df2012)
iv_ld_dmspols_zhang_2_basentl2   <- felm(dmspols_zhang_2   ~ 1 | 0 | (near_anyimproved_5km|near_anyimproved_5kmXdmspols_zhang_ihs_sum2_1996_woreda ~ near_mst_mindist_5km + near_mst_mindist_5kmXdmspols_zhang_ihs_sum2_1996_woreda) | W_CODE, data = df2012)
iv_ld_dmspols_zhang_6_basentl2   <- felm(dmspols_zhang_6   ~ 1 | 0 | (near_anyimproved_5km|near_anyimproved_5kmXdmspols_zhang_ihs_sum2_1996_woreda ~ near_mst_mindist_5km + near_mst_mindist_5kmXdmspols_zhang_ihs_sum2_1996_woreda) | W_CODE, data = df2012)
iv_ld_globcover_urban_basentl2   <- felm(globcover_urban   ~ 1 | 0 | (near_anyimproved_5km|near_anyimproved_5kmXdmspols_zhang_ihs_sum2_1996_woreda ~ near_mst_mindist_5km + near_mst_mindist_5kmXdmspols_zhang_ihs_sum2_1996_woreda) | W_CODE, data = df2016)

# OLS - Stargazer --------------------------------------------------------------
stargazer(lm_dmspols_zhang_ihs,
          lm_dmspols_zhang_ihs_basentl,
          lm_dmspols_zhang_ihs_basentl2,
          lm_dmspols_zhang_ihs_addis,
          
          lm_dmspols_zhang_2,
          lm_dmspols_zhang_2_basentl,
          lm_dmspols_zhang_2_basentl2,
          lm_dmspols_zhang_2_addis,
         
          lm_dmspols_zhang_6,
          lm_dmspols_zhang_6_basentl,
          lm_dmspols_zhang_6_basentl2,
          lm_dmspols_zhang_6_addis,
          
          lm_globcover_urban,
          lm_globcover_urban_basentl,
          lm_globcover_urban_basentl2,
          lm_globcover_urban_addis,
         
          dep.var.labels.include = T,
          dep.var.labels = c("NTL (IHS)", "NTL $>$ 2", "NTL $>$ 6", "Urban"),
          dep.var.caption = "",
          omit = c("temp_avg", "precipitation"),
          covariate.labels = c("Imp Rd.",
                               "Imp Rd.$\\times NTL_{96}$",
                               "Imp Rd.$\\times NTL_{96}>2$",
                               "Imp Rd. X Dist Addis"),
          omit.stat = c("f","ser"),
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width="-15pt",
          digits=2,
          out = file.path(paper_tables, 
                          "ols_near_road_5km_eth_grid_eth_grid_results.tex"))

# MST - Cost Distance - Stargazer ----------------------------------------------
stargazer(iv_cd_dmspols_zhang_ihs,
          iv_cd_dmspols_zhang_ihs_basentl,
          iv_cd_dmspols_zhang_ihs_basentl2,
          iv_cd_dmspols_zhang_ihs_addis,
          
          iv_cd_dmspols_zhang_2,
          iv_cd_dmspols_zhang_2_basentl,
          iv_cd_dmspols_zhang_2_basentl2,
          iv_cd_dmspols_zhang_2_addis,
          
          iv_cd_dmspols_zhang_6,
          iv_cd_dmspols_zhang_6_basentl,
          iv_cd_dmspols_zhang_6_basentl2,
          iv_cd_dmspols_zhang_6_addis,
          
          iv_cd_globcover_urban,
          iv_cd_globcover_urban_basentl,
          iv_cd_globcover_urban_basentl2,
          iv_cd_globcover_urban_addis,
          
          dep.var.labels.include = T,
          dep.var.labels = c("NTL (IHS)", "NTL $>$ 2", "NTL $>$ 6", "Urban"),
          dep.var.caption = "",
          covariate.labels = c("Imp Rd.",
                               "Imp Rd.$\\times NTL_{96}$",
                               "Imp Rd.$\\times NTL_{96}>2$",
                               "Imp Rd. X Dist Addis"),
          omit.stat = c("f","ser"),
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width="-15pt",
          digits=2,
          out = file.path(paper_tables,
                          "iv_near_mst_cost_distance_5km_eth_grid_results.tex"))

# MST - Least Distance - Stargazer ---------------------------------------------
stargazer(iv_ld_dmspols_zhang_ihs,
          iv_ld_dmspols_zhang_ihs_basentl,
          iv_ld_dmspols_zhang_ihs_basentl2,
          iv_ld_dmspols_zhang_ihs_addis,
          
          iv_ld_dmspols_zhang_2,
          iv_ld_dmspols_zhang_2_basentl,
          iv_ld_dmspols_zhang_2_basentl2,
          iv_ld_dmspols_zhang_2_addis,
          
          iv_ld_dmspols_zhang_6,
          iv_ld_dmspols_zhang_6_basentl,
          iv_ld_dmspols_zhang_6_basentl2,
          iv_ld_dmspols_zhang_6_addis,
          
          iv_ld_globcover_urban,
          iv_ld_globcover_urban_basentl,
          iv_ld_globcover_urban_basentl2,
          iv_ld_globcover_urban_addis,
        
          dep.var.labels.include = T,
          dep.var.labels = c("NTL (IHS)", "NTL $>$ 2", "NTL $>$ 6", "Urban"),
          dep.var.caption = "",
          covariate.labels = c("Imp Rd.",
                               "Imp Rd.$\\times NTL_{96}$",
                               "Imp Rd.$\\times NTL_{96}>2$",
                               "Imp Rd. X Dist Addis"),
          omit.stat = c("f","ser"),
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width="-15pt",
          digits=2,
          out = file.path(paper_tables, 
                          "iv_near_mst_least_distance_5km_eth_grid_results.tex"))

# First Stage - Cost Distance - Stargazer --------------------------------------
stargazer(iv_cd_dmspols_zhang_ihs$stage1,
          dep.var.labels.include = T,
          dep.var.labels = c("Near Improved Road"),
          dep.var.caption = "",
          covariate.labels = "Near MST",
          omit.stat = c("f","ser"),
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width="-15pt",
          digits=2,
          add.lines = list(
            c("1st Stage F-Stat", 
              lfe::waldtest(iv_cd_dmspols_zhang_ihs$stage1, ~near_mst_5km, lhs=iv_cd_dmspols_zhang_ihs$stage1$lhs)[5] %>% round(ROUND_NUM)
            )
          ),
          out = file.path(paper_tables, 
                          "iv_near_mst_cost_distance_5km_1ststage_eth_grid_results.tex"))

# IV - First Stage - Stargazer -------------------------------------------------
stargazer(iv_cd_dmspols_zhang_ihs$stage1,
          dep.var.labels.include = T,
          dep.var.labels = c("Near Improved Road"),
          dep.var.caption = "",
          covariate.labels = "Near MST",
          omit.stat = c("f","ser"),
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width="-15pt",
          digits=2,
          add.lines = list(
            c("1st Stage F-Stat", 
              lfe::waldtest(iv_cd_dmspols_zhang_ihs$stage1, ~near_mst_5km, lhs=iv_cd_dmspols_zhang_ihs$stage1$lhs)[5] %>% round(ROUND_NUM)
            )
          ),
          out = file.path(paper_tables, 
                          "iv_near_mst_cost_distance_5km_1ststage_eth_grid_results.tex"))

stargazer(iv_ld_dmspols_zhang_ihs$stage1,
          dep.var.labels.include = T,
          dep.var.labels = c("Near Improved Road"),
          dep.var.caption = "",
          covariate.labels = "Near MST",
          omit.stat = c("f","ser"),
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width="-15pt",
          digits=2,
          add.lines = list(
            c("1st Stage F-Stat", 
              lfe::waldtest(iv_ld_dmspols_zhang_ihs$stage1, ~near_mst_mindist_5km, lhs=iv_ld_dmspols_zhang_ihs$stage1$lhs)[5] %>% round(ROUND_NUM)
            )
          ),
          out = file.path(paper_tables, 
                          "iv_near_mst_least_distance_5km_1ststage_eth_grid_results.tex"))


