# Impact of Expressway Expansion
# Ethiopia IE

dataset <- "points_5percent"

# Load Data --------------------------------------------------------------------
if(dataset == "cluster_all"){
  data <- readRDS(file.path(project_file_path, "Data", "FinalData", "urban_cluster_dataset", "urban_cluster_data_analysisvars.Rds"))
  data$cell_id <- data$cluster_id
}

if(dataset == "points"){
  data <- readRDS(file.path(project_file_path, "Data", "FinalData", "dmspols_grid_dataset", "dmspols_level_dataset_analysisvars.Rds"))
}

if(dataset == "points_5percent"){
  data <- readRDS(file.path(project_file_path, "Data", "FinalData", "dmspols_grid_dataset", "dmspols_level_dataset_5percentsample_analysisvars.Rds"))
  data$gc_urban_mean <- data$globcover_urban
}

data <- data[(data$year >= 1996) & (data$year <= 2016),]
data <- data[!is.na(data$years_since_improved_all),]

# Fix Variables ----------------------------------------------------------------
data$years_since_improved_all_group_placebo <- factor(data$years_since_improved_all_group_placebo, 
                                                      levels = c("T: -1","Treated", "T: -2", "T: -3", "T: -4"))

data$rsdp_phase_improved_all <- 0
data$rsdp_phase_improved_all[data$year_improved_all %in% 1996:2002 & data$year >= 1996] <- 0
data$rsdp_phase_improved_all[data$year_improved_all %in% 2003:2007 & data$year >= 2003] <- 2
data$rsdp_phase_improved_all[data$year_improved_all %in% 2008:2010 & data$year >= 2008] <- 3
data$rsdp_phase_improved_all[data$year_improved_all %in% 2011:2016 & data$year >= 2011] <- 4
data$rsdp_phase_improved_all <- as.factor(data$rsdp_phase_improved_all)

data$rsdp_phase_improved_50above <- 0
data$rsdp_phase_improved_50above[data$year_improved_50above %in% 1996:2002 & data$year >= 1996] <- 0
data$rsdp_phase_improved_50above[data$year_improved_50above %in% 2003:2007 & data$year >= 2003] <- 2
data$rsdp_phase_improved_50above[data$year_improved_50above %in% 2008:2010 & data$year >= 2008] <- 3
data$rsdp_phase_improved_50above[data$year_improved_50above %in% 2011:2016 & data$year >= 2011] <- 4
data$rsdp_phase_improved_50above <- as.factor(data$rsdp_phase_improved_50above)

data$rsdp_phase_improved_below50 <- 0
data$rsdp_phase_improved_below50[data$year_improved_below50 %in% 1996:2002 & data$year >= 1996] <- 0
data$rsdp_phase_improved_below50[data$year_improved_below50 %in% 2003:2007 & data$year >= 2003] <- 2
data$rsdp_phase_improved_below50[data$year_improved_below50 %in% 2008:2010 & data$year >= 2008] <- 3
data$rsdp_phase_improved_below50[data$year_improved_below50 %in% 2011:2016 & data$year >= 2011] <- 4
data$rsdp_phase_improved_below50 <- as.factor(data$rsdp_phase_improved_below50)



# Regressions: Binary Treatment ------------------------------------------------
covariate.labels <- c("Imp. Rd, Any",
                      "Imp. Rd $<$ 50km/hr",
                      "Imp. Rd $\\geq$ 50km/hr",
                      
                      "Imp. Rd, Any $\\times$ Base NTL Zero",
                      "Imp. Rd, Any $\\times$ Base NTL Low",
                      "Imp. Rd, Any $\\times$ Base NTL High",
                      
                      "Imp. Rd $<$ 50km/hr $\\times$ Base NTL Zero",
                      "Imp. Rd $<$ 50km/hr $\\times$ Base NTL Low",
                      "Imp. Rd $<$ 50km/hr $\\times$ Base NTL High",
                      
                      "Imp. Rd $\\geq$ 50km/hr $\\times$ Base NTL Zero",
                      "Imp. Rd $\\geq$ 50km/hr $\\times$ Base NTL Low",
                      "Imp. Rd $\\geq$ 50km/hr $\\times$ Base NTL High")

# Points -----------------------------------------------------------------------

# Placebo
felm_allroads_placebo_dmspols_yearfe <- felm(dmspols_zhang ~ years_since_improved_all_placebo_bin | cell_id + year | 0 | GADM_ID_3, data=data)
felm_allroads_placebo_dmspols_yearlinear <- felm(dmspols_zhang ~ years_since_improved_all_placebo_bin + year | cell_id | 0 | GADM_ID_3, data=data)

felm_allroads_placebo_dmspols1_yearfe <- felm(dmspols_zhang_1 ~ years_since_improved_all_placebo_bin | cell_id + year | 0 | GADM_ID_3, data=data)
felm_allroads_placebo_dmspols1_yearlinear <- felm(dmspols_zhang_1 ~ years_since_improved_all_placebo_bin + year | cell_id | 0 | GADM_ID_3, data=data)

felm_allroads_placebo_dmspols5_yearfe <- felm(dmspols_zhang_5 ~ years_since_improved_all_placebo_bin | cell_id + year | 0 | GADM_ID_3, data=data)
felm_allroads_placebo_dmspols5_yearlinear <- felm(dmspols_zhang_5 ~ years_since_improved_all_placebo_bin + year | cell_id | 0 | GADM_ID_3, data=data)

felm_allroads_placebo_urban_yearfe <- felm(gc_urban_mean ~ years_since_improved_all_placebo_bin | cell_id + year | 0 | GADM_ID_3, data=data)
felm_allroads_placebo_urban_yearlinear <- felm(gc_urban_mean ~ years_since_improved_all_placebo_bin + year | cell_id | 0 | GADM_ID_3, data=data)

felm_allroads_placebo_cropland_yearfe <- felm(globcover_cropland ~ years_since_improved_all_placebo_bin | cell_id + year | 0 | GADM_ID_3, data=data)
felm_allroads_placebo_cropland_yearlinear <- felm(globcover_cropland ~ years_since_improved_all_placebo_bin + year | cell_id | 0 | GADM_ID_3, data=data)

felm_allroads_placebo_ndvi_yearfe <- felm(ndvi ~ years_since_improved_all_placebo_bin | cell_id + year | 0 | GADM_ID_3, data=data)
felm_allroads_placebo_ndvi_yearlinear <- felm(ndvi ~ years_since_improved_all_placebo_bin + year | cell_id | 0 | GADM_ID_3, data=data)

felm_allroads_placebo_ndvi_cropland_yearfe <- felm(ndvi_cropland ~ years_since_improved_all_placebo_bin | cell_id + year | 0 | GADM_ID_3, data=data[data$globcover_cropland_2015 > 0,])
felm_allroads_placebo_ndvi_cropland_yearlinear <- felm(ndvi_cropland ~ years_since_improved_all_placebo_bin + year | cell_id | 0 | GADM_ID_3, data=data[data$globcover_cropland_2015 > 0,])

felm_allroads_placebo_ndvi_nocropland_yearfe <- felm(ndvi_nocropland ~ years_since_improved_all_placebo_bin | cell_id + year | 0 | GADM_ID_3, data=data[data$globcover_cropland_2015 %in% 0,])
felm_allroads_placebo_ndvi_nocropland_yearlinear <- felm(ndvi_nocropland ~ years_since_improved_all_placebo_bin + year | cell_id | 0 | GADM_ID_3, data=data[data$globcover_cropland_2015 %in% 0,])

# Placebo - Leads
felm_allroads_placebo_leads_dmspols <- felm(dmspols_zhang ~ factor(years_since_improved_all_group_placebo) | year + cell_id | 0 | GADM_ID_3, data=data)
felm_allroads_placebo_leads_dmspols_1 <- felm(dmspols_zhang_1 ~ factor(years_since_improved_all_group_placebo) | year + cell_id | 0 | GADM_ID_3, data=data)
felm_allroads_placebo_leads_dmspols_5 <- felm(dmspols_zhang_5 ~ factor(years_since_improved_all_group_placebo) | year + cell_id | 0 | GADM_ID_3, data=data)
felm_allroads_placebo_leads_urban <- felm(globcover_urban ~ factor(years_since_improved_all_group_placebo) | year + cell_id | 0 | GADM_ID_3, data=data)
felm_allroads_placebo_leads_cropland <- felm(globcover_cropland ~ factor(years_since_improved_all_group_placebo) | year + cell_id | 0 | GADM_ID_3, data=data)
felm_allroads_placebo_leads_ndvi <- felm(ndvi ~ factor(years_since_improved_all_group_placebo) | year + cell_id | 0 | GADM_ID_3, data=data)
felm_allroads_placebo_leads_ndvi_cropland <- felm(ndvi_nocropland ~ factor(years_since_improved_all_group_placebo) | year + cell_id | 0 | GADM_ID_3, data=data)

# Did - all
felm_allroads_did_dmspols <- felm(dmspols_zhang ~ rsdp_phase_improved_all | year + cell_id | 0 | GADM_ID_3, data=data)
felm_allroads_did_dmspols_1 <- felm(dmspols_zhang_1 ~ rsdp_phase_improved_all | year + cell_id | 0 | GADM_ID_3, data=data)
felm_allroads_did_dmspols_5 <- felm(dmspols_zhang_5 ~ rsdp_phase_improved_all | year + cell_id | 0 | GADM_ID_3, data=data)
felm_allroads_did_urban <- felm(globcover_urban ~ rsdp_phase_improved_all | year + cell_id | 0 | GADM_ID_3, data=data)
felm_allroads_did_cropland <- felm(globcover_cropland ~ rsdp_phase_improved_all | year + cell_id | 0 | GADM_ID_3, data=data)
felm_allroads_did_ndvi <- felm(ndvi ~ rsdp_phase_improved_all | year + cell_id | 0 | GADM_ID_3, data=data)
felm_allroads_did_ndvi_cropland <- felm(ndvi_cropland ~ rsdp_phase_improved_all | year + cell_id | 0 | GADM_ID_3, data=data)

# Did - by road
felm_byroad_did_dmspols <- felm(dmspols_zhang ~ rsdp_phase_improved_50above + rsdp_phase_improved_below50  | year + cell_id | 0 | GADM_ID_3, data=data)
felm_byroad_did_dmspols_1 <- felm(dmspols_zhang_1 ~ rsdp_phase_improved_50above + rsdp_phase_improved_below50 | year + cell_id | 0 | GADM_ID_3, data=data)
felm_byroad_did_dmspols_5 <- felm(dmspols_zhang_5 ~ rsdp_phase_improved_50above + rsdp_phase_improved_below50 | year + cell_id | 0 | GADM_ID_3, data=data)
felm_byroad_did_urban <- felm(globcover_urban ~ rsdp_phase_improved_50above + rsdp_phase_improved_below50 | year + cell_id | 0 | GADM_ID_3, data=data)
felm_byroad_did_cropland <- felm(globcover_cropland ~ rsdp_phase_improved_50above + rsdp_phase_improved_below50 | year + cell_id | 0 | GADM_ID_3, data=data)
felm_byroad_did_ndvi <- felm(ndvi ~ rsdp_phase_improved_50above + rsdp_phase_improved_below50 | year + cell_id | 0 | GADM_ID_3, data=data)
felm_byroad_did_ndvi_cropland <- felm(ndvi_cropland ~ rsdp_phase_improved_50above + rsdp_phase_improved_below50 | year + cell_id | 0 | GADM_ID_3, data=data)

# DMSP-OLS Value 
felm_allroads <- felm(dmspols_zhang ~ years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data)
felm_allroads_ntlbase <- felm(dmspols_zhang ~ years_since_improved_all_bin*factor(dmspols_1997_group)-factor(dmspols_1997_group)-years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data) 

felm_blabv50roads <- felm(dmspols_zhang ~ years_since_improved_below50_bin + years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin),])
felm_blabv50roads_ntlbase <- felm(dmspols_zhang ~ years_since_improved_below50_bin*factor(dmspols_1997_group) + 
                                    years_since_improved_50above_bin*factor(dmspols_1997_group) -
                                    factor(dmspols_1997_group) -
                                    years_since_improved_below50_bin - 
                                    years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin),])  

# DMSP-OLS > 1
felm_allroads_dmspols1 <- felm(dmspols_zhang_1 ~ years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data)
felm_allroads_ntlbase_dmspols1 <- felm(dmspols_zhang_1 ~ years_since_improved_all_bin*factor(dmspols_1997_group)-factor(dmspols_1997_group)-years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data) 

felm_blabv50roads_dmspols1 <- felm(dmspols_zhang_1 ~ years_since_improved_below50_bin + years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin),])
felm_blabv50roads_ntlbase_dmspols1 <- felm(dmspols_zhang_1 ~ years_since_improved_below50_bin*factor(dmspols_1997_group) + 
                                          years_since_improved_50above_bin*factor(dmspols_1997_group) -
                                          factor(dmspols_1997_group) -
                                          years_since_improved_below50_bin - 
                                          years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin),])  

# DMSP-OLS > 5
felm_allroads_dmspols5 <- felm(dmspols_zhang_5 ~ years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data)
felm_allroads_ntlbase_dmspols5 <- felm(dmspols_zhang_5 ~ years_since_improved_all_bin*factor(dmspols_1997_group)-factor(dmspols_1997_group)-years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data) 

felm_blabv50roads_dmspols5 <- felm(dmspols_zhang_5 ~ years_since_improved_below50_bin + years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin),])
felm_blabv50roads_ntlbase_dmspols5 <- felm(dmspols_zhang_5 ~ years_since_improved_below50_bin*factor(dmspols_1997_group) + 
                                             years_since_improved_50above_bin*factor(dmspols_1997_group) -
                                             factor(dmspols_1997_group) -
                                             years_since_improved_below50_bin - 
                                             years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin),])  

# Urban
felm_allroads_urban <- felm(gc_urban_mean ~ years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data)
felm_allroads_ntlbase_urban <- felm(gc_urban_mean ~ years_since_improved_all_bin*factor(dmspols_1997_group)-factor(dmspols_1997_group)-years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data) 

felm_blabv50roads_urban <- felm(gc_urban_mean ~ years_since_improved_below50_bin + years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin),])
felm_blabv50roads_ntlbase_urban <- felm(gc_urban_mean ~ years_since_improved_below50_bin*factor(dmspols_1997_group) + 
                                         years_since_improved_50above_bin*factor(dmspols_1997_group) -
                                         factor(dmspols_1997_group) -
                                         years_since_improved_below50_bin - 
                                         years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin),])  

# Cropland
felm_allroads_cropland <- felm(globcover_cropland ~ years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data)
felm_allroads_ntlbase_cropland <- felm(globcover_cropland ~ years_since_improved_all_bin*factor(dmspols_1997_group)-factor(dmspols_1997_group)-years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data) 

felm_blabv50roads_cropland <- felm(globcover_cropland ~ years_since_improved_below50_bin + years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin),])
felm_blabv50roads_ntlbase_cropland <- felm(globcover_cropland ~ years_since_improved_below50_bin*factor(dmspols_1997_group) + 
                                          years_since_improved_50above_bin*factor(dmspols_1997_group) -
                                          factor(dmspols_1997_group) -
                                          years_since_improved_below50_bin - 
                                          years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin),])  

# Cropland - Irrigates
felm_allroads_cropland_irrigated <- felm(globcover_cropland_irrigated ~ years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data)
felm_allroads_ntlbase_cropland_irrigated <- felm(globcover_cropland_irrigated ~ years_since_improved_all_bin*factor(dmspols_1997_group)-factor(dmspols_1997_group)-years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data) 

felm_blabv50roads_cropland_irrigated <- felm(globcover_cropland_irrigated ~ years_since_improved_below50_bin + years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin),])
felm_blabv50roads_ntlbase_cropland_irrigated <- felm(globcover_cropland_irrigated ~ years_since_improved_below50_bin*factor(dmspols_1997_group) + 
                                             years_since_improved_50above_bin*factor(dmspols_1997_group) -
                                             factor(dmspols_1997_group) -
                                             years_since_improved_below50_bin - 
                                             years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin),])  

# Cropland - Rainfed
felm_allroads_cropland_rainfed <- felm(globcover_cropland_rainfed ~ years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data)
felm_allroads_ntlbase_cropland_rainfed <- felm(globcover_cropland_rainfed ~ years_since_improved_all_bin*factor(dmspols_1997_group)-factor(dmspols_1997_group)-years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data) 

felm_blabv50roads_cropland_rainfed <- felm(globcover_cropland_rainfed ~ years_since_improved_below50_bin + years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin),])
felm_blabv50roads_ntlbase_cropland_rainfed <- felm(globcover_cropland_rainfed ~ years_since_improved_below50_bin*factor(dmspols_1997_group) + 
                                                       years_since_improved_50above_bin*factor(dmspols_1997_group) -
                                                       factor(dmspols_1997_group) -
                                                       years_since_improved_below50_bin - 
                                                       years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin),])  

# Cropland - Mosaic
felm_allroads_cropland_mosaic <- felm(globcover_cropland_mosaic ~ years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data)
felm_allroads_ntlbase_cropland_mosaic <- felm(globcover_cropland_mosaic ~ years_since_improved_all_bin*factor(dmspols_1997_group)-factor(dmspols_1997_group)-years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data) 

felm_blabv50roads_cropland_mosaic <- felm(globcover_cropland_mosaic ~ years_since_improved_below50_bin + years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin),])
felm_blabv50roads_ntlbase_cropland_mosaic <- felm(globcover_cropland_mosaic ~ years_since_improved_below50_bin*factor(dmspols_1997_group) + 
                                                     years_since_improved_50above_bin*factor(dmspols_1997_group) -
                                                     factor(dmspols_1997_group) -
                                                     years_since_improved_below50_bin - 
                                                     years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin),])  

# NDVI
felm_allroads_ndvi <- felm(ndvi ~ years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data)
felm_allroads_ntlbase_ndvi <- felm(ndvi ~ years_since_improved_all_bin*factor(dmspols_1997_group)-factor(dmspols_1997_group)-years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data) 

felm_blabv50roads_ndvi <- felm(ndvi ~ years_since_improved_below50_bin + years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin),])
felm_blabv50roads_ntlbase_ndvi <- felm(ndvi ~ years_since_improved_below50_bin*factor(dmspols_1997_group) + 
                                                    years_since_improved_50above_bin*factor(dmspols_1997_group) -
                                                    factor(dmspols_1997_group) -
                                                    years_since_improved_below50_bin - 
                                                    years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin),])  

# NDVI - Cropland
felm_allroads_ndvi_cropland <- felm(ndvi_cropland ~ years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data[data$globcover_cropland_2015 > 0,])
felm_allroads_ntlbase_ndvi_cropland <- felm(ndvi_cropland ~ years_since_improved_all_bin*factor(dmspols_1997_group)-factor(dmspols_1997_group)-years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data[data$globcover_cropland_2015 > 0,]) 

felm_blabv50roads_ndvi_cropland <- felm(ndvi_cropland ~ years_since_improved_below50_bin + years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin) & data$globcover_cropland_2015 > 0,])
felm_blabv50roads_ntlbase_ndvi_cropland <- felm(ndvi_cropland ~ years_since_improved_below50_bin*factor(dmspols_1997_group) + 
                                                    years_since_improved_50above_bin*factor(dmspols_1997_group) -
                                                    factor(dmspols_1997_group) -
                                                    years_since_improved_below50_bin - 
                                                    years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin) & data$globcover_cropland_2015 > 0,])  

# NDVI - No Cropland
felm_allroads_ndvi_nocropland <- felm(ndvi_nocropland ~ years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data[data$globcover_cropland_2015 %in% 0,])
felm_allroads_ntlbase_ndvi_nocropland <- felm(ndvi_nocropland ~ years_since_improved_all_bin*factor(dmspols_1997_group)-factor(dmspols_1997_group)-years_since_improved_all_bin | cell_id + year | 0 | GADM_ID_3, data=data[data$globcover_cropland_2015 %in% 0,]) 

felm_blabv50roads_ndvi_nocropland <- felm(ndvi_nocropland ~ years_since_improved_below50_bin + years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin) & data$globcover_cropland_2015 %in% 0,])
felm_blabv50roads_ntlbase_ndvi_nocropland <- felm(ndvi_nocropland ~ years_since_improved_below50_bin*factor(dmspols_1997_group) + 
                                                  years_since_improved_50above_bin*factor(dmspols_1997_group) -
                                                  factor(dmspols_1997_group) -
                                                  years_since_improved_below50_bin - 
                                                  years_since_improved_50above_bin | cell_id + year | 0 | GADM_ID_3, data=data[!is.na(data$years_since_improved_all_bin) & data$globcover_cropland_2015 %in% 0,])  



## Time Since Treatment
felm_allroads_yearssince_dmspols <- felm(dmspols_zhang ~ factor(years_since_improved_all_group) | cell_id + year | 0 | GADM_ID_3, data=data)
felm_allroads_blabv50roads_dmspols <- felm(dmspols_zhang ~ factor(years_since_improved_below50_group) + factor(years_since_improved_50above_group) | cell_id + year | 0 | GADM_ID_3, data=data)

felm_allroads_yearssince_dmspols1 <- felm(dmspols_zhang_1 ~ factor(years_since_improved_all_group) | cell_id + year | 0 | GADM_ID_3, data=data)
felm_allroads_blabv50roads_dmspols1 <- felm(dmspols_zhang_1 ~ factor(years_since_improved_below50_group) + factor(years_since_improved_50above_group) | cell_id + year | 0 | GADM_ID_3, data=data)

felm_allroads_yearssince_dmspols5 <- felm(dmspols_zhang_5 ~ factor(years_since_improved_all_group) | cell_id + year | 0 | GADM_ID_3, data=data)
felm_allroads_blabv50roads_dmspols5 <- felm(dmspols_zhang_5 ~ factor(years_since_improved_below50_group) + factor(years_since_improved_50above_group) | cell_id + year | 0 | GADM_ID_3, data=data)

felm_allroads_yearssince_urban <- felm(gc_urban_mean ~ factor(years_since_improved_all_group) | cell_id + year | 0 | GADM_ID_3, data=data)
felm_allroads_blabv50roads_urban <- felm(gc_urban_mean ~ factor(years_since_improved_below50_group) + factor(years_since_improved_50above_group) | cell_id + year | 0 | GADM_ID_3, data=data)

felm_allroads_yearssince_cropland <- felm(globcover_cropland ~ factor(years_since_improved_all_group) | cell_id + year | 0 | GADM_ID_3, data=data)
felm_allroads_blabv50roads_cropland <- felm(globcover_cropland ~ factor(years_since_improved_below50_group) + factor(years_since_improved_50above_group) | cell_id + year | 0 | GADM_ID_3, data=data)

felm_allroads_yearssince_cropland_irrigated <- felm(globcover_cropland_irrigated ~ factor(years_since_improved_all_group) | cell_id + year | 0 | GADM_ID_3, data=data)
felm_allroads_blabv50roads_cropland_irrigated <- felm(globcover_cropland_irrigated ~ factor(years_since_improved_below50_group) + factor(years_since_improved_50above_group) | cell_id + year | 0 | GADM_ID_3, data=data)

felm_allroads_yearssince_cropland_rainfed <- felm(globcover_cropland_rainfed ~ factor(years_since_improved_all_group) | cell_id + year | 0 | GADM_ID_3, data=data)
felm_allroads_blabv50roads_cropland_rainfed <- felm(globcover_cropland_rainfed ~ factor(years_since_improved_below50_group) + factor(years_since_improved_50above_group) | cell_id + year | 0 | GADM_ID_3, data=data)

felm_allroads_yearssince_cropland_mosaic <- felm(globcover_cropland_mosaic ~ factor(years_since_improved_all_group) | cell_id + year | 0 | GADM_ID_3, data=data)
felm_allroads_blabv50roads_cropland_mosaic <- felm(globcover_cropland_mosaic ~ factor(years_since_improved_below50_group) + factor(years_since_improved_50above_group) | cell_id + year | 0 | GADM_ID_3, data=data)

felm_allroads_yearssince_ndvi <- felm(ndvi ~ factor(years_since_improved_all_group) | cell_id + year | 0 | GADM_ID_3, data=data)
felm_allroads_blabv50roads_ndvi <- felm(ndvi ~ factor(years_since_improved_below50_group) + factor(years_since_improved_50above_group) | cell_id + year | 0 | GADM_ID_3, data=data)

felm_allroads_yearssince_ndvi_cropland <- felm(ndvi_cropland ~ factor(years_since_improved_all_group) | cell_id + year | 0 | GADM_ID_3, data=data[data$globcover_cropland_2015 > 0,])
felm_allroads_blabv50roads_ndvi_cropland <- felm(ndvi_cropland ~ factor(years_since_improved_below50_group) + factor(years_since_improved_50above_group) | cell_id + year | 0 | GADM_ID_3, data=data[data$globcover_cropland_2015 > 0,])

felm_allroads_yearssince_ndvi_nocropland <- felm(ndvi_nocropland ~ factor(years_since_improved_all_group) | cell_id + year | 0 | GADM_ID_3, data=data[data$globcover_cropland_2015 %in% 0,])
felm_allroads_blabv50roads_ndvi_nocropland <- felm(ndvi_nocropland ~ factor(years_since_improved_below50_group) + factor(years_since_improved_50above_group) | cell_id + year | 0 | GADM_ID_3, data=data[data$globcover_cropland_2015 %in% 0,])

# Placebo - Leads
felm_allreaods_placebo_leads_dmspols <- felm(dmspols_zhang ~ factor(years_since_improved_all_group_placebo) | year + cell_id | 0 | GADM_ID_3, data=data)
felm_allreaods_placebo_leads_dmspols_1 <- felm(dmspols_zhang_1 ~ factor(years_since_improved_all_group_placebo) | year + cell_id | 0 | GADM_ID_3, data=data)
felm_allreaods_placebo_leads_dmspols_5 <- felm(dmspols_zhang_5 ~ factor(years_since_improved_all_group_placebo) | year + cell_id | 0 | GADM_ID_3, data=data)
felm_allreaods_placebo_leads_urban <- felm(globcover_urban ~ factor(years_since_improved_all_group_placebo) | year + cell_id | 0 | GADM_ID_3, data=data)
felm_allreaods_placebo_leads_cropland <- felm(globcover_cropland ~ factor(years_since_improved_all_group_placebo) | year + cell_id | 0 | GADM_ID_3, data=data)
felm_allreaods_placebo_leads_ndvi <- felm(ndvi ~ factor(years_since_improved_all_group_placebo) | year + cell_id | 0 | GADM_ID_3, data=data)
felm_allreaods_placebo_leads_ndvi_cropland <- felm(ndvi_nocropland ~ factor(years_since_improved_all_group_placebo) | year + cell_id | 0 | GADM_ID_3, data=data)

stargazer(felm_allroads_placebo_leads_dmspols,
          felm_allroads_placebo_leads_dmspols_1,
          felm_allroads_placebo_leads_dmspols_5,
          felm_allroads_placebo_leads_urban,
          felm_allroads_placebo_leads_cropland,
          felm_allroads_placebo_leads_ndvi,
          felm_allroads_placebo_leads_ndvi_cropland,
          dep.var.labels.include = T,
          dep.var.labels = c("DMSP-OLS","DMSP-OLS $>$ 0","DMSP-OLS $>$ 5","Proportion Urban",
                             "Crop", "NDVI", "NDVI: Crop"),
          dep.var.caption = "",
          covariate.labels = c("Treated", 
                               "Placebo treatment date $t-2$",
                               "Placebo treatment date $t-3$",
                               "Placebo treatment date $t \\leq 4$"),
          omit.stat = c("f","ser"),
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width="-15pt",
          digits=2,
          add.lines = list(
            c("Cell FE", rep("Y", 7)),
            c("Year FE", rep("Y", 7))),
            out = file.path(tables_file_path, paste0("eventstudy_results_placebo_leads",
                                                     "_dataset",dataset,
                                                     "_distthresh",DIST_THRESH,
                                                     ".tex")))



stargazer(felm_allroads_did_dmspols,
          felm_allroads_did_dmspols_1,
          felm_allroads_did_dmspols_5,
          felm_allroads_did_urban,
          felm_allroads_did_cropland,
          felm_allroads_did_ndvi,
          felm_allroads_did_ndvi_cropland,
          dep.var.labels.include = T,
          dep.var.labels = c("DMSP-OLS","DMSP-OLS $>$ 0","DMSP-OLS $>$ 5","Proportion Urban",
                             "Crop", "NDVI", "NDVI: Crop"),
          dep.var.caption = "",
          covariate.labels = c("RSDP Phase 2", 
                               "RSDP Phase 3",
                               "RSDP Phase 4"),
          omit.stat = c("f","ser"),
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width="-15pt",
          digits=2,
          add.lines = list(
            c("Cell FE", rep("Y", 7)),
            c("Year FE", rep("Y", 7))),
          out = file.path(tables_file_path, paste0("eventstudy_results_did_all",
                                                   "_dataset",dataset,
                                                   "_distthresh",DIST_THRESH,
                                                   ".tex")))

stargazer(felm_byroad_did_dmspols,
          felm_byroad_did_dmspols_1,
          felm_byroad_did_dmspols_5,
          felm_byroad_did_urban,
          felm_byroad_did_cropland,
          felm_byroad_did_ndvi,
          felm_byroad_did_ndvi_cropland,
          dep.var.labels.include = T,
          dep.var.labels = c("DMSP-OLS","DMSP-OLS $>$ 0","DMSP-OLS $>$ 5","Proportion Urban",
                             "Crop", "NDVI", "NDVI: Crop"),
          dep.var.caption = "",
          covariate.labels = c("RSDP Phase 2 [Speed 50 Above]", 
                               "RSDP Phase 3 [Speed 50 Above]",
                               "RSDP Phase 4 [Speed 50 Above]",
                               "RSDP Phase 2 [Speed Below 50]", 
                               "RSDP Phase 3 [Speed Below 50]",
                               "RSDP Phase 4 [Speed Below 50]"),
          omit.stat = c("f","ser"),
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width="-15pt",
          digits=2,
          add.lines = list(
            c("Cell FE", rep("Y", 7)),
            c("Year FE", rep("Y", 7))),
          out = file.path(tables_file_path, paste0("eventstudy_results_did_byroad",
                                                   "_dataset",dataset,
                                                   "_distthresh",DIST_THRESH,
                                                   ".tex")))





stargazer(felm_allroads_cropland,
          felm_blabv50roads_cropland,
          felm_allroads_ntlbase_cropland,
          felm_blabv50roads_ntlbase_cropland,

          felm_allroads_cropland_rainfed,
          felm_blabv50roads_cropland_rainfed,
          felm_allroads_ntlbase_cropland_rainfed,
          felm_blabv50roads_ntlbase_cropland_rainfed,
          
          felm_allroads_cropland_irrigated,
          felm_blabv50roads_cropland_irrigated,
          felm_allroads_ntlbase_cropland_irrigated,
          felm_blabv50roads_ntlbase_cropland_irrigated,
          
          felm_allroads_cropland_mosaic,
          felm_blabv50roads_cropland_mosaic,
          felm_allroads_ntlbase_cropland_mosaic,
          felm_blabv50roads_ntlbase_cropland_mosaic,
          dep.var.labels.include = T,
          dep.var.labels = c("Crop","Crop: Irrig","Crop: Rain","Crop: Mosaic"),
          dep.var.caption = "",
          covariate.labels = covariate.labels,
          omit.stat = c("f","ser"),
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width="-15pt",
          digits=2,
          add.lines = list(
            c("Cell FE", rep("Y", 16)),
            c("Year FE", rep("Y", 16))
          ),
          out = file.path(tables_file_path, paste0("eventstudy_results_crop",
                                                   "_dataset",dataset,
                                                   "_distthresh",DIST_THRESH,
                                                   ".tex")))

stargazer(felm_allroads_ndvi,
          felm_blabv50roads_ndvi,
          felm_allroads_ntlbase_ndvi,
          felm_blabv50roads_ntlbase_ndvi,

          felm_allroads_ndvi_cropland,
          felm_blabv50roads_ndvi_cropland,
          felm_allroads_ntlbase_ndvi_cropland,
          felm_blabv50roads_ntlbase_ndvi_cropland,
          
          felm_allroads_ndvi_nocropland,
          felm_blabv50roads_ndvi_nocropland,
          felm_allroads_ntlbase_ndvi_nocropland,
          felm_blabv50roads_ntlbase_ndvi_nocropland,

          dep.var.labels.include = T,
          dep.var.labels = c("NDVI","NDVI: Crop", "NDVI: No Crop"),
          dep.var.caption = "",
          covariate.labels = covariate.labels,
          omit.stat = c("f","ser"),
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width="-15pt",
          digits=2,
          add.lines = list(
            c("Cell FE", rep("Y", 12)),
            c("Year FE", rep("Y", 12))
          ),
          out = file.path(tables_file_path, paste0("eventstudy_results_ndvi",
                                                   "_dataset",dataset,
                                                   "_distthresh",DIST_THRESH,
                                                   ".tex")))


stargazer(felm_allroads_placebo_dmspols_yearfe,
          felm_allroads_placebo_dmspols_yearlinear,
          felm_allroads_placebo_dmspols1_yearfe,
          felm_allroads_placebo_dmspols1_yearlinear,
          felm_allroads_placebo_dmspols5_yearfe,
          felm_allroads_placebo_dmspols5_yearlinear,
          felm_allroads_placebo_urban_yearfe,
          felm_allroads_placebo_urban_yearlinear,
          
          felm_allroads_placebo_cropland_yearfe,
          felm_allroads_placebo_cropland_yearlinear,
          
          felm_allroads_placebo_ndvi_yearfe,
          felm_allroads_placebo_ndvi_yearlinear,
          
          felm_allroads_placebo_ndvi_cropland_yearfe,
          felm_allroads_placebo_ndvi_cropland_yearlinear,
          
          dep.var.labels.include = T,
          dep.var.labels = c("DMSP-OLS","DMSP-OLS $>$ 0","DMSP-OLS $>$ 5","Proportion Urban",
                             "Crop", "NDVI", "NDVI: Crop"),
          dep.var.caption = "",
          covariate.labels = c("Imp. Rd, Any", "Year"),
          omit.stat = c("f","ser"),
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width="-1pt",
          digits=2,
          add.lines = list(
            c("Cell FE", rep("Y",14)),
            c("Year FE", rep(c("Y","N"), 7))
          ),
          out = file.path(tables_file_path, paste0("eventstudy_results_placebo",
                                                   "_dataset",dataset,
                                                   "_distthresh",DIST_THRESH,
                                                   ".tex")))


stargazer(felm_allroads,
          felm_blabv50roads,
          felm_allroads_ntlbase,
          felm_blabv50roads_ntlbase,
          felm_allroads_dmspols1,
          felm_blabv50roads_dmspols1,
          felm_allroads_ntlbase_dmspols1,
          felm_blabv50roads_ntlbase_dmspols1,
          felm_allroads_dmspols5,
          felm_blabv50roads_dmspols5,
          felm_allroads_ntlbase_dmspols5,
          felm_blabv50roads_ntlbase_dmspols5,
          felm_allroads_urban,
          felm_blabv50roads_urban,
          felm_allroads_ntlbase_urban,
          felm_blabv50roads_ntlbase_urban,
          dep.var.labels.include = T,
          dep.var.labels = c("DMSP-OLS","DMSP-OLS $>$ 0","DMSP-OLS $>$ 5","Proportion Urban"),
          dep.var.caption = "",
          covariate.labels = covariate.labels,
          omit.stat = c("f","ser"),
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width="-15pt",
          digits=2,
          add.lines = list(
            c("Cell FE", rep("Y", 16)),
            c("Year FE", rep("Y", 16))
          ),
          out = file.path(tables_file_path, paste0("eventstudy_results",
                                                   "_dataset",dataset,
                                                   "_distthresh",DIST_THRESH,
                                                   ".tex")))


stargazer(felm_allroads_yearssince_dmspols,
          felm_allroads_blabv50roads_dmspols,
          felm_allroads_yearssince_dmspols1,
          felm_allroads_blabv50roads_dmspols1,
          felm_allroads_yearssince_dmspols5,
          felm_allroads_blabv50roads_dmspols5,
          felm_allroads_yearssince_urban,
          felm_allroads_blabv50roads_urban,
          dep.var.labels.include = T,
          dep.var.labels = c("DMSP-OLS","DMSP-OLS $>$ 0","DMSP-OLS $>$ 5","Prop. Urban"),
          dep.var.caption = "",
          covariate.labels = c("Imp Rd, Any: Yr 0",
                               "Imp Rd, Any: Yr 1",
                               "Imp Rd, Any: Yr 2",
                               "Imp Rd, Any: Yr 3",
                               "Imp Rd, Any: Yr $\\geq$ 4",
                               "Imp. Rd $<$ 50km/hr: Yr 0",
                               "Imp. Rd $<$ 50km/hr: Yr 1",
                               "Imp. Rd $<$ 50km/hr: Yr 2",
                               "Imp. Rd $<$ 50km/hr: Yr 3",
                               "Imp. Rd $<$ 50km/hr: Yr $\\geq$ 4",
                               "Imp. Rd $\\geq$ 50km/hr: Yr 0",
                               "Imp. Rd $\\geq$ 50km/hr: Yr 1",
                               "Imp. Rd $\\geq$ 50km/hr: Yr 2",
                               "Imp. Rd $\\geq$ 50km/hr: Yr 3",
                               "Imp. Rd $\\geq$ 50km/hr: Yr $\\geq$ 4"),
          omit.stat = c("f","ser"),
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width="-1pt",
          digits=2,
          add.lines = list(
            c("Cell FE", rep("Y", 8)),
            c("Year FE", rep("Y", 8))
          ),
          out = file.path(tables_file_path, paste0("eventstudy_results_yearssince",
                                                   "_dataset",dataset,
                                                   "_distthresh",DIST_THRESH,
                                                   ".tex")))

stargazer(felm_allroads_yearssince_cropland,
          felm_allroads_blabv50roads_cropland,
          felm_allroads_yearssince_cropland_irrigated,
          felm_allroads_blabv50roads_cropland_irrigated,
          felm_allroads_yearssince_cropland_rainfed,
          felm_allroads_blabv50roads_cropland_rainfed,
          felm_allroads_yearssince_cropland_mosaic,
          felm_allroads_blabv50roads_cropland_mosaic,
          felm_allroads_yearssince_ndvi,
          felm_allroads_blabv50roads_ndvi,
          felm_allroads_yearssince_ndvi_cropland,
          felm_allroads_blabv50roads_ndvi_cropland,
          felm_allroads_yearssince_ndvi_nocropland,
          felm_allroads_blabv50roads_ndvi_nocropland,
          dep.var.labels.include = T,
          dep.var.labels = c("Crop","Crop: Irrig","Crop: Rain","Crop: Mosaic", "NDVI", "NDVI: Crop", "NDVI: No Crop"),
          dep.var.caption = "",
          covariate.labels = c("Imp Rd, Any: Yr 0",
                               "Imp Rd, Any: Yr 1",
                               "Imp Rd, Any: Yr 2",
                               "Imp Rd, Any: Yr 3",
                               "Imp Rd, Any: Yr $\\geq$ 4",
                               "Imp. Rd $<$ 50km/hr: Yr 0",
                               "Imp. Rd $<$ 50km/hr: Yr 1",
                               "Imp. Rd $<$ 50km/hr: Yr 2",
                               "Imp. Rd $<$ 50km/hr: Yr 3",
                               "Imp. Rd $<$ 50km/hr: Yr $\\geq$ 4",
                               "Imp. Rd $\\geq$ 50km/hr: Yr 0",
                               "Imp. Rd $\\geq$ 50km/hr: Yr 1",
                               "Imp. Rd $\\geq$ 50km/hr: Yr 2",
                               "Imp. Rd $\\geq$ 50km/hr: Yr 3",
                               "Imp. Rd $\\geq$ 50km/hr: Yr $\\geq$ 4"),
          omit.stat = c("f","ser"),
          align=TRUE,
          no.space=TRUE,
          float=FALSE,
          column.sep.width="-1pt",
          digits=2,
          add.lines = list(
            c("Cell FE", rep("Y", 14)),
            c("Year FE", rep("Y", 14))
          ),
          out = file.path(tables_file_path, paste0("eventstudy_results_yearssince_crop",
                                                   "_dataset",dataset,
                                                   "_distthresh",DIST_THRESH,
                                                   ".tex")))
  
  
  
  
