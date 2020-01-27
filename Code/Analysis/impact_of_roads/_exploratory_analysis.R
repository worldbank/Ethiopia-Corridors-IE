# Exploratory Analysis

# APPROACH: Somewhat follow the Haiti paper. There it seems road improvement
# kinda random throughout their years. Here that's not the case, but maybe
# can assume that within an RDSP phase?

# OUTLINE
# 1. Overall impact of each phase.
# 2. Heterogeneity of impact, within each phase
#    2.1. Road type 
#    2.2. Baseline Dep Var (for ntl, num or divide into thirds a la aiddata?)
#    2.3. Distance to City (could break down by city pop)

# DEPENDENT VARIABLES
# 1. dmspols_zhang_ihs
# 2. globcover_urban
# 3. Cropland? NDVI?

# PRESENT RESULTS
# 1. Post-treatment
# 2. Coef-plots. In same plot. All cells and (for hetro), below/above cutoffs (median / quartiles). Super important to see pre-trends.

# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "merged_datasets", "grid_data_clean.Rds"))

# Overal Results ---------------------------------------------------------------
lm_result <- felm(globcover_urban ~ years_since_improved | year + cell_id | 0 | GADM_ID_3, data=data) 

data$years_since_improved_50aboveafter %>% table()

# Improved by RDSP -------------------------------------------------------------
# RDSP I - From July 1997 - June 2002 [1997 - 2002]
# RDSP II - From July 2002 to June 2007 [2003 - 2007]
# RDSP III - From July 2007 to June 2010 [2008 - 2010]
# RDSP IV - From July 2010 to June 2015 [2011 - 2015]
# RDSP V - From July 2015 to June 2020 [2016 - 2020]

data_rdsp_i <- data[data$year_improved %in% 1997:2002,]
data_rdsp_ii <- data[data$year_improved %in% 2003:2007,]
data_rdsp_iii <- data[data$year_improved %in% 2008:2010,]
data_rdsp_iv <- data[data$year_improved %in% 2011:2015,]
data_rdsp_v <- data[data$year_improved %in% 2016:2020,]

# Globcover Urban
lm_rdspiii_urban <- felm(globcover_urban ~ post_improved | year + cell_id | 0 | GADM_ID_3, data=data_rdsp_iii) 
lm_rdspiii_urban_baseline <- felm(globcover_urban ~ post_improved*globcover_urban_1996 - globcover_urban_1996 | year + cell_id | 0 | GADM_ID_3, data=data_rdsp_iii) 

lm_rdspiii_urban_years <- felm(globcover_urban ~ years_since_improved | year + cell_id | 0 | GADM_ID_3, data=data_rdsp_iii) 

summary(lm_rdspiii_urban)
summary(lm_rdspiii_urban_baseline)
summary(lm_rdspiii_urban_years)

# DMSP-OLS
data_rsdp_temp <- data_rdsp_ii
lm_rdspiii_dmspols_zhang <- felm(dmspols_zhang_ihs ~ post_improved | year + cell_id | 0 | GADM_ID_3, data=data_rsdp_temp) 
lm_rdspiii_dmspols_zhang_baseline <- felm(dmspols_zhang_ihs ~ post_improved*dmspols_zhang_1996_ihs - dmspols_zhang_1996_ihs | year + cell_id | 0 | GADM_ID_3, data=data_rsdp_temp) 
lm_rdspiii_dmspols_zhang_baselinegroup <- felm(dmspols_zhang_ihs ~ post_improved*dmspols_zhang_1996_group - dmspols_zhang_1996_group | year + cell_id | 0 | GADM_ID_3, data=data_rsdp_temp) 

lm_rdspiii_dmspols_zhang_years <- felm(dmspols_zhang_ihs ~ years_since_improved | year + cell_id | 0 | GADM_ID_3, data=data_rsdp_temp) 
lm_rdspiii_dmspols_zhang_years_base1 <- felm(dmspols_zhang_ihs ~ years_since_improved | year + cell_id | 0 | GADM_ID_3, data=data_rsdp_temp[data_rsdp_temp$dmspols_zhang_1996_group %in% 1,]) 
lm_rdspiii_dmspols_zhang_years_base2 <- felm(dmspols_zhang_ihs ~ years_since_improved | year + cell_id | 0 | GADM_ID_3, data=data_rsdp_temp[data_rsdp_temp$dmspols_zhang_1996_group %in% 2,]) 
lm_rdspiii_dmspols_zhang_years_base3 <- felm(dmspols_zhang_ihs ~ years_since_improved | year + cell_id | 0 | GADM_ID_3, data=data_rsdp_temp[data_rsdp_temp$dmspols_zhang_1996_group %in% 3,]) 

summary(lm_rdspiii_dmspols_zhang)
summary(lm_rdspiii_dmspols_zhang_baseline)
summary(lm_rdspiii_dmspols_zhang_baselinegroup)

summary(lm_rdspiii_dmspols_zhang_years)
summary(lm_rdspiii_dmspols_zhang_years_base1)
summary(lm_rdspiii_dmspols_zhang_years_base2)
summary(lm_rdspiii_dmspols_zhang_years_base3)



lm_rdspiii_urban_baseline <- felm(globcover_urban ~ post_improved*globcover_urban_1996 - globcover_urban_1996 | year + cell_id | 0 | 0, data=data_rdsp_iii) 
summary(lm_rdspiii_urban_baseline)

lm_rdspiii_urban_baseline <- felm(globcover_cropland ~ post_improved*globcover_cropland_1996 - globcover_cropland_1996 | year + cell_id | 0 | 0, data=data_rdsp_iii) 
summary(lm_rdspiii_urban_baseline)

# Restrict to cells that were improved at some point where have data. For example,
# if using DMSP-OLS only have data until 2012, so only want areas near improved 
# roads that were improved until 2012. 

lm_test <- felm(globcover_urban ~ years_since_improved_50above | year + cell_id | 0 | GADM_ID_3, data=data)
summary(lm_test)


head(data)

