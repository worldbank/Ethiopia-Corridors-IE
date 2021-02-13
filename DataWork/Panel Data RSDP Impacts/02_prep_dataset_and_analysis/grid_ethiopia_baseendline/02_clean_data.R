# Clean Data

# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(panel_rsdp_imp_data_file_path, "dmspols_grid_ethiopia",
                          "merged_datasets", "panel_data.Rds"))

#data <- data[data$cell_id %in% unique(data$cell_id)[1:5000],] ## for testing

# Create Varibles --------------------------------------------------------------
calc_ihs <- function(x) log(x + sqrt(x^2 + 1))

data <- data %>%
  mutate(
    # DMSP - IHS
    dmspols_ihs = calc_ihs(dmspols),
    dmspols_zhang_ihs = calc_ihs(dmspols_zhang),
    
    # DMSP - Binary
    dmspols_zhang_2 = as.numeric(dmspols_zhang >= 2),
    dmspols_zhang_6 = as.numeric(dmspols_zhang >= 6),
    
    dmspols_2 = as.numeric(dmspols >= 2),
    dmspols_6 = as.numeric(dmspols >= 6),
    
    # Define "Near Road" Variables
    near_anyimproved_ever_5km = as.numeric(distance_anyimproved_ever <= 5*1000),
    near_anyimproved_by2012_5km = as.numeric(distance_anyimproved_by2012 <= 5*1000),
    
    near_anyroad2012_5km = as.numeric(distance_anyroad2012 <= 5*1000),
    near_anyroad2016_5km = as.numeric(distance_anyroad2016 <= 5*1000),
    
    near_mst_5km = as.numeric(distance_mst <= 5*1000),
    near_mst_mindist_5km = as.numeric(distance_mst_mindist <= 5*1000),
    
    # Endline
    endline = as.numeric(year %in% c(2012, 2016))
    )

## Baseline values
data <- data %>%
  group_by(cell_id) %>%
  mutate(dmspols_zhang_1996 = dmspols_zhang[year == 1996],
         dmspols_1996 = dmspols[year == 1996],
         dmspols_2_1996 = dmspols_2[year == 1996],
         dmspols_6_1996 = dmspols_6[year == 1996],
         dmspols_zhang_2_1996 = dmspols_zhang_2[year == 1996],
         dmspols_zhang_6_1996 = dmspols_zhang_6[year == 1996]) %>%
  ungroup()

data <- data %>%
  group_by(woreda_id) %>%
  mutate(dmspols_zhang_1996_woreda = mean(dmspols_zhang_1996, na.rm = T),
         dmspols_1996_woreda = mean(dmspols_1996, na.rm = T),
         dmspols_sum2_1996_woreda = sum(dmspols_2_1996, na.rm = T),
         dmspols_sum6_1996_woreda = sum(dmspols_6_1996, na.rm = T),
         dmspols_zhang_sum2_1996_woreda = sum(dmspols_zhang_2_1996, na.rm = T),
         dmspols_zhang_sum6_1996_woreda = sum(dmspols_zhang_6_1996, na.rm = T)) %>%
  ungroup()

data$dmspols_zhang_ihs_1996_woreda <- calc_ihs(data$dmspols_zhang_1996_woreda)
data$dmspols_ihs_1996_woreda       <- calc_ihs(data$dmspols_1996_woreda)
data$dmspols_zhang_ihs_sum2_1996_woreda       <- calc_ihs(data$dmspols_zhang_sum2_1996_woreda)
data$dmspols_zhang_ihs_sum6_1996_woreda       <- calc_ihs(data$dmspols_zhang_sum6_1996_woreda)

## NTL Categories
data$dmspols_1996_bin3 <- NA
data$dmspols_1996_bin3[data$dmspols_sum2_1996_woreda %in% 0] <- 1
data$dmspols_1996_bin3[data$dmspols_sum2_1996_woreda > 0]    <- 2
data$dmspols_1996_bin3[data$dmspols_sum6_1996_woreda > 0]    <- 3

data$dmspols_1996_bin3_1 <-  as.numeric(data$dmspols_1996_bin3 == 1)
data$dmspols_1996_bin3_2 <-  as.numeric(data$dmspols_1996_bin3 == 2)
data$dmspols_1996_bin3_3 <-  as.numeric(data$dmspols_1996_bin3 == 3)

## NTL lit at baseline
data$dmspols_zhang_base0na <- data$dmspols_zhang
data$dmspols_zhang_base0na[data$dmspols_zhang_1996 %in% 0] <- NA

data$dmspols_zhang_ihs_base0na <- data$dmspols_zhang_ihs
data$dmspols_zhang_ihs_base0na[data$dmspols_zhang_1996 %in% 0] <- NA

# Export -----------------------------------------------------------------------
saveRDS(data, file.path(panel_rsdp_imp_data_file_path, "dmspols_grid_ethiopia",
                          "merged_datasets", "panel_data_clean.Rds"))






