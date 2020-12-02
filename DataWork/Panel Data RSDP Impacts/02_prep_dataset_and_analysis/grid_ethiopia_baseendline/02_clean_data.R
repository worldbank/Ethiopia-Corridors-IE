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
  mutate(dmspols_zhang_1996 = dmspols_zhang[year == 1996]) %>%
  ungroup()

## NTL in Lit Cells
data$dmspols_zhang_ihs_base0na <- data$dmspols_zhang_ihs
data$dmspols_zhang_ihs_base0na[data$dmspols_zhang_1996 %in% 0] <- NA

# Export -----------------------------------------------------------------------
saveRDS(data, file.path(panel_rsdp_imp_data_file_path, "dmspols_grid_ethiopia",
                          "merged_datasets", "panel_data_clean.Rds"))
write_dta(data, file.path(panel_rsdp_imp_data_file_path, "dmspols_grid_ethiopia",
                        "merged_datasets", "panel_data_clean.dta"))





