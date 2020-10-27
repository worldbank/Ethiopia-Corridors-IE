# Prep data for long difference

# Uses clean panel data

NEAR_THRESHOLD <- 0 # 0 meters, meaning woreda must intersect

# Load Data / Create Dataset Lists ---------------------------------------------
data <- readRDS(file.path(panel_rsdp_imp_data_file_path, "woreda", "merged_datasets", "panel_data_clean.Rds"))

# Road Variables ---------------------------------------------------------------
# Variables for road length above Xkm in each year


# Export -----------------------------------------------------------------------
saveRDS(data, file.path(panel_rsdp_imp_data_file_path, "woreda", "merged_datasets", "longdiff_data_clean.Rds"))
write_dta(data, file.path(panel_rsdp_imp_data_file_path, "woreda", "merged_datasets", "longdiff_data_clean.dta"))






