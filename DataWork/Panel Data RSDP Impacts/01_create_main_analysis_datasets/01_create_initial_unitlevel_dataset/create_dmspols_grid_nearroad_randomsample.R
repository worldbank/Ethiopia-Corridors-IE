# Create Points at DMSPOLS Level

set.seed(42)

# Load Data --------------------------------------------------------------------
dmspols_points <- readRDS(file.path(panel_rsdp_imp_data_file_path, "dmspols_grid_nearroad", "individual_datasets","points.Rds"))
dmspols_poly <- readRDS(file.path(panel_rsdp_imp_data_file_path, "dmspols_grid_nearroad", "individual_datasets","polygons.Rds"))

# Random Sample ----------------------------------------------------------------
keep_cells <- sample(x = c(TRUE, FALSE), 
                     prob = c(.02, .98),
                     size = nrow(dmspols_points), 
                     replace = T)

dmspols_points <- dmspols_points[keep_cells,]
dmspols_poly   <- dmspols_poly[keep_cells,]

# Export -----------------------------------------------------------------------
saveRDS(dmspols_points, file.path(panel_rsdp_imp_data_file_path, "dmspols_grid_nearroad_randomsample", "individual_datasets","points.Rds"))
saveRDS(dmspols_poly,   file.path(panel_rsdp_imp_data_file_path, "dmspols_grid_nearroad_randomsample", "individual_datasets","polygons.Rds"))



