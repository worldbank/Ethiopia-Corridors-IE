# Woreda Summary Trends

# Woreda Data ------------------------------------------------------------------
woreda_blank <- readRDS(file.path(panel_rsdp_imp_data_file_path, "woreda", "individual_datasets", "polygons_no_road_cut.Rds"))

woreda_blank <- spTransform(woreda_blank, UTM_ETH)

areas <- woreda_blank %>% gArea(byid=T) 

areas_km <- areas/(1000^2) 
summary(areas_km)
sum(areas_km)
