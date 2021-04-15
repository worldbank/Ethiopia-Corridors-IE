# Create Modjo-Hawassa Expressway Polyline

# Derived from RSDP Cleaned Shapefile

# Load Data --------------------------------------------------------------------
roads <- readRDS(file.path(project_file_path, "Data", "RSDP Roads", "FinalData", "RoadNetworkPanelData_1996_2016.Rds"))

# Restrict to Modjo-Hawassa ----------------------------------------------------
roads <- roads[roads$LINKNAME %in% c("Modjo - Ziway",
                                     "Ziway - Shashemene",
                                     "Shashemene - Awassa"),]

roads <- roads %>% st_as_sf() %>% st_union() %>% as("Spatial")
roads$name <- "Modjo-Hawassa"

# Export -----------------------------------------------------------------------
saveRDS(roads, file.path(project_file_path, "Data", "RSDP Roads", "FinalData", "modjo_hawassa.Rds"))

roads_sf <- roads %>% st_as_sf()
write_sf(roads_sf, file.path(project_file_path, "Data", "RSDP Roads", "FinalData", "modjo_hawassa.geojson"),
         delete_dsn = T)




