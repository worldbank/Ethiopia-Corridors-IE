# Clean RSDP Road Shapefile

# Load Road Data ---------------------------------------------------------------
panel <- readRDS(file.path(project_file_path, "Data", "FinalData", "roads", "RoadNetworkPanelData_1996_2016.Rds"))
panel_2008 <- panel[panel$Speed2008 > 0,]

sep_2008 <- readOGR(dsn = file.path(project_file_path, "Data","RawData", "RoadNetworkPanelDataV3_1996_2016_Revised"), 
                      layer = paste0("All_Network_",2008))
sep_2008 <- spTransform(sep_2008, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#panel_2014@data <- panel_2014@data %>%
#  dplyr::select(LINKID, Speed2008)

#sep_2014@data <- sep_1996@data %>%
#  dplyr::select(LINKID, Speed2014)

# Export -----------------------------------------------------------------------
st_write(panel_2008 %>% st_as_sf(), "~/Desktop/panel2008.kml")
st_write(sep_2008 %>% st_as_sf(), "~/Desktop/sep2008.kml")

st_write(panel_2008 %>% st_as_sf(), "~/Desktop/panel2008.geojson")
st_write(sep_2008 %>% st_as_sf(), "~/Desktop/sep2008.geojson")


