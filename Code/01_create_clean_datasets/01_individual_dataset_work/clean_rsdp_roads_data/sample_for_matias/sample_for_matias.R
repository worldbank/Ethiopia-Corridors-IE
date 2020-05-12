# Create Sample Dataset for Matias

roads <- readRDS(file.path(project_file_path, "Data", "FinalData", "roads", "RoadNetworkPanelData_1996_2016.Rds"))
roads@data <- roads@data %>%
  dplyr::select(LINKID, LINKNAME, LINKLENGTH, REGION, WOERDA, SURFACETYP, ROADCLASS, Classifi_m,
                SURFACETYP_yearshape_1996, SURFACETYP_yearshape_2006, 
                Speed1996, Speed2006, Speed2016) %>%
  dplyr::rename(Surface2016 = SURFACETYP) %>%
  mutate(uid = 1:n())

##
roads_sf <- st_as_sf(roads)
st_write(roads_sf, file.path(project_file_path, "Data", "FinalData", "roads", "samples", "eth_roads.geojson"), delete_dsn=T)

##
roads_uid <- roads
roads_uid@data <- roads_uid@data %>%
  dplyr::select(uid)
writeOGR(roads_uid,
        dsn = file.path(project_file_path, "Data", "FinalData", "roads", "samples"),
        layer = "eth_roads",
        driver = "ESRI Shapefile")
write.csv(roads@data, file.path(project_file_path, "Data", "FinalData", "roads", "samples", "eth_roads.csv"), row.names=F)

