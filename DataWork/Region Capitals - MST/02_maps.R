# Create Minimal Spanning Tree

# Load Data --------------------------------------------------------------------
mst_lc <- readRDS(
        file.path(data_file_path, "Region Capitals - MST", "FinalData", 
                  "region_capitals_leastcost_mst.Rds"))

mst_euc <- readRDS(
        file.path(data_file_path, "Region Capitals - MST", "FinalData", 
                  "region_capitals_eucdist_mst.Rds"))
mst_euc <- spTransform(mst_euc, CRS("+init=epsg:4326"))

roads_sdf <- readRDS(file.path(data_file_path, "RSDP Roads", "FinalData", "RoadNetworkPanelData_1996_2016.Rds"))
roads_sdf$id <- 1 # useful to have a variable the same for all obs when aggreagting roads later
roads_sdf_p123 <- roads_sdf[roads_sdf$Complete_G %in% 1997:2009,]


# Map --------------------------------------------------------------------------
leaflet() %>%
  addTiles() %>%
  addPolylines(data = roads_sdf_p123, color = "orange", opacity = 0.4) %>%
  addPolylines(data = mst_lc) %>%
  addPolylines(data = mst_euc, color = "red")


plot(mst_lc)
plot(mst_euc)
