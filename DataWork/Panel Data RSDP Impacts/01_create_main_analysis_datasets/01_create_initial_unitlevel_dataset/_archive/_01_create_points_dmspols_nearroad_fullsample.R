# Create Points at DMSPOLS Level

# Creates a blank (1) point dataset and (2) polygon dataset of grids at the DMSP-OLS
# level (ie, 1x1km grid). These datasets contain a unique ID and spatial information.
# The points file is saved as a dataframe with lat/lon while the poylgon is saved
# as a spatial features object.

# The script also generates a dataset that includes a panel of DMSP-OLS data. It 
# is done in this script as the process for preparing the above files makes
# creating this one quick

# NOTE: This script takes about 20 minutes to run.

# Load Data --------------------------------------------------------------------
dmspols <- raster(file.path(rawdata_file_path, "Nighttime Lights", "DMSP_OLS","Stacked", "eth_dmspols_allyears.tif"),1)
roads <- readRDS(file.path(project_file_path, "Data", "FinalData", "roads", "RoadNetworkPanelData_1996_2016.Rds"))
eth_adm3 <- readRDS(file.path(rawdata_file_path, "GADM", "gadm36_ETH_3_sp.rds"))

# Determine Pixels to Keep Based on Location -----------------------------------
# Creates a vector of cells_to_keep that is TRUE when we should keep cells. Don't
# directly restrict cells yet as using raster format. Subsetting comes in later
# step after polygonizing and making point file.

#### Restrict to Ethiopia Based on GEE
# Only keep cells without value for NTL. Removes area not in Ethiopia according to GEE;
# this is because GEE only exported NTL for areas in the country.
dmspols_df <- dmspols[] %>% as.data.frame
cells_to_keep <- !is.na(dmspols_df$.)

#### Restrict to Ethiopia Based on GADM
# Only keep cells within boundary of Ethiopia according to GADM
dmspols_coords <- coordinates(dmspols) %>% 
  as.data.frame %>%
  dplyr::rename(long = x) %>%
  dplyr::rename(lat = y) %>%
  mutate(id = 1:n())
coordinates(dmspols_coords) <- ~long+lat
crs(dmspols_coords) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

points_OVER_gadm <- sp::over(dmspols_coords, eth_adm3)

cells_to_keep <- cells_to_keep & !is.na(points_OVER_gadm$NAME_0)

#### Restrict to Near Road
# Restrict roads to consider
#roads <- roads[roads$Complete_G >= 1997,]

# Simplify roads to make buffering go faster
roads_s <- gSimplify(roads, tol=.001)
roads_s$id <- 1:length(roads_s)
roads_buff <- gBuffer_chunks(roads_s, width=7/111.12, 50)
roads_buff$id <- 1
roads_buff_agg <- raster::aggregate(roads_buff, by="id")

points_OVER_road <- over_chunks(dmspols_coords, roads_buff_agg, "sum", 20000)

cells_to_keep <- cells_to_keep & !is.na(points_OVER_road$id)

# Keep Random Sample of Cells --------------------------------------------------
# We restrict the sample to an X percent sample. We further refine the cells_to_keep
# vector.

#cells_to_keep[cells_to_keep %in% TRUE] <- sample(x = c(TRUE, FALSE),
#                                                 size = length(cells_to_keep[cells_to_keep %in% TRUE]),
#                                                 replace = T,
#                                                 prob = c(PROPORTION_SAMPLE, 1-PROPORTION_SAMPLE))

# Individual Points ------------------------------------------------------------
dmspols_coords <- coordinates(dmspols) %>% 
  as.data.frame %>%
  dplyr::rename(long = x) %>%
  dplyr::rename(lat = y)
dmspols_coords <- dmspols_coords[cells_to_keep,]
dmspols_coords$cell_id <- 1:nrow(dmspols_coords)

saveRDS(dmspols_coords, file.path(finaldata_file_path, "dmspols_grid_dataset_nearroad", "individual_datasets","points.Rds"))
write_csv(dmspols_coords, file.path(finaldata_file_path, "dmspols_grid_dataset_nearroad", "individual_datasets","points.csv"))

# Polygon of Points ------------------------------------------------------------
dmspols_poly <- polygonize(dmspols, na.rm=F)
dmspols_poly <- dmspols_poly[cells_to_keep,]
dmspols_poly$cell_id <- 1:nrow(dmspols_poly)
dmspols_poly$eth_dmspols_allyears <- NULL

saveRDS(dmspols_poly, file.path(finaldata_file_path, "dmspols_grid_dataset_nearroad", "individual_datasets","polygons.Rds"))

# DMSP Panel -------------------------------------------------------------------
# Create datsaet of DMSPOLS data. Easier to do here because if the cells_to_keep
# variable
dmspols_panel <- lapply(1:22, function(i){
  print(i)
  dmspols_i <- raster(file.path(rawdata_file_path, "Nighttime Lights", "DMSP_OLS","Stacked", "eth_dmspols_allyears.tif"),i)[] %>% 
    as.data.frame %>%
    dplyr::rename(dmspols = ".")
  dmspols_i$year <- i + 1991
  
  dmspols_i <- dmspols_i[cells_to_keep,]
  dmspols_i$cell_id <- 1:nrow(dmspols_i)
  
  return(dmspols_i)
}) %>%
  bind_rows

saveRDS(dmspols_panel, file.path(finaldata_file_path, "dmspols_grid_dataset_nearroad", "individual_datasets","points_dmspols.Rds"))

