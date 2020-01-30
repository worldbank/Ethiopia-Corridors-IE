# Travel Time

# https://www.rdocumentation.org/packages/raster/versions/1.4-10/topics/linesToRaster

# Load Data --------------------------------------------------------------------
roads <- readRDS(file.path(project_file_path, "Data", "FinalData", "roads", "RoadNetworkPanelData_1996_2016.Rds"))
ethiopia <- readRDS(file.path(project_file_path, "Data", "RawData", "GADM", "gadm36_ETH_0_sp.rds"))

# Reproject to Ethiopia Projection ---------------------------------------------
# Reproject to UTM. Better for distance calculations (eg, for setting grid cell size)
roads <- spTransform(roads, UTM_ETH)
eth1iopia <- spTransform(ethiopia, UTM_ETH)

# Crete Raster BaseLayer -------------------------------------------------------
r <- raster(xmn=ethiopia@bbox[1,1], 
           xmx=ethiopia@bbox[1,2], 
           ymn=ethiopia@bbox[2,1], 
           ymx=ethiopia@bbox[2,2], 
       crs=UTM_ETH, 
       resolution = 50*1000)

r_sp <- as(r, "SpatialPolygonsDataFrame")















