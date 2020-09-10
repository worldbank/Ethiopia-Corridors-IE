# Create Minimal Spanning Tree

# Load Data --------------------------------------------------------------------
## Woredas
woreda <- readOGR(dsn = file.path(rawdata_file_path, "woreda_population", "HDX_CSA"), layer = "Ethioworeda")
woreda <- spTransform(woreda, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
woreda$uid <- 1:nrow(woreda)

## Ethiopia ADM boundary
# The "woreda" file has holes for water bodies
eth <- readRDS(file.path(rawdata_file_path, "GADM", "gadm36_ETH_0_sp.rds")) %>%
  gBuffer(width = 10/111.12)

## Population
gpw <- raster(file.path(rawdata_file_path, "gpw-v4-population-density-2000", "gpw-v4-population-density_2000.tif"))
gpw <- gpw %>% crop(eth)

## Elevation/Slope
elevation <- raster(file.path(rawdata_file_path, "elevation", "gee_download", "eth_elevation_1000m.tif"))
slope <- terrain(elevation, opt="slope", unit="degrees",neighbors=8)

## Globcover
land_cover <- raster(file.path(rawdata_file_path, "esa_globcover", "scratch", "ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif"), 1) %>%
  crop(eth)

land_cover_urban <- calc(land_cover, function(x) x %in% 190)
land_cover_wetland <- calc(land_cover, function(x) x %in% 180)
land_cover_water <- calc(land_cover, function(x) x %in% 210)

land_cover_urban <- resample(land_cover_urban, slope)
land_cover_wetland <- resample(land_cover_wetland, slope)
land_cover_water <- resample(land_cover_water, slope)

# To 0/1
land_cover_urban <- calc(land_cover_urban, function(x) as.numeric(x > 0))
land_cover_wetland <- calc(land_cover_wetland, function(x) as.numeric(x > 0))
land_cover_water <- calc(land_cover_water, function(x) as.numeric(x > 0))

# Location with largest population with woreda ---------------------------------
woreda_points <- lapply(1:nrow(woreda), function(i){
  
  print(i)
  
  gpw_i <- gpw %>% 
    crop(woreda[i,]) %>%
    mask(woreda[i,])
  
  df <- gpw_i %>% coordinates() %>% as.data.frame()
  df$pop <- gpw_i[]
  
  loc_df <- df[which.max(df$pop),] %>%
    dplyr::select(x,y)
  
  if(nrow(loc_df) %in% 0){
    loc_df <- coordinates(woreda[i,]) %>%
      as.data.frame() %>%
      dplyr::rename(x= V1,
                    y= V2)
  }
  
  
  return(loc_df)
  
}) %>% 
  bind_rows()

woreda_points$uid <- 1:nrow(woreda_points)
coordinates(woreda_points) <- ~x+y
crs(woreda_points) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Cost Surface -----------------------------------------------------------------
cost_r <- 1 + slope + 25*land_cover_urban + 25*land_cover_wetland + 25*land_cover_water
cost_r <- cost_r %>% mask(eth)

# Least Cost Path --------------------------------------------------------------
# https://stackoverflow.com/questions/52601127/r-how-to-find-least-cost-path-through-raster-image
cost_t <- transition(cost_r, function(x) 1/mean(x), directions=8)

extract_path_cost <- function(i){
  
  print(i)
  
  path_i <- shortestPath(cost_t, 
                         woreda_points[i,], 
                         woreda_points[(i+1):nrow(woreda_points),], 
                         output = "SpatialLines")
  
  cost_i <- costDistance(cost_t, 
                         woreda_points[i,], 
                         woreda_points[(i+1):nrow(woreda_points),])
  
  #s <- woreda_points[-i,]
  #s$cost <- cost_i %>% as.vector()
  #plot(cost_r)
  #plot(s[s$cost %in% Inf,], add=T)
  
  path_i$temp <- 1:length(path_i)
  path_i$cost <- cost_i %>% as.vector()
  path_i$origin <- woreda_points$uid[i]
  path_i$dest <- woreda_points$uid[(i+1):nrow(woreda_points)]
  
  #path_cost_i <- SpatialLinesDataFrame(sl = path_i, data = data.frame(cost=as.numeric(cost_i),
  #                                                                    origin=markets_all_long_origin$origin[i],
  #                                                                    dest=markets_all_long_origin$dest[i]), match.ID = FALSE)
  return(path_i)
}

least_cost_paths_sdf <- lapply(1:(nrow(woreda_points)-1), extract_path_cost) %>% do.call(what="rbind")
least_cost_paths_sdf$edge_uid <- 1:nrow(least_cost_paths_sdf)

# Create minimal spanning tree -----------------------------------------------
least_cost_paths_network <- readshpnw(least_cost_paths_sdf)
least_cost_paths_graph <- nel2igraph(least_cost_paths_network[[2]],
                                     least_cost_paths_network[[3]],
                                     weight=least_cost_paths_network[[5]]$cost,
                                     eadf=least_cost_paths_network[[5]])
minimal_spanning_tree <- mst(least_cost_paths_graph)

# Shapefile of Minimal Spanning Tree -----------------------------------------
minimal_spanning_tree_data <- as_data_frame(minimal_spanning_tree)
minimal_spanning_tree_sdf <- least_cost_paths_sdf[least_cost_paths_sdf$edge_uid %in% minimal_spanning_tree_data$edge_uid,]

# Export -----------------------------------------------------------------------
minimal_spanning_tree_sdf$road_id <- 1:nrow(minimal_spanning_tree_sdf)
minimal_spanning_tree_sdf <- subset(minimal_spanning_tree_sdf, select=c(road_id,cost,origin,dest))

saveRDS(minimal_spanning_tree_sdf, 
        file.path(finaldata_file_path, 
                  "hypothetical_road_networks",
                  "least_cost_path_mst.Rds"))



roads <- readRDS(file.path(finaldata_file_path, "roads", "RoadNetworkPanelData_1996_2016.Rds"))
roads <- roads[roads$Speed1996 > 20,]

leaflet() %>%
  addTiles() %>%
  addPolylines(data=roads, color="red") %>%
  addPolylines(data=minimal_spanning_tree_sdf, opacity=1) 




