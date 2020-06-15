# Create Minimal Spanning Tree

# Load Data --------------------------------------------------------------------
## Woredas
woreda <- readOGR(dsn = file.path(rawdata_file_path, "woreda_population", "HDX_CSA"), layer = "Ethioworeda")
woreda <- spTransform(woreda, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

## Population
gpw <- raster(file.path(rawdata_file_path, "gpw-v4-population-density-2000", "gpw-v4-population-density_2000.tif"))
gpw <- gpw %>% crop(woreda)

## Elevation/Slope
elevation <- raster(file.path(rawdata_file_path, "elevation", "gee_download", "eth_elevation_1000m.tif"))
slope <- terrain(elevation, opt="slope", unit="degrees",neighbors=8)

## Globcover
land_cover <- raster(file.path(rawdata_file_path, "esa_globcover", "scratch", "ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif"), 1) %>%
  crop(woreda)

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
cost_r <- cost_r %>% mask(woreda)

# Least Cost Path --------------------------------------------------------------
# https://stackoverflow.com/questions/52601127/r-how-to-find-least-cost-path-through-raster-image
cost_t <- transition(cost_r, function(x) 1/mean(x), directions=8)

extract_path_cost <- function(i){
  print(i)
  
  path_i <- shortestPath(cost_t, 
                         woreda_points[i,], 
                         woreda_points[-i,], 
                         output = "SpatialLines")
  
  cost_i <- costDistance(cost_t, 
                         markets_all_long_origin[i,], 
                         markets_all_long_dest[i,])
  
  path_cost_i <- SpatialLinesDataFrame(sl = path_i, data = data.frame(cost=as.numeric(cost_i),
                                                                      origin=markets_all_long_origin$origin[i],
                                                                      dest=markets_all_long_origin$dest[i]), match.ID = FALSE)
  return(path_cost_i)
}

least_cost_paths_sdf <- lapply(1:nrow(markets_all_long_dest), extract_path_cost) %>% do.call(what="rbind")
least_cost_paths_sdf$edge_uid <- 1:nrow(least_cost_paths_sdf)


