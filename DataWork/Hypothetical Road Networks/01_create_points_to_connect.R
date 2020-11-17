# Create Minimal Spanning Tree

# Load Data --------------------------------------------------------------------
## Woredas
woreda <- readOGR(dsn = file.path(data_file_path, "Woreda Population", "RawData"), layer = "Ethioworeda")
woreda <- spTransform(woreda, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
woreda$uid <- 1:nrow(woreda)

## Ethiopia ADM boundary
# The "woreda" file has holes for water bodies
eth <- readRDS(file.path(data_file_path, "GADM", "RawData", "gadm36_ETH_0_sp.rds")) %>%
  gBuffer(width = 10/111.12)

## Population
gpw <- raster(file.path(data_file_path, "Gridded Population of the World", "RawData", "gpw-v4-population-density_2000.tif"))
gpw <- gpw %>% crop(eth)

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

# Export -----------------------------------------------------------------------
saveRDS(woreda_points, 
        file.path(data_file_path, "Hypothetical Road Networks", "points_to_connect.Rds"))


