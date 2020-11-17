# Create Minimal Spanning Tree

# Load Data --------------------------------------------------------------------
## Points to Connect
woreda_points <- readRDS(file.path(data_file_path, "Hypothetical Road Networks", "points_to_connect.Rds"))

## Elevation/Slope
# elevation <- raster(file.path(data_file_path, "Elevation", "RawData", "gee_download", "eth_elevation_1000m.tif"))
# slope <- terrain(elevation, opt="slope", unit="degrees",neighbors=8)
# 
# ## Ethiopia ADM boundary
# # The "woreda" file has holes for water bodies
# eth <- readRDS(file.path(data_file_path, "GADM", "RawData", "gadm36_ETH_0_sp.rds")) %>%
#   gBuffer(width = 10/111.12)

# Cost Surface -----------------------------------------------------------------
#cost_r <- slope
#cost_r[] <- 1
#cost_r <- cost_r %>% mask(eth)

# Least Cost Path --------------------------------------------------------------
# https://stackoverflow.com/questions/52601127/r-how-to-find-least-cost-path-through-raster-image
#cost_t <- transition(cost_r, function(x) 1/mean(x), directions=8)

extract_path_cost <- function(i){
  
  print(i)
  
  path_i <- lapply(i:(nrow(woreda_points)-1), function(row){
    #print(row)
    
    l <- rbind(woreda_points[i,],
               woreda_points[(row+1),]) %>%
      as("SpatialLines")
    
    l$cost <- gDistance(woreda_points[row,],
                        woreda_points[(row+1),])
    
    return(l)
    
  }) %>% 
    do.call(what = "rbind")
  
  path_i$origin <- woreda_points$uid[i]
  path_i$dest <- woreda_points$uid[(i+1):nrow(woreda_points)]
  
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
        file.path(data_file_path, "Hypothetical Road Networks", "least_euc_distance_path_mst.Rds"))



