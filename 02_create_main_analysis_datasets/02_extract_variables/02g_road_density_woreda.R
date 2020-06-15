# Extract GADM to Points

#source("~/Documents/Github/Ethiopia-Corridors-IE/Code/_ethiopia_ie_master.R")

# Load Data --------------------------------------------------------------------
if(DATASET_TYPE %in% c("woreda_panel_hdx_csa")){
  polygons <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", "points_all.Rds"))
  polygons <- spTransform(polygons, CRS(UTM_ETH))
} else if (DATASET_TYPE %in% "woreda_panel_hdx_csa_nearroad") { 
  polygons <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", "points_cropped_only.Rds"))
  polygons <- spTransform(polygons, CRS(UTM_ETH))
} else{
  polygons <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", "points.Rds"))
  polygons <- spTransform(polygons, CRS(UTM_ETH))
}

# Reoad Density by Year --------------------------------------------------------
#### Function to calculate road density
calc_road_density_i <- function(i, polygons, roads){
  # Given polygons, determines length of road in each polygon
  
  polygons_i <- polygons[i,]
  polygons_i <- gBuffer(polygons_i, byid=T, width=0) # fix self intersections
  roads_i <- raster::intersect(roads, polygons_i)
  
  if(is.null(roads_i)){
    out <- 0
  } else{
    roads_i$length_km <- gLength(roads_i, byid=T) / 1000
    out <- roads_i$length_km %>% sum()
  }
  
  return(out)
}

calc_road_density <- function(polygons, roads){
  
  out <- pbmclapply(1:nrow(polygons), 
                    calc_road_density_i,
                    polygons,
                    roads) %>% unlist()
  
  return(out)
}

#### Create dataframe of road length
for(SEP_ROAD_SHAPEFILES in c(TRUE, FALSE)){
  
  polygon_roadlength <- lapply(1996:2016, function(year){
    
    print(paste(year, "--------------------------------------------------------"))
    
    #### Load/Prep Roads
    if(SEP_ROAD_SHAPEFILES){
      
      # If road isn't even (ie, odd), use previous year
      if((year %% 2) %in% 0){
        year_road <- year
      } else{
        year_road <- year - 1
      }
      
      # Load Roads
      roads <- readOGR(dsn = file.path(project_file_path, "Data", "RawData", "RoadNetworkPanelDataV3_1996_2016_Revised"),
                       layer = paste0("All_Network_", year_road))
      if("Speed2006a" %in% names(roads)) roads$Speed2006 <- roads$Speed2006a
      
    } else{
      roads <- readRDS(file.path(project_file_path, "Data", "FinalData", "roads", "RoadNetworkPanelData_1996_2016.Rds"))
      year_road <- year
    }
    
    roads <- spTransform(roads, UTM_ETH)
    
    #### Loop through speeds within a year and bind columns to create a dataframe
    speeds <- roads[[paste0("Speed", year_road)]] %>% unique()
    
    df_yyyy <- lapply(speeds, function(speed_i){
      
      roads_i <- roads[roads[[paste0("Speed", year_road)]] %in% speed_i,]
      out <- calc_road_density(polygons, roads_i)
      out_df <- out %>% 
        as.data.frame() 
      names(out_df) <- paste0("road_length_", speed_i)
      
      return(out_df)
    }) %>% bind_cols()
    df_yyyy$year <- year
    df_yyyy$uid <- polygons$uid
    
    return(df_yyyy)
  }) %>% bind_rows()
  
  # Export -----------------------------------------------------------------------
  if(SEP_ROAD_SHAPEFILES){
    out_add <- "_rdsep"
  } else{
    out_add <- ""
  }
  
  saveRDS(polygon_roadlength, file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", paste0("points_roadlength_km",out_add,".Rds")))
}




