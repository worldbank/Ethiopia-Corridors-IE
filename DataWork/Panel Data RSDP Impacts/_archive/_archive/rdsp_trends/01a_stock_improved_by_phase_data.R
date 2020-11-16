# Stock of Roads Improved by Phase

# @Rob: could you add the % of total stock improved by provinces for each phase? 
# For ex, 552.52 km of road were improved in Afar in the first phase of RSDP (1997-2001), 
# and this represent n % of the stock of roads at the beginning of the period considered. 
# We need n for every province at the beginning of the periods

# Phases:
  # I: 1997 - 2002
  # II: 2002 - 2007
  # III: 2007 - 2010
  # IV: 2010 - 2015

# Load Data --------------------------------------------------------------------
## RSDP Roads
roads <- readRDS(file.path(data_file_path, "RSDP Roads", "FinalData", "RoadNetworkPanelData_1996_2016.Rds"))

## Province Data
eth <- readOGR(dsn = file.path(data_file_path, "Woreda Boundaries - 2013", "RawData"),
               layer = "Eth_Woreda_2013")
eth <- spTransform(eth, CRS("+proj=merc +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
eth <- gBuffer(eth, width = 0, byid = T)
province_sdf <- raster::aggregate(eth, by="REGIONNAME")

# Roads Per Year Per Province --------------------------------------------------
road_growth_stats_by_phase <- function(phase){
  if(phase == 1){
    phase_begin_year <- 1997
    phase_end_year <- 2001
  }
  
  if(phase == 2){
    phase_begin_year <- 2002
    phase_end_year <- 2006
  }
  
  if(phase == 3){
    phase_begin_year <- 2007
    phase_end_year <- 2009
  }
  
  if(phase == 4){
    phase_begin_year <- 2010
    phase_end_year <- 2015
  }
  
  eth_roads_startyear <- eth_roads[eth_roads[[paste0("Speed", phase_begin_year)]] > 0,]
  eth_roads_improved <- eth_roads[eth_roads[[paste0("Speed", phase_end_year)]] > eth_roads[[paste0("Speed", phase_begin_year)]],]
  
  phase_data <- lapply(province_sdf$REGIONNAME, function(adm){
    print(adm)
    
    adm_sdf_i <- province_sdf[province_sdf$REGIONNAME == adm,]
    
    eth_roads_staryear_i <- raster::intersect(eth_roads_startyear, adm_sdf_i)
    eth_roads_improved_i <- raster::intersect(eth_roads_improved, adm_sdf_i)
    
    if(is.null(eth_roads_staryear_i)){
      eth_roads_staryear_length <- 0
    } else{
      eth_roads_staryear_length <- gLength(eth_roads_staryear_i)
    }
    
    if(is.null(eth_roads_improved_i)){
      eth_roads_improved_length <- 0
    } else{
      eth_roads_improved_length <- gLength(eth_roads_improved_i)
    }
    
    df_out <- data.frame(adm = adm,
                         eth_roads_staryear_length = eth_roads_staryear_length,
                         eth_roads_improved_length = eth_roads_improved_length)
    
    return(df_out)
  }) %>% bind_rows
  phase_data$phase_begin_year <- phase_begin_year
  phase_data$phase_end_year <- phase_end_year
  phase_data$phase <- phase
  
  return(phase_data)
}

phase_results_all <- lapply(1:4, road_growth_stats_by_phase) %>% bind_rows

# Adjust Variables -------------------------------------------------------------
phase_results_all$roads_staryear_length_km <- phase_results_all$eth_roads_staryear_length / 1000
phase_results_all$roads_improved_length_km <- phase_results_all$eth_roads_improved_length / 1000

phase_results_all$roads_improved_prop <- phase_results_all$roads_improved_length_km / phase_results_all$roads_staryear_length_km

phase_results_all <- phase_results_all %>%
  dplyr::select(adm, phase, phase_begin_year, phase_end_year,
                roads_staryear_length_km, roads_improved_length_km, roads_improved_prop) %>%
  dplyr::rename(province = adm)

# Export -----------------------------------------------------------------------
write.csv(phase_results_all, file.path(finaldata_file_path, "rsdp_summary_data", "rsdp_improvement_by_phase.csv"), row.names=F)




