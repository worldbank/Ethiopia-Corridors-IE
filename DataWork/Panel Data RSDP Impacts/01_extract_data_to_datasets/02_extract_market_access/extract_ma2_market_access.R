# Compute Market Access

# Load Data --------------------------------------------------------------------
location_traveltimes <- readRDS(file.path(panel_rsdp_imp_data_file_path, DATASET_TYPE, "individual_datasets", "ma1_travel_times_for_market_access.Rds"))
woreda <- readRDS(file.path(panel_rsdp_imp_data_file_path, DATASET_TYPE, "individual_datasets", "polygons_no_road_cut.Rds"))
gpw <- raster(file.path(data_file_path, "Gridded Population of the World", "RawData", "gpw-v4-population-density_2000.tif"))
ntl <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", "RawData", "Individual Files", "eth_dmspols_1996.tif"))
globcover_urban <- raster(file.path(data_file_path, "Globcover", "RawData", "1992_2015_data", "ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif"), (1996 - 1991))

globcover_urban <- crop(globcover_urban, woreda)
globcover_urban[] <- as.numeric(globcover_urban[] %in% 190)

# Extract Population -----------------------------------------------------------
# Add woreda population from gpw. First, use geometry, then centroid. If population
# from geometry is NA, then use centroid

## Population using geometry
gpw <- crop(gpw, woreda)
woreda$pop_geom <- velox(gpw)$extract(sp = woreda, fun = function(x) sum(x, na.rm=T)) %>% as.vector()
woreda$pop_geom_log <- log(woreda$pop_geom)

## Population using centroid
# Use when geometry returns NA
woreda_pop_centroid <- gCentroid(woreda, byid=T)
woreda$pop_centroid <- raster::extract(gpw, woreda_pop_centroid)

## If population using geometry is NA, use from centroid
woreda$pop_geom[is.na(woreda$pop_geom)] <- woreda$pop_centroid[is.na(woreda$pop_geom)]

## Cleanup
woreda@data <- woreda@data %>%
  dplyr::rename(dest_pop2000 = pop_geom,
                dest_poplog2000 = pop_geom_log,
                dest_uid = cell_id)

# Extract NTL ------------------------------------------------------------------
# Add woreda population from gpw. First, use geometry, then centroid. If population
# from geometry is NA, then use centroid

## Population using geometry
ntl <- crop(ntl, woreda)
woreda$ntl_geom <- velox(ntl)$extract(sp = woreda, fun = function(x) sum(x, na.rm=T)) %>% as.vector()

## Population using centroid
# Use when geometry returns NA
woreda_ntl_centroid <- gCentroid(woreda, byid=T)
woreda$ntl_centroid <- raster::extract(ntl, woreda_ntl_centroid)

## If population using geometry is NA, use from centroid
woreda$ntl_geom[is.na(woreda$ntl_geom)] <- woreda$ntl_centroid[is.na(woreda$ntl_geom)]

## Cleanup
woreda@data <- woreda@data %>%
  dplyr::rename(dest_ntl1996 = ntl_geom)

# Extract NTL - Urban Threshold ------------------------------------------------
# Add woreda population from gpw. First, use geometry, then centroid. If population
# from geometry is NA, then use centroid

## Population using geometry
ntl <- crop(ntl, woreda)

ntl_urban2 <- ntl
ntl_urban6 <- ntl
ntl_urban33 <- ntl
ntl_urban2[] <- as.numeric(ntl[] >= 2)
ntl_urban6[] <- as.numeric(ntl[] >= 6)
ntl_urban33[] <- as.numeric(ntl[] > 33)

woreda$ntlurban2_geom  <- velox(ntl_urban2)$extract(sp = woreda, fun = function(x) sum(x, na.rm=T)) %>% as.vector()
woreda$ntlurban6_geom  <- velox(ntl_urban6)$extract(sp = woreda, fun = function(x) sum(x, na.rm=T)) %>% as.vector()
woreda$ntlurban33_geom <- velox(ntl_urban33)$extract(sp = woreda, fun = function(x) sum(x, na.rm=T)) %>% as.vector()

## Population using centroid
# Use when geometry returns NA
woreda_ntlurban_centroid <- gCentroid(woreda, byid=T)
woreda$ntlurban2_centroid  <- raster::extract(ntl_urban2, woreda_ntl_centroid)
woreda$ntlurban6_centroid  <- raster::extract(ntl_urban6, woreda_ntl_centroid)
woreda$ntlurban33_centroid <- raster::extract(ntl_urban33, woreda_ntl_centroid)

## If population using geometry is NA, use from centroid
woreda$ntlurban2_geom[is.na(woreda$ntlurban_geom)]  <- woreda$ntlurban2_centroid[is.na(woreda$ntlurban2_geom)]
woreda$ntlurban6_geom[is.na(woreda$ntlurban_geom)]  <- woreda$ntlurban6_centroid[is.na(woreda$ntlurban6_geom)]
woreda$ntlurban33_geom[is.na(woreda$ntlurban_geom)] <- woreda$ntlurban33_centroid[is.na(woreda$ntlurban33_geom)]

## Cleanup
woreda@data <- woreda@data %>%
  dplyr::rename(dest_ntlurban2_1996  = ntlurban2_geom,
                dest_ntlurban6_1996  = ntlurban6_geom,
                dest_ntlurban33_1996 = ntlurban33_geom) %>%
  dplyr::mutate(dest_ntlurban2_1996  = as.numeric(dest_ntlurban2_1996 > 0),
                dest_ntlurban6_1996  = as.numeric(dest_ntlurban6_1996 > 0),
                dest_ntlurban33_1996 = as.numeric(dest_ntlurban33_1996 > 0)) # just urban/rural binary

# Extract Globcover Urban ------------------------------------------------------
# Add woreda population from gpw. First, use geometry, then centroid. If population
# from geometry is NA, then use centroid

## Population using geometry
woreda$gcu_geom <- velox(globcover_urban)$extract(sp = woreda, fun = function(x) sum(x, na.rm=T)) %>% as.vector()

## Population using centroid
# Use when geometry returns NA
woreda_gcu_centroid <- gCentroid(woreda, byid=T)
woreda$gcu_centroid <- raster::extract(ntl, woreda_gcu_centroid)

## If population using geometry is NA, use from centroid
woreda$gcu_geom[is.na(woreda$gcu_geom)] <- woreda$gcu_centroid[is.na(woreda$gcu_geom)]

## Cleanup
woreda@data <- woreda@data %>%
  dplyr::rename(dest_gc_urban1996 = gcu_geom)

# Merge Data -------------------------------------------------------------------
location_traveltimes <- merge(location_traveltimes, 
                              woreda@data, 
                              by="dest_uid")

# Prep Travel Time and Iceberg Costs -------------------------------------------
# Remove cases where travel time is zero
location_traveltimes <- location_traveltimes[!(location_traveltimes$travel_time %in% 0),]

# Travel time to minutes
location_traveltimes$travel_time <- location_traveltimes$travel_time * 60

#### Iceberg tt/cost
location_traveltimes_1996 <- location_traveltimes %>% filter(year %in% 1996)

psi <- 0.6

## find p, such that median iceberg costs are 1.25
m_itt <- 0
p <- 0
step <- 0.0000001
while(m_itt <= 1.25){

  p <- p + step
  
  itt <- 1 + (p*location_traveltimes_1996$travel_time)^psi
  m_itt <- median(itt)

  print(paste(round(m_itt, 4), " ", p))
}

p <- p - step

## Calc iceberg trade costs
location_traveltimes$iceberg_cost <- 1 + (p*location_traveltimes$travel_time)^psi

# Pop Divided by TT ------------------------------------------------------------
location_traveltimes <- location_traveltimes %>%
  dplyr::rename(
                ## y_vars
                pop = dest_pop2000,
                poplog = dest_poplog2000,
                ntl = dest_ntl1996,
                gcu = dest_gc_urban1996,
                
                ## tt_var
                tt = travel_time,
                ic = iceberg_cost
                )

for(y_var in c("pop", "poplog", "ntl", "gcu")){
  for(tt_var in c("tt", "ic")){
    for(theta in c(1,2,3.8,5,8)){
      #location_traveltimes$pop_DIV_tt_theta1 <- location_traveltimes$dest_pop2000 / (location_traveltimes$travel_time^1)
      
      location_traveltimes[[paste0(y_var, "_DIV_", tt_var, "_theta", theta)]] <-
        location_traveltimes[[y_var]] / (location_traveltimes[[tt_var]]^theta)
    }
  }
}

# Calculate Market Access ------------------------------------------------------
calc_MA <- function(location_traveltimes, 
                    suffix,
                    MA_all_df = NULL){
  # If MA_all_df is not null, merges results to that dataset
  
  #### Calculate Market Access
  MA_df <- location_traveltimes[, list(
    ## Travel Time
    MA_pop2000_tt_theta1   = sum(pop_DIV_tt_theta1), 
    MA_pop2000_tt_theta2   = sum(pop_DIV_tt_theta2), 
    MA_pop2000_tt_theta3_8 = sum(pop_DIV_tt_theta3.8), 
    MA_pop2000_tt_theta5   = sum(pop_DIV_tt_theta5),
    MA_pop2000_tt_theta8   = sum(pop_DIV_tt_theta8),
    
    MA_poplog2000_tt_theta1   = sum(poplog_DIV_tt_theta1), 
    MA_poplog2000_tt_theta2   = sum(poplog_DIV_tt_theta2), 
    MA_poplog2000_tt_theta3_8 = sum(poplog_DIV_tt_theta3.8), 
    MA_poplog2000_tt_theta5   = sum(poplog_DIV_tt_theta5),
    MA_poplog2000_tt_theta8   = sum(poplog_DIV_tt_theta8),
    
    MA_ntl2000_tt_theta1   = sum(ntl_DIV_tt_theta1), 
    MA_ntl2000_tt_theta2   = sum(ntl_DIV_tt_theta2), 
    MA_ntl2000_tt_theta3_8 = sum(ntl_DIV_tt_theta3.8), 
    MA_ntl2000_tt_theta5   = sum(ntl_DIV_tt_theta5),
    MA_ntl2000_tt_theta8   = sum(ntl_DIV_tt_theta8),
    
    MA_gcu2000_tt_theta1   = sum(gcu_DIV_tt_theta1), 
    MA_gcu2000_tt_theta2   = sum(gcu_DIV_tt_theta2), 
    MA_gcu2000_tt_theta3_8 = sum(gcu_DIV_tt_theta3.8), 
    MA_gcu2000_tt_theta5   = sum(gcu_DIV_tt_theta5),
    MA_gcu2000_tt_theta8   = sum(gcu_DIV_tt_theta8),
    
    ## Iceberg Cost
    MA_pop2000_ic_theta1   = sum(pop_DIV_ic_theta1), 
    MA_pop2000_ic_theta2   = sum(pop_DIV_ic_theta2), 
    MA_pop2000_ic_theta3_8 = sum(pop_DIV_ic_theta3.8), 
    MA_pop2000_ic_theta5   = sum(pop_DIV_ic_theta5),
    MA_pop2000_ic_theta8   = sum(pop_DIV_ic_theta8),
    
    MA_poplog2000_ic_theta1   = sum(poplog_DIV_ic_theta1), 
    MA_poplog2000_ic_theta2   = sum(poplog_DIV_ic_theta2), 
    MA_poplog2000_ic_theta3_8 = sum(poplog_DIV_ic_theta3.8), 
    MA_poplog2000_ic_theta5   = sum(poplog_DIV_ic_theta5),
    MA_poplog2000_ic_theta8   = sum(poplog_DIV_ic_theta8),
    
    MA_ntl2000_ic_theta1   = sum(ntl_DIV_ic_theta1), 
    MA_ntl2000_ic_theta2   = sum(ntl_DIV_ic_theta2), 
    MA_ntl2000_ic_theta3_8 = sum(ntl_DIV_ic_theta3.8), 
    MA_ntl2000_ic_theta5   = sum(ntl_DIV_ic_theta5),
    MA_ntl2000_ic_theta8   = sum(ntl_DIV_ic_theta8),
    
    MA_gcu2000_ic_theta1   = sum(gcu_DIV_ic_theta1), 
    MA_gcu2000_ic_theta2   = sum(gcu_DIV_ic_theta2), 
    MA_gcu2000_ic_theta3_8 = sum(gcu_DIV_ic_theta3.8), 
    MA_gcu2000_ic_theta5   = sum(gcu_DIV_ic_theta5),
    MA_gcu2000_ic_theta8   = sum(gcu_DIV_ic_theta8)
  ), by=list(orig_uid, year)] %>%
    as.data.frame() %>%
    dplyr::rename(cell_id = orig_uid)
  
  MA_df <- MA_df %>% rename_at(vars(-cell_id, -year), ~ paste0(., suffix))
  
  if(is.null(MA_all_df)){
    MA_df_out <- MA_df
  } else{
    MA_df_out <- merge(MA_all_df, MA_df, by = c("cell_id", "year"), all=T)
  }
  
  return(MA_df_out)
}


MA_all_df <- calc_MA(location_traveltimes, "")

## NTL Urban - Threshold of 2
MA_all_df <- calc_MA(location_traveltimes %>%
                       filter(dest_ntlurban2_1996 %in% 1), 
                     paste0("_urban2"), 
                     MA_all_df)

MA_all_df <- calc_MA(location_traveltimes %>%
                       filter(dest_ntlurban2_1996 %in% 0), 
                     paste0("_rural2"), 
                     MA_all_df)

## NTL Urban - Threshold of 6
MA_all_df <- calc_MA(location_traveltimes %>%
                       filter(dest_ntlurban6_1996 %in% 1), 
                     paste0("_urban6"), 
                     MA_all_df)

MA_all_df <- calc_MA(location_traveltimes %>%
                       filter(dest_ntlurban6_1996 %in% 0), 
                     paste0("_rural6"), 
                     MA_all_df)

## NTL Urban - Threshold of 33
MA_all_df <- calc_MA(location_traveltimes %>%
                       filter(dest_ntlurban33_1996 %in% 1), 
                     paste0("_urban33"), 
                     MA_all_df)

MA_all_df <- calc_MA(location_traveltimes %>%
                       filter(dest_ntlurban33_1996 %in% 0), 
                     paste0("_rural33"), 
                     MA_all_df)

for(dist in c(10, 20, 50, 100)){
  print(dist)
  
  MA_all_df <- calc_MA(location_traveltimes %>%
                         filter(distance > dist*1000), 
                       paste0("_exclude",dist,"km"), 
                       MA_all_df)
  
  ## NTL Urban - Threshold of 2
  MA_all_df <- calc_MA(location_traveltimes %>%
                         filter(distance > dist*1000,
                                dest_ntlurban2_1996 %in% 1), 
                       paste0("_exclude",dist,"km_urban2"), 
                       MA_all_df)
  
  MA_all_df <- calc_MA(location_traveltimes %>%
                         filter(distance > dist*1000,
                                dest_ntlurban2_1996 %in% 0), 
                       paste0("_exclude",dist,"km_rural2"), 
                       MA_all_df)
  
  ## NTL Urban - Threshold of 6
  MA_all_df <- calc_MA(location_traveltimes %>%
                         filter(distance > dist*1000,
                                dest_ntlurban6_1996 %in% 1), 
                       paste0("_exclude",dist,"km_urban6"), 
                       MA_all_df)
  
  MA_all_df <- calc_MA(location_traveltimes %>%
                         filter(distance > dist*1000,
                                dest_ntlurban6_1996 %in% 0), 
                       paste0("_exclude",dist,"km_rural6"), 
                       MA_all_df)
  
  ## NTL Urban - Threshold of 33
  MA_all_df <- calc_MA(location_traveltimes %>%
                         filter(distance > dist*1000,
                                dest_ntlurban33_1996 %in% 1), 
                       paste0("_exclude",dist,"km_urban33"), 
                       MA_all_df)
  
  MA_all_df <- calc_MA(location_traveltimes %>%
                         filter(distance > dist*1000,
                                dest_ntlurban33_1996 %in% 0), 
                       paste0("_exclude",dist,"km_rural33"), 
                       MA_all_df)
  
  
}

# Export -----------------------------------------------------------------------
saveRDS(MA_all_df, file.path(panel_rsdp_imp_data_file_path, DATASET_TYPE, "individual_datasets", "ma2_market_access.Rds"))



