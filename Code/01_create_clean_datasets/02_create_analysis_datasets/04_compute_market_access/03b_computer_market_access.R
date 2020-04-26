# Travel Time

# Load Data --------------------------------------------------------------------
#### Travel Times
location_traveltimes <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", "woreda_traveltimes_distances.Rds"))

#### Population
woreda_pop <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", "woreda_details.Rds"))
woreda_pop <- woreda_pop %>%
  dplyr::select(uid, Pop2007, Area) %>%
  dplyr::rename(dest_uid = uid,
                dest_pop2007 = Pop2007) %>%
  as.data.table()

#### Road Length
woreda_rdlngth <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", "points_roadlength_km.Rds"))

## Variables for (1) road length var and (2) road speed numbers
road_length_vars <- names(woreda_rdlngth)[grepl("road_length_", names(woreda_rdlngth))] 

road_lengths <- road_length_vars %>% 
  str_replace_all("road_length_", "") %>% 
  as.numeric() %>% 
  sort()
road_lengths <- road_lengths[road_lengths>0]

## Replace NAs with 0 road length
for(var in road_length_vars){
  woreda_rdlngth[[var]][is.na(woreda_rdlngth[[var]])] <- 0
}

## Create road_length_[speed]over: length of road of speed and above
for(speed_i in road_lengths){
  print(speed_i)
  road_lengths_speedi_over <- road_lengths[road_lengths >= speed_i]
  
  woreda_rdlngth[[paste0("road_length_", speed_i, "over")]] <- 
    apply(woreda_rdlngth %>%
            dplyr::select(paste0("road_length_",road_lengths_speedi_over)), 
          1, 
          FUN = sum)
}

## Prepare Dataset
#woreda_rdlngth %>%
#  group_by(year) %>%
#  dplyr::summarise(var = sum(road_length_25over)) %>%
#  ggplot +
#  geom_line(aes(x = year, y= var))

woreda_rdlngth <- woreda_rdlngth %>%
  filter(year == 1996) %>%
  dplyr::select(c("uid", paste0("road_length_",road_lengths, "over"))) %>%
  dplyr::rename(dest_uid = uid) %>%
  as.data.table()

# Merge Datasets ---------------------------------------------------------------
woreda_data_all <- merge(woreda_pop, woreda_rdlngth, by="dest_uid")
location_traveltimes <- merge(location_traveltimes, woreda_data_all, by="dest_uid")

# Check Pop + Road Length Correlation ------------------------------------------
plot(woreda_data_all$road_length_25over, woreda_data_all$dest_pop2007)

# Calculate Market Access ------------------------------------------------------
# Remove cases where travel time is zero
location_traveltimes <- location_traveltimes[!(location_traveltimes$travel_time %in% 0),]

# Population divided by travel time
location_traveltimes$pop_DIV_tt_theta1 <- location_traveltimes$dest_pop2007 / (location_traveltimes$travel_time^1)
location_traveltimes$pop_DIV_tt_theta5 <- location_traveltimes$dest_pop2007 / (location_traveltimes$travel_time^5)

location_traveltimes$rd_25over_DIV_tt_theta1 <- location_traveltimes$road_length_25over / (location_traveltimes$travel_time^1)
location_traveltimes$rd_25over_DIV_tt_theta5 <- location_traveltimes$road_length_25over / (location_traveltimes$travel_time^5)

location_traveltimes$rd_30over_DIV_tt_theta1 <- location_traveltimes$road_length_30over / (location_traveltimes$travel_time^1)
location_traveltimes$rd_30over_DIV_tt_theta5 <- location_traveltimes$road_length_30over / (location_traveltimes$travel_time^5)

# Calculate Market Access
MA_df <- location_traveltimes[, list(
                                     MA_pop2007_theta1 = sum(pop_DIV_tt_theta1), 
                                     MA_pop2007_theta5 = sum(pop_DIV_tt_theta5),
                                     
                                     MA_rd_25over_theta1 = sum(rd_25over_DIV_tt_theta1), 
                                     MA_rd_25over_theta5 = sum(rd_25over_DIV_tt_theta5),
                                     
                                     MA_rd_30over_theta1 = sum(rd_30over_DIV_tt_theta1), 
                                     MA_rd_30over_theta5 = sum(rd_30over_DIV_tt_theta5)
                                     ), by=list(orig_uid, year)] %>%
  as.data.frame() %>%
  dplyr::rename(uid = orig_uid)

# Calculate Market Access: Remove Distance > 100km -----------------------------
# Remove cases where travel time is zero
location_traveltimes <- location_traveltimes[!(location_traveltimes$distance > 100 * 1000),]

# Population divided by travel time
location_traveltimes$pop_DIV_tt_theta1 <- location_traveltimes$dest_pop2007 / (location_traveltimes$travel_time^1)
location_traveltimes$pop_DIV_tt_theta5 <- location_traveltimes$dest_pop2007 / (location_traveltimes$travel_time^5)

# Calculate Market Access
MA_exclude_100km_df <- location_traveltimes[, list(MA_pop2007_theta1_exclude100km = sum(pop_DIV_tt_theta1), 
                                                   MA_pop2007_theta5_exclude100km = sum(pop_DIV_tt_theta5),
                                                   
                                                   MA_rd_25over_theta1_exclude100km = sum(rd_25over_DIV_tt_theta1), 
                                                   MA_rd_25over_theta5_exclude100km = sum(rd_25over_DIV_tt_theta5),
                                                   
                                                   MA_rd_30over_theta1_exclude100km = sum(rd_30over_DIV_tt_theta1), 
                                                   MA_rd_30over_theta5_exclude100km = sum(rd_30over_DIV_tt_theta5)
                                                   ), by=list(orig_uid, year)] %>%
  as.data.frame() %>%
  dplyr::rename(uid = orig_uid)

# Merge Market Access Measures -------------------------------------------------
MA_all_df <- merge(MA_df, MA_exclude_100km_df, by=c("uid" , "year"), all=T)

# Export -----------------------------------------------------------------------
saveRDS(MA_all_df, file.path(finaldata_file_path, DATASET_TYPE, "individual_datasets", "woreda_market_access.Rds"))



