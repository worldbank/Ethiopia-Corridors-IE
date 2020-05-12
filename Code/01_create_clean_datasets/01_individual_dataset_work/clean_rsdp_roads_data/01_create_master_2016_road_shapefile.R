# Clean RSDP Road Shapefile

# Road data provided as series of separate shapefiles. However, we trust the 2016
# road shapefile the most. The 2016 shapefile has year of completion and change in
# speed limit from previous years. For example, "UNAD-26a" is a road that is
# shown in the 2016 shapefile and, according to the shapefile, was completed in
# 2016 -- before the speed limit was 35 and now is 70. However, the 2012 shapefile
# doesn't show this road existing / or a road in the same area. However, according to
# looking at historic imagery from Google Earth a road existed here in 2012.

# The 2016 shapefile shows how speed limits changed in each year; however, it
# does not show how surface type changed over the years; it only shows current
# surface type. Consequently, we merge in these variables to the 2016 shapefile 
# from the previous shapefiles. Due to differences in the shapefiles, in some
# years we will have a speed for a road (based on 2016 shapefile) but won't 
# have the surface type for that year because the road was not included in a 
# previous shapefile.

# Load Road Data ---------------------------------------------------------------
# Road data is provided as separate shapefiles for each even year. Loop through
# shapefiles and load them. Name them in format of: roads_[year] (eg, roads_2012)

for(year in seq(from=1996, to=2016, by=2)){
  
  roads_yyyy <- readOGR(dsn = file.path(project_file_path, "Data","RawData", "RoadNetworkPanelDataV3_1996_2016_Revised"), 
                        layer = paste0("All_Network_",year))
  
  roads_yyyy$LINKID <- roads_yyyy$LINKID %>% as.character
  roads_yyyy$LINKNAME <- roads_yyyy$LINKNAME %>% as.character
  
  assign(paste0("roads_", year), roads_yyyy)
}

# Fix Select Variables ---------------------------------------------------------

#### If LINKID is NA, use LINKNAME
roads_1996$LINKID[is.na(roads_1996$LINKID)] <- roads_1996$LINKNAME[is.na(roads_1996$LINKID)]
roads_1998$LINKID[is.na(roads_1998$LINKID)] <- roads_1998$LINKNAME[is.na(roads_1998$LINKID)]
roads_2000$LINKID[is.na(roads_2000$LINKID)] <- roads_2000$LINKNAME[is.na(roads_2000$LINKID)]
roads_2002$LINKID[is.na(roads_2002$LINKID)] <- roads_2002$LINKNAME[is.na(roads_2002$LINKID)]
roads_2004$LINKID[is.na(roads_2004$LINKID)] <- roads_2004$LINKNAME[is.na(roads_2004$LINKID)]
roads_2006$LINKID[is.na(roads_2006$LINKID)] <- roads_2006$LINKNAME[is.na(roads_2006$LINKID)]
roads_2008$LINKID[is.na(roads_2008$LINKID)] <- roads_2008$LINKNAME[is.na(roads_2008$LINKID)]
roads_2010$LINKID[is.na(roads_2010$LINKID)] <- roads_2010$LINKNAME[is.na(roads_2010$LINKID)]
roads_2012$LINKID[is.na(roads_2012$LINKID)] <- roads_2012$LINKNAME[is.na(roads_2012$LINKID)]
roads_2014$LINKID[is.na(roads_2014$LINKID)] <- roads_2014$LINKNAME[is.na(roads_2014$LINKID)]
roads_2016$LINKID[is.na(roads_2016$LINKID)] <- roads_2016$LINKNAME[is.na(roads_2016$LINKID)]

#### Select and rename variables
# Some of the variable names are spelled differently in different shapefiles so
# make sure everything spelled the same way.

roads_2006@data <- roads_2006@data %>% dplyr::rename(Speed2006 = Speed2006a)

clean_roads_data <- function(roads, year){
  
  roads_out <- roads@data %>% 
    dplyr::rename("Speed" = paste0("Speed", year)) %>%
    dplyr::select(LINKID, SURFACETYP, LINKLENGTH, Speed) %>%
    group_by(LINKID) %>%
    summarise(SURFACETYP = SURFACETYP[1],
              LINKLENGTH = sum(LINKLENGTH),
              Speed = Speed[1]) %>%
    ungroup() 
  
    names(roads_out)[names(roads_out) %in% "SURFACETYP"] <- paste0("SURFACETYP_yearshape_", year)
    names(roads_out)[names(roads_out) %in% "LINKLENGTH"] <- paste0("LINKLENGTH_yearshape_", year)
    names(roads_out)[names(roads_out) %in% "Speed"] <- paste0("Speed_yearshape_", year)
    

  
  return(roads_out)
  
}

roads_1996 <- clean_roads_data(roads_1996, 1996)
roads_1998 <- clean_roads_data(roads_1998, 1998)
roads_2000 <- clean_roads_data(roads_2000, 2000)
roads_2002 <- clean_roads_data(roads_2002, 2002)
roads_2004 <- clean_roads_data(roads_2004, 2004)
roads_2006 <- clean_roads_data(roads_2006, 2006)
roads_2008 <- clean_roads_data(roads_2008, 2008)
roads_2010 <- clean_roads_data(roads_2010, 2010)
roads_2012 <- clean_roads_data(roads_2012, 2012)
roads_2014 <- clean_roads_data(roads_2014, 2014)

roads_2016 <- merge(roads_2016, roads_1996, by="LINKID", all.x=T,all.y=F)
roads_2016 <- merge(roads_2016, roads_1998, by="LINKID", all.x=T,all.y=F)
roads_2016 <- merge(roads_2016, roads_2000, by="LINKID", all.x=T,all.y=F)
roads_2016 <- merge(roads_2016, roads_2002, by="LINKID", all.x=T,all.y=F)
roads_2016 <- merge(roads_2016, roads_2004, by="LINKID", all.x=T,all.y=F)
roads_2016 <- merge(roads_2016, roads_2006, by="LINKID", all.x=T,all.y=F)
roads_2016 <- merge(roads_2016, roads_2008, by="LINKID", all.x=T,all.y=F)
roads_2016 <- merge(roads_2016, roads_2010, by="LINKID", all.x=T,all.y=F)
roads_2016 <- merge(roads_2016, roads_2012, by="LINKID", all.x=T,all.y=F)
roads_2016 <- merge(roads_2016, roads_2014, by="LINKID", all.x=T,all.y=F)

# Spread Variables -------------------------------------------------------------
# We know speeds in even yeas and the completion year for all years. Spread speed 
# variable to odd years 
for(year in c(1997,1999,2001,2003,2005,2007,2009,2011,2013,2015)){
  
  # Initially, give odd year the speed from the previous year. So 2001 gets 2000's speed
  roads_2016[[paste0("Speed",year)]] <- roads_2016[[paste0("Speed",year-1)]]
  
  # If the road was completed in an odd year, the odd year gets the speed from the next year.
  roads_2016[[paste0("Speed",year)]][roads_2016$Complete_G == year] <- roads_2016[[paste0("Speed",year+1)]][roads_2016$Complete_G == year]
}

# Add Phase Variable -----------------------------------------------------------
#### Phase Variable
roads_2016$rsdp_phase <- NA
roads_2016$rsdp_phase[roads_2016$Complete_G %in% 1997:2002] <- 1
roads_2016$rsdp_phase[roads_2016$Complete_G %in% 2003:2007] <- 2
roads_2016$rsdp_phase[roads_2016$Complete_G %in% 2008:2010] <- 3
roads_2016$rsdp_phase[roads_2016$Complete_G %in% 2011:2016] <- 4
roads_2016$rsdp_phase[roads_2016$Complete_G %in% 2017:2020] <- 5

#### Speed at Start of Phase
roads_2016$speed_phasestart <- NA
roads_2016$speed_phasestart[roads_2016$rsdp_phase %in% 1] <- roads_2016$Speed1997[roads_2016$rsdp_phase %in% 1]
roads_2016$speed_phasestart[roads_2016$rsdp_phase %in% 2] <- roads_2016$Speed2003[roads_2016$rsdp_phase %in% 2]
roads_2016$speed_phasestart[roads_2016$rsdp_phase %in% 3] <- roads_2016$Speed2008[roads_2016$rsdp_phase %in% 3]
roads_2016$speed_phasestart[roads_2016$rsdp_phase %in% 4] <- roads_2016$Speed2011[roads_2016$rsdp_phase %in% 4]
roads_2016$speed_phasestart[roads_2016$rsdp_phase %in% 5] <- roads_2016$Speed2017[roads_2016$rsdp_phase %in% 5]

# Years Since Phase Started ----------------------------------------------------
roads_2016@data <- roads_2016@data %>%
  group_by(rsdp_phase) %>%
  mutate(phase_start_year = min(Complete_G)) %>%
  ungroup() %>%
  mutate(years_since_phase_start = Complete_G - phase_start_year) %>%
  as.data.frame()

# Reproject to WGS 84 ----------------------------------------------------------
roads_2016 <- roads_2016 %>% spTransform("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Export -----------------------------------------------------------------------
saveRDS(roads_2016, file.path(project_file_path, "Data", "FinalData", "roads", "RoadNetworkPanelData_1996_2016.Rds"))

roads_2016_sf <- st_as_sf(roads_2016)
st_write(roads_2016_sf, file.path(project_file_path, "Data", "FinalData", "roads", "RoadNetworkPanelData_1996_2016.geojson"), delete_dsn=T)


