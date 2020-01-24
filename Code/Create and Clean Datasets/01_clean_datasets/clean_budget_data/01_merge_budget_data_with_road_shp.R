# Merge Data

# Load Data --------------------------------------------------------------------
roads_sdf <- readOGR(dsn=file.path(rawdata_file_path, "RoadNetworkPanelDataV3_1996_2016_Revised"), layer="All_Network_2016")
rsdp123_df <- read_excel(file.path(rawdata_file_path, "ERA Documentation", "eradocs", "rsdp123", "RSDP total_13_ years New_Final.xls"), 4)

roads_sdf$LINKNAME[grepl("dalol", tolower(roads_sdf$LINKNAME))]

roads_sdf$LINKNAME
rsdp123_df$...2 %>% unique %>% sort

20] "Wukro - Adigrat - Zalambessa"                                                                                   
[421] "Wukro - Zalambessa"                                                                                             
[422] "Wukro -Berale-Ahmed Ala -Dalol"  