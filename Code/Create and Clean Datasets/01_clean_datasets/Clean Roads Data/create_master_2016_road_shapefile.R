# Impact of Expressway Expansion
# Ethiopia IE

# Setup ------------------------------------------------------------------------
if(Sys.info()[["user"]] == "r521633") project_file_path <- "/home/wb521633/IEs/Ethiopia IE"
if(Sys.info()[["user"]] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE"
if(Sys.info()[["user"]] == "robmarty") project_file_path <- "~/Dropbox/World Bank/IEs/Ethiopia IE"

set.seed(42)

library(lfe)
library(reshape)
library(dplyr)
library(ggplot2)
library(data.table)
library(doBy)
library(leaflet)
library(rgdal)
library(raster)
library(sf)

# Load Roads -------------------------------------------------------------------
#### Load
setwd(file.path(project_file_path, "Data","RawData", "RoadNetworkPanelDataV3_1996_2016_Revised"))
for(year in seq(from=1996, to=2016, by=2)){
  
  roads_yyyy <- readOGR(dsn=".", layer=paste0("All_Network_",year))
  roads_yyyy$LINKID <- roads_yyyy$LINKID %>% as.character
  roads_yyyy$LINKNAME <- roads_yyyy$LINKNAME %>% as.character
  
  assign(paste0("roads_", year), roads_yyyy)
}

if(F){
# "UNAD-26a" - this seems to have existed in 2012 as earth according to Google Earth, so trust 2016 shapefile
# "UNAD-21" - parts seem to have existed before as earth, early 2000s
LINKID_i = "UNAD-20"
roads_1996_i <- roads_1996[roads_1996$LINKID %in% LINKID_i,]
roads_1998_i <- roads_1998[roads_1998$LINKID %in% LINKID_i,]
roads_2000_i <- roads_2000[roads_2000$LINKID %in% LINKID_i,]
roads_2002_i <- roads_2002[roads_2002$LINKID %in% LINKID_i,]
roads_2004_i <- roads_2004[roads_2004$LINKID %in% LINKID_i,]
roads_2006_i <- roads_2006[roads_2006$LINKID %in% LINKID_i,]
roads_2008_i <- roads_2008[roads_2008$LINKID %in% LINKID_i,]
roads_2010_i <- roads_2010[roads_2010$LINKID %in% LINKID_i,]
roads_2012_i <- roads_2012[roads_2012$LINKID %in% LINKID_i,]
roads_2014_i <- roads_2014[roads_2014$LINKID %in% LINKID_i,]
roads_2016_i <- roads_2016[roads_2016$LINKID %in% LINKID_i,]
}

# If LINKID is NA, use LINKNAME
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

# UC-241 repeats and has different surface type. One is 21km the other is 22km
roads_1996$LINKID[roads_1996$LINKID == "UC-241" & roads_1996$LINKLENGTH > 100] <- "UC-241_long"
roads_1998$LINKID[roads_1998$LINKID == "UC-241" & roads_1998$LINKLENGTH > 100] <- "UC-241_long"
roads_2000$LINKID[roads_2000$LINKID == "UC-241" & roads_2000$LINKLENGTH > 100] <- "UC-241_long"
roads_2002$LINKID[roads_2002$LINKID == "UC-241" & roads_2002$LINKLENGTH > 100] <- "UC-241_long"
roads_2004$LINKID[roads_2004$LINKID == "UC-241" & roads_2004$LINKLENGTH > 100] <- "UC-241_long"
roads_2006$LINKID[roads_2006$LINKID == "UC-241" & roads_2006$LINKLENGTH > 100] <- "UC-241_long"
roads_2008$LINKID[roads_2008$LINKID == "UC-241" & roads_2008$LINKLENGTH > 100] <- "UC-241_long"
roads_2010$LINKID[roads_2010$LINKID == "UC-241" & roads_2010$LINKLENGTH > 100] <- "UC-241_long"
roads_2012$LINKID[roads_2012$LINKID == "UC-241" & roads_2012$LINKLENGTH > 100] <- "UC-241_long"
roads_2014$LINKID[roads_2014$LINKID == "UC-241" & roads_2014$LINKLENGTH > 100] <- "UC-241_long"
roads_2016$LINKID[roads_2016$LINKID == "UC-241" & roads_2016$LINKLENGTH > 100] <- "UC-241_long"

# Subset and rename variables
roads_1996 <- subset(roads_1996@data, select=c(LINKID, SURFACETYP, LINKLENGTH, Speed1996)) %>% dplyr::rename(SURFACETYP1996 = SURFACETYP) %>% dplyr::rename(LINKLENGTH1996 = LINKLENGTH) %>% dplyr::rename(Speed1996origfile = Speed1996)
roads_1998 <- subset(roads_1998@data, select=c(LINKID, SURFACETYP, LINKLENGTH, Speed1998)) %>% dplyr::rename(SURFACETYP1998 = SURFACETYP) %>% dplyr::rename(LINKLENGTH1998 = LINKLENGTH) %>% dplyr::rename(Speed1998origfile = Speed1998)
roads_2000 <- subset(roads_2000@data, select=c(LINKID, SURFACETYP, LINKLENGTH, Speed2000)) %>% dplyr::rename(SURFACETYP2000 = SURFACETYP) %>% dplyr::rename(LINKLENGTH2000 = LINKLENGTH) %>% dplyr::rename(Speed2000origfile = Speed2000)
roads_2002 <- subset(roads_2002@data, select=c(LINKID, SURFACETYP, LINKLENGTH, Speed2002)) %>% dplyr::rename(SURFACETYP2002 = SURFACETYP) %>% dplyr::rename(LINKLENGTH2002 = LINKLENGTH) %>% dplyr::rename(Speed2002origfile = Speed2002) 
roads_2004 <- subset(roads_2004@data, select=c(LINKID, SURFACETYP, LINKLENGTH, Speed2004)) %>% dplyr::rename(SURFACETYP2004 = SURFACETYP) %>% dplyr::rename(LINKLENGTH2004 = LINKLENGTH) %>% dplyr::rename(Speed2004origfile = Speed2004)
roads_2006 <- subset(roads_2006@data, select=c(LINKID, SURFACETYP, LINKLENGTH, Speed2006a)) %>% dplyr::rename(SURFACETYP2006 = SURFACETYP) %>% dplyr::rename(LINKLENGTH2006 = LINKLENGTH) %>% dplyr::rename(Speed2006origfile = Speed2006a)
roads_2008 <- subset(roads_2008@data, select=c(LINKID, SURFACETYP, LINKLENGTH, Speed2008)) %>% dplyr::rename(SURFACETYP2008 = SURFACETYP) %>% dplyr::rename(LINKLENGTH2008 = LINKLENGTH) %>% dplyr::rename(Speed2008origfile = Speed2008)
roads_2010 <- subset(roads_2010@data, select=c(LINKID, SURFACETYP, LINKLENGTH, Speed2010)) %>% dplyr::rename(SURFACETYP2010 = SURFACETYP) %>% dplyr::rename(LINKLENGTH2010 = LINKLENGTH) %>% dplyr::rename(Speed2010origfile = Speed2010)
roads_2012 <- subset(roads_2012@data, select=c(LINKID, SURFACETYP, LINKLENGTH, Speed2012)) %>% dplyr::rename(SURFACETYP2012 = SURFACETYP) %>% dplyr::rename(LINKLENGTH2012 = LINKLENGTH) %>% dplyr::rename(Speed2012origfile = Speed2012)
roads_2014 <- subset(roads_2014@data, select=c(LINKID, SURFACETYP, LINKLENGTH, Speed2014)) %>% dplyr::rename(SURFACETYP2014 = SURFACETYP) %>% dplyr::rename(LINKLENGTH2014 = LINKLENGTH) %>% dplyr::rename(Speed2014origfile = Speed2014)

roads_1996 <- summaryBy(LINKLENGTH1996 + Speed1996origfile ~ LINKID + SURFACETYP1996, FUN=sum, data=roads_1996, keep.names=T)
roads_1998 <- summaryBy(LINKLENGTH1998 + Speed1998origfile ~ LINKID + SURFACETYP1998, FUN=sum, data=roads_1998, keep.names=T)
roads_2000 <- summaryBy(LINKLENGTH2000 + Speed2000origfile ~ LINKID + SURFACETYP2000, FUN=sum, data=roads_2000, keep.names=T)
roads_2002 <- summaryBy(LINKLENGTH2002 + Speed2002origfile ~ LINKID + SURFACETYP2002, FUN=sum, data=roads_2002, keep.names=T)
roads_2004 <- summaryBy(LINKLENGTH2004 + Speed2004origfile ~ LINKID + SURFACETYP2004, FUN=sum, data=roads_2004, keep.names=T)
roads_2006 <- summaryBy(LINKLENGTH2006 + Speed2006origfile ~ LINKID + SURFACETYP2006, FUN=sum, data=roads_2006, keep.names=T)
roads_2008 <- summaryBy(LINKLENGTH2008 + Speed2008origfile ~ LINKID + SURFACETYP2008, FUN=sum, data=roads_2008, keep.names=T)
roads_2010 <- summaryBy(LINKLENGTH2010 + Speed2010origfile ~ LINKID + SURFACETYP2010, FUN=sum, data=roads_2010, keep.names=T)
roads_2012 <- summaryBy(LINKLENGTH2012 + Speed2012origfile ~ LINKID + SURFACETYP2012, FUN=sum, data=roads_2012, keep.names=T)
roads_2014 <- summaryBy(LINKLENGTH2014 + Speed2014origfile ~ LINKID + SURFACETYP2014, FUN=sum, data=roads_2014, keep.names=T)

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
for(year in c(1997,1999,2001,2003,2005,2007,2009,2011,2013,2015)){
  roads_2016[[paste0("Speed",year)]] <- roads_2016[[paste0("Speed",year-1)]]
  roads_2016[[paste0("Speed",year)]][roads_2016$Complete_G == year] <- roads_2016[[paste0("Speed",year+1)]][roads_2016$Complete_G == year]
}

# Export -----------------------------------------------------------------------
setwd(file.path(project_file_path, "Data", "FinalData", "roads"))
writeOGR(obj=roads_2016, dsn=".", layer="RoadNetworkPanelData_1996_2016", driver="ESRI Shapefile",overwrite_layer=T)
saveRDS(roads_2016, file.path(project_file_path, "Data", "FinalData", "roads", "RoadNetworkPanelData_1996_2016.Rds"))

roads_2016_sf <- st_as_sf(roads_2016)
st_write(roads_2016_sf, file.path(project_file_path, "Data", "FinalData", "roads", "RoadNetworkPanelData_1996_2016.geojson"))

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
##### PURGATORY #####
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Example Issue ----------------------------------------------------------------
roads_1996 <- roads_1996 %>% spTransform(CRS("+init=epsg:4326"))
roads_2012 <- roads_2012 %>% spTransform(CRS("+init=epsg:4326"))
roads_2016 <- roads_2016 %>% spTransform(CRS("+init=epsg:4326"))

sum(roads_1996$LINKLENGTH)
sum(roads_2016$LINKLENGTH[roads_2016$Speed1996 > 0])

leaflet() %>%
  addTiles() %>%
  addPolylines(data=roads_2012 ,color="red",group="2012") %>%
  addPolylines(data=roads_2016,color="green",group="2016") %>%
  addPolylines(data=roads_2016[roads_2016$LINKID %in% "UNDM-50",],color="blue",group="2016: Route") %>%
  addLayersControl(
    overlayGroups = c("2012", "2016", "2016: Route"),
    options = layersControlOptions(collapsed = FALSE)
  )

leaflet() %>%
  addProviderTiles(providers$Esri.WorldStreetMap) %>%
  addPolylines(data=(roads_2014_i %>% spTransform(CRS("+init=epsg:4326"))) ,color="blue",popup=~LINKID) %>%
  addPolylines(data=(roads_1996[roads_1996$SURFACETYP %in% "Earth",] %>% spTransform(CRS("+init=epsg:4326"))) ,color="red",popup=~LINKID)





roads_2016$problem <- 0
for(year in 1996:2014){
  roads_2016$problem[as.numeric(roads_2016[[paste0("Speed",year)]] > 0) & is.na(roads_2016[[paste0("SURFACETYP",year)]]) == 1] <- 1
}

roads_2016_problem <- roads_2016[roads_2016$problem == 1,]
View(roads_2016_problem@data)

unad <- roads_2016[grepl("UNAD", roads_2016$LINKID),]
aa <- roads_2016[!is.na(roads_2016$Speed1996) & is.na(roads_2016$Speed1996origfile),]
b <- roads_2016[!(roads_2016$Speed1996 %in% roads_2016$Speed1996origfile),]
a <- roads_2016[is.na(roads_2016$SURFACETYP1996),]



leaflet() %>%
  addTiles() %>%
  addPolylines(data=(roads_2012 %>% spTransform(CRS("+init=epsg:4326"))) ,color="red") %>%
  addPolylines(data=(roads_2016[roads_2016$LINKID %in% "UNDM-50",] %>% spTransform(CRS("+init=epsg:4326"))) ,color="blue")

leaflet() %>%
  addProviderTiles(providers$Esri.WorldStreetMap) %>%
  addPolylines(data=(roads_2014_i %>% spTransform(CRS("+init=epsg:4326"))) ,color="blue",popup=~LINKID) %>%
  addPolylines(data=(roads_1996[roads_1996$SURFACETYP %in% "Earth",] %>% spTransform(CRS("+init=epsg:4326"))) ,color="red",popup=~LINKID)




b$LINKID %>% unique

a <- roads_2016 %>%
  group_by()
roads_2016$LINKID

a <- merge(roads_2016, roads_1996, by="LINKID", all.x=T, all.y=F)

a <- roads_1996 %>% group_by(LINKID) %>% mutate(N=n())
b <- a[a$LINKLENGTH != a$LINKLENGTH1996,]
c <- subset(b, select=c(LINKLENGTH, LINKLENGTH1996))

roads_1996_a$LINKID %>% table %>% table

roads_1996_i <- roads_1996[roads_1996$LINKID %in% "B32-4",]
roads_2016_i <- roads_2016[roads_2016$LINKID %in% "B32-4",]

roads_1996 <- subset(roads_1996@data, select=c(LINKID, SURFACETYP)) %>% unique %>% dplyr::rename(SURFACETYP1996 = SURFACETYP)
roads_1998 <- subset(roads_1998@data, select=c(LINKID, SURFACETYP)) %>% unique %>% dplyr::rename(SURFACETYP1998 = SURFACETYP)
roads_2000 <- subset(roads_2000@data, select=c(LINKID, SURFACETYP)) %>% unique %>% dplyr::rename(SURFACETYP2000 = SURFACETYP)
roads_2002 <- subset(roads_2002@data, select=c(LINKID, SURFACETYP)) %>% unique %>% dplyr::rename(SURFACETYP2002 = SURFACETYP)
roads_2004 <- subset(roads_2004@data, select=c(LINKID, SURFACETYP)) %>% unique %>% dplyr::rename(SURFACETYP2004 = SURFACETYP)
roads_2006 <- subset(roads_2006@data, select=c(LINKID, SURFACETYP)) %>% unique %>% dplyr::rename(SURFACETYP2006 = SURFACETYP)
roads_2008 <- subset(roads_2008@data, select=c(LINKID, SURFACETYP)) %>% unique %>% dplyr::rename(SURFACETYP2008 = SURFACETYP)
roads_2010 <- subset(roads_2010@data, select=c(LINKID, SURFACETYP)) %>% unique %>% dplyr::rename(SURFACETYP2010 = SURFACETYP)
roads_2012 <- subset(roads_2012@data, select=c(LINKID, SURFACETYP)) %>% unique %>% dplyr::rename(SURFACETYP2012 = SURFACETYP)
roads_2014 <- subset(roads_2014@data, select=c(LINKID, SURFACETYP)) %>% unique %>% dplyr::rename(SURFACETYP2014 = SURFACETYP)

roads_1996$



a <- roads_2016[roads_2016$LINKID %in% roads_1996$LINKID]

earlyyear_notin2016 <- roads_1996[!(roads_1996$LINKID %in% roads_2016$LINKID),]@data

roads_2016[grepl("Bantu",roads_2016$LINKNAME),]@data


add_N <- function(df){
  df <- df %>%
    group_by(LINKID) %>%
    mutate(N = n())
}

roads_1996 <- add_N(roads_1996)
roads_1998 <- add_N(roads_1998)
roads_2002 <- add_N(roads_2002)
roads_2004 <- add_N(roads_2004)
roads_2006 <- add_N(roads_2006)
roads_2008 <- add_N(roads_2008)
roads_2010 <- add_N(roads_2010)
roads_2012 <- add_N(roads_2012)
roads_2014 <- add_N(roads_2014)

View(roads_2010[roads_2010$N > 1,])

roads_2010$N %>% table

roads_2000 <- add_N(roads_2000)




roads_1998 <- roads_1998 %>%
  group_by(LINKID) %>%
  mutate(N = n())

roads_2000 <- roads_2000 %>%
  group_by(LINKID) %>%
  mutate(N = n())



roads_1996$LINKID %>% table %>% table

roads_2014 <- roads_2014@data %>% unique






table(roads_1996$LINKID %in% roads_2016$LINKID)

roads_2016@data <- roads_2016@data %>% 
  group_by(LINKID) %>% 
  mutate(LINKID_N = n()) %>% 
  as.data.frame
roads_2016 <- roads_2016[roads_2016$LINKID_N %in% 1,]

roads_1996@data <- roads_1996@data %>% 
  group_by(LINKID) %>% 
  mutate(LINKID_N = n()) %>% 
  as.data.frame
roads_1996 <- roads_1996[roads_1996$LINKID_N %in% 1,]

a <- merge(roads_1996@data, roads_2016@data, by="LINKID", all.x=F,all.y=T)

roads_1996$LINKID %in% roads_2016$LINKID
