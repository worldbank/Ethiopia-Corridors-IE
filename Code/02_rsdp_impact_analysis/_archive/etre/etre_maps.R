# Explore Accident Data by Creating Maps

# Preliminaries ----------------------------------------------------------------

# John Computer
#file.path <- "~/Dropbox/research/2017/ethroads/Ethiopia IE/"

# Rob Personal Computer
file.path <- "~/Dropbox/Ethiopia IE/"

library(foreign)
library(readr)
library(tidyverse)
library(rgdal)
library(rgeos)
library(sp)
library(plyr)
library(ggplot2)
library(ggmap)
library(doBy)
library(RColorBrewer)

# Load Data --------------------------------------------------------------------
# Accident Data
df.accidents.2015 <- readxl::read_excel(paste(file.path, "Data/etre/Research Data/AAE Crush Research Data/2015 AAE Crush Research Data.xlsx",sep=""))
df.accidents.2016 <- readxl::read_excel(paste(file.path, "Data/etre/Research Data/AAE Crush Research Data/2016 AAE Crush Research Data.xlsx",sep=""))
df.accidents.2017 <- readxl::read_excel(paste(file.path, "Data/etre/Research Data/AAE Crush Research Data/2017 AAE Crush  Research Data .xlsx",sep=""))

# Road Data
setwd(paste(file.path, "", sep="Data/RoadNetworkPanelDataV3_1996_2016_Revised/"))
road.2016 <- readOGR(dsn=".", "All_Network_2016")

equal.distant.projection <- paste("+proj=aeqd +lat_0=",-1.283333," +lon_0=",36.816667, sep="")

addis.adama <- road.2016[road.2016$LINKNAME %in% "Addis - Adama (Toll Road)",]
addis.adama <- spTransform(addis.adama, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) 
addis.adama <- spTransform(addis.adama, CRS(equal.distant.projection)) 

# Create Shapefile of Points Every 10 Meters Along Road ------------------------
num.of.points  <-  gLength(addis.adama) / 10
addis.adama <- spTransform(addis.adama, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) 
addis.adama.points <- spsample(addis.adama, n = num.of.points, type = "regular")
addis.adama.points <- as.data.frame(addis.adama.points)
names(addis.adama.points) <- c("longitude","latitude")

addis.adama.points$distance.adama.direction <- (1:nrow(addis.adama.points))*10

# Determine Distance Along Roads of Accidents ----------------------------------
accidentLocationToDistance <- function(loc){
  loc <- strsplit(loc, "\\+")[[1]]
  loc <- as.numeric(loc)
  if(is.na(loc[2])){
    loc[2] <- 0
  }
  loc.dist <- loc[1]*1000 + loc[2]
  return(loc.dist)
}

# Calculate Distances
df.accidents.2015$accident.loc.dist <- unlist(lapply(df.accidents.2015$`Accident Location`, accidentLocationToDistance))
df.accidents.2016$accident.loc.dist <- unlist(lapply(df.accidents.2016$`Accident Location`, accidentLocationToDistance))
df.accidents.2017$accident.loc.dist <- unlist(lapply(df.accidents.2017$`Accident Location`, accidentLocationToDistance))

# Remove Observations Not in Direction of Addis ot Adama & Remove Observations with NA Distance
df.accidents.2015 <- df.accidents.2015 %>% 
  subset(grepl("(Addis|Adama)", Direction)) %>%
  drop_na(accident.loc.dist)

df.accidents.2016 <- df.accidents.2016 %>% 
  subset(grepl("(Addis|Adama)", Direction)) %>%
  drop_na(accident.loc.dist)

df.accidents.2017 <- df.accidents.2017 %>% 
  subset(grepl("(Addis|Adama)", Direction)) %>%
  drop_na(accident.loc.dist)

# Convert All Distances to Adama Direction
df.accidents.2015$accident.loc.dist[grepl("Addis", df.accidents.2015$Direction)] <- 76840 - df.accidents.2015$accident.loc.dist[grepl("Addis", df.accidents.2015$Direction)]
df.accidents.2016$accident.loc.dist[grepl("Addis", df.accidents.2016$Direction)] <- 76840 - df.accidents.2016$accident.loc.dist[grepl("Addis", df.accidents.2016$Direction)]
df.accidents.2017$accident.loc.dist[grepl("Addis", df.accidents.2017$Direction)] <- 76840 - df.accidents.2017$accident.loc.dist[grepl("Addis", df.accidents.2017$Direction)]

# Merge Lat/Lon Accident Locations ---------------------------------------------
df.accidents.2017 <- merge(df.accidents.2017, addis.adama.points, by.x="accident.loc.dist", by.y="distance.adama.direction")

# Map --------------------------------------------------------------------------
google.map.expressway <- get_map(location=c(lon=mean(addis.adama@bbox[1,]), 
                            lat=mean(addis.adama@bbox[2,])), 
                 zoom = 11,
                 maptype="roadmap")

ggmap(google.map.expressway) + 
  stat_density2d(data=as.data.frame(df.accidents.2017), aes(x = longitude, y = latitude, fill = ..level..), 
                 alpha=0.5, 
                 geom = "polygon", 
                 bins=10,
                 h=c(0.02,0.02),
                 show.legend=F) +
  scale_fill_gradientn(colours=rev(brewer.pal(7,"Spectral"))) + 
  coord_equal() + 
  theme_nothing(legend=TRUE) +
  theme(legend.key=element_blank()) +
  ggtitle("Accidents 2017: Heatmap") +
  theme(plot.title = element_text(hjust = 0.5))

