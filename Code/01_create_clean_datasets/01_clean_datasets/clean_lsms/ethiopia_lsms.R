# Merging Additional Data to Fishnets Produced by "ethiopia_prepare_NTL_urban.R"
# Ethiopia IE

# - - - - - - - - - - - - - - - #
##### *** Preliminaries *** #####
# - - - - - - - - - - - - - - - #
start.time.script <- Sys.time()

file.path = "C:/Users/wb521633/Documents/Ethiopia/"

library(sp)
library(raster)
library(rgdal)
library(rgeos)
library(parallel)
library(foreign)
library(shp2graph)

merge.fishnet.5x5km <- TRUE
merge.fishnet.10x10km <- TRUE
merge.fishnet.15x15km <- TRUE

# - - - - - - - - - - - - - - - #
##### *** Loading Data *** #####
# - - - - - - - - - - - - - - - #

if(merge.fishnet.5x5km){
  load(paste(file.path,"Data/Fishnets/fishnet.5x5km.Rda",sep=""))
}

if(merge.fishnet.10x10km){
  load(paste(file.path,"Data/Fishnets/fishnet.10x10km.Rda",sep=""))
}

if(merge.fishnet.15x15km){
  load(paste(file.path,"Data/Fishnets/fishnet.15x15km.Rda",sep=""))
}

load(paste(file.path,"Data/data_from_R_STATA/ntl_woreda.rda",sep=""))

# Created equidistant projected point files from fishnet
equal.distant.projection <- "+proj=aeqd +lat_0=8.980603 +lon_0=38.75776"

if(merge.fishnet.5x5km){
  fishnet.5x5km.ed.points <- fishnet.5x5km@data
  coordinates(fishnet.5x5km.ed.points) <- ~eq.distant.longitude+eq.distant.latitude
  proj4string(fishnet.5x5km.ed.points) <- equal.distant.projection
}

if(merge.fishnet.10x10km){
  fishnet.10x10km.ed.points <- fishnet.10x10km@data
  coordinates(fishnet.10x10km.ed.points) <- ~eq.distant.longitude+eq.distant.latitude
  proj4string(fishnet.10x10km.ed.points) <- equal.distant.projection
}

if(merge.fishnet.15x15km){
  fishnet.15x15km.ed.points <- fishnet.15x15km@data
  coordinates(fishnet.15x15km.ed.points) <- ~eq.distant.longitude+eq.distant.latitude
  proj4string(fishnet.15x15km.ed.points) <- equal.distant.projection
}

# - - - - - - - - - - - - - - - - - - - - - - #
##### *** Merging Administrative Data *** #####
# - - - - - - - - - - - - - - - - - - - - - - #
# Making ADM shapefile same projection as points
eth.woreda.2013.ed.prj <- spTransform(eth.woreda.2013, CRS(equal.distant.projection)) 

# Extract values from ADM shapefile to points
# What if did with shapefile instead of points? Then could do nearest neighbor?? This is a centroid approach.
if(merge.fishnet.5x5km){
  fishnet.5x5km_OVER_eth.woreda.2013 <- over(fishnet.5x5km.ed.points, eth.woreda.2013.ed.prj)
  fishnet.5x5km$REGIONNAME <- fishnet.5x5km_OVER_eth.woreda.2013$REGIONNAME
  fishnet.5x5km$ZONENAME <- fishnet.5x5km_OVER_eth.woreda.2013$ZONENAME
  fishnet.5x5km$WOREDANAME <- fishnet.5x5km_OVER_eth.woreda.2013$WOREDANAME
  fishnet.5x5km$REG_P_CODE <- fishnet.5x5km_OVER_eth.woreda.2013$REG_P_CODE
  fishnet.5x5km$ZON_P_CODE <- fishnet.5x5km_OVER_eth.woreda.2013$ZON_P_CODE
  fishnet.5x5km$WOR_P_CODE <- fishnet.5x5km_OVER_eth.woreda.2013$WOR_P_CODE
}

if(merge.fishnet.10x10km){
  fishnet.10x10km_OVER_eth.woreda.2013 <- over(fishnet.10x10km.ed.points, eth.woreda.2013.ed.prj)
  fishnet.10x10km$REGIONNAME <- fishnet.10x10km_OVER_eth.woreda.2013$REGIONNAME
  fishnet.10x10km$ZONENAME <- fishnet.10x10km_OVER_eth.woreda.2013$ZONENAME
  fishnet.10x10km$WOREDANAME <- fishnet.10x10km_OVER_eth.woreda.2013$WOREDANAME
  fishnet.10x10km$REG_P_CODE <- fishnet.10x10km_OVER_eth.woreda.2013$REG_P_CODE
  fishnet.10x10km$ZON_P_CODE <- fishnet.10x10km_OVER_eth.woreda.2013$ZON_P_CODE
  fishnet.10x10km$WOR_P_CODE <- fishnet.10x10km_OVER_eth.woreda.2013$WOR_P_CODE
}

if(merge.fishnet.15x15km){
  fishnet.15x15km_OVER_eth.woreda.2013 <- over(fishnet.15x15km.ed.points, eth.woreda.2013.ed.prj)
  fishnet.15x15km$REGIONNAME <- fishnet.15x15km_OVER_eth.woreda.2013$REGIONNAME
  fishnet.15x15km$ZONENAME <- fishnet.15x15km_OVER_eth.woreda.2013$ZONENAME
  fishnet.15x15km$WOREDANAME <- fishnet.15x15km_OVER_eth.woreda.2013$WOREDANAME
  fishnet.15x15km$REG_P_CODE <- fishnet.15x15km_OVER_eth.woreda.2013$REG_P_CODE
  fishnet.15x15km$ZON_P_CODE <- fishnet.15x15km_OVER_eth.woreda.2013$ZON_P_CODE
  fishnet.15x15km$WOR_P_CODE <- fishnet.15x15km_OVER_eth.woreda.2013$WOR_P_CODE
}

# - - - - - - - - - - - - - - - - - - - - - - #
##### *** Merging SEZs Buffers *** #####
# - - - - - - - - - - - - - - - - - - - - - - #

# Bole Lemi
# Google Maps: Bole Lemi Industrial Zone, Ethiopia
bole.lemi <- as.data.frame(t(c(8.971451, 38.856808)))
names(bole.lemi) <- c("latitude", "longitude")
bole.lemi$bole.lemi.inBuff <- 1
coordinates(bole.lemi) <- ~longitude + latitude
proj4string(bole.lemi) <- "+proj=longlat +datum=WGS84"
bole.lemi <- spTransform(bole.lemi, CRS(equal.distant.projection)) 

if(merge.fishnet.5x5km){
  fishnet.5x5km$distance.bole.lemi <- as.numeric(unlist(gDistance(bole.lemi, fishnet.5x5km.ed.points, byid=T)))
}

if(merge.fishnet.10x10km){
  fishnet.10x10km$distance.bole.lemi <- as.numeric(unlist(gDistance(bole.lemi, fishnet.10x10km.ed.points, byid=T)))
}

if(merge.fishnet.15x15km){
  fishnet.15x15km$distance.bole.lemi <- as.numeric(unlist(gDistance(bole.lemi, fishnet.15x15km.ed.points, byid=T)))
}

# Eastern Industrial Zone
# http://www.addismap.com/a193352480/eastern-industry-zone
eiz <- as.data.frame(t(c(8.772201, 38.919684)))
names(eiz) <- c("latitude", "longitude")
eiz$eiz.inBuff <- 1
coordinates(eiz) <- ~longitude+latitude
proj4string(eiz) <- "+proj=longlat +datum=WGS84"
eiz <- spTransform(eiz, CRS(equal.distant.projection)) 

if(merge.fishnet.5x5km){
  fishnet.5x5km$distance.eiz <- as.numeric(unlist(gDistance(eiz, fishnet.5x5km.ed.points, byid=T)))
}

if(merge.fishnet.10x10km){
  fishnet.10x10km$distance.eiz <- as.numeric(unlist(gDistance(eiz, fishnet.10x10km.ed.points, byid=T)))
}

if(merge.fishnet.15x15km){
  fishnet.15x15km$distance.eiz <- as.numeric(unlist(gDistance(eiz, fishnet.15x15km.ed.points, byid=T)))
}

# - - - - - - - - - - - - - - - - - - - - - #
##### *** Merging Distance to Roads *** #####
# - - - - - - - - - - - - - - - - - - - - - #

##### * Loading Roads * #####

##### Load Proposed Expressway Data #####
setwd(paste(file.path,"Data/proposed_expressway/",sep=""))
prop.express <- readOGR(".","Proposed_Expressway")
prop.express <- spTransform(prop.express, CRS(equal.distant.projection)) 

##### Load Ethiopia Road Network #####
setwd(paste(file.path,"Data/ERA Road/ERA Road 2015/",sep=""))
eth.roads <- readOGR(".","Ethiopian_Road_ntk")
eth.roads <- spTransform(eth.roads, CRS(equal.distant.projection))

express1.Addis.Adama <- eth.roads[eth.roads$LINKNAME %in% "Addis - Adama (Toll Road)",]
trunkroad <- eth.roads[eth.roads$ROADCLASS %in% "ERA A",]

##### Load Trunk Roads #####
setwd(paste(file.path,"Data/RoadNetworkTrunk/",sep=""))
trunk.roads <- readOGR(".","Ethiopia_RoadNetwork_Trunk")
trunk.roads <- spTransform(eth.roads, CRS(equal.distant.projection))

# Make separate shapefile for each expressway (A1-10)
# "eth.roads.a#"
for(i in 1:10){
  assign(paste("eth.roads.a",i,sep=""), eth.roads[eth.roads$ROADCODE %in% c(paste("A",i,sep="")),])
}

# Shapefile of all A roads
eth.roads.a <- eth.roads[eth.roads$ROADCODE %in% c("A1","A2","A3","A4","A5","A6","A7","A8","A9","A10"),]

# Endpoints of Roads
express1.Addis.Adama.network <- readshpnw(express1.Addis.Adama, Detailed=F)
express1.Addis.Adama.endpoints <- as.data.frame(cbind(express1.Addis.Adama.network[[6]], express1.Addis.Adama.network[[7]]))
names(express1.Addis.Adama.endpoints) <- c("latitude","longitude")
coordinates(express1.Addis.Adama.endpoints) <- ~longitude+latitude
proj4string(express1.Addis.Adama.endpoints) <- equal.distant.projection

trunkroad.network <- readshpnw(trunkroad, Detailed=F)
trunkroad.endpoints <- as.data.frame(cbind(trunkroad.network[[6]], trunkroad.network[[7]]))
names(trunkroad.endpoints) <- c("latitude","longitude")
coordinates(trunkroad.endpoints) <- ~longitude+latitude
proj4string(trunkroad.endpoints) <- equal.distant.projection

##### * Calculating Shortest Distance to Roads * #####

gDistance_mc <- function(i, points, road){
  distance <- gDistance(points[i,], road)
  return(distance)
}

##### Distance to Proposed Roads #####

road.list <- c("eth.roads.a",
               "eth.roads.a1",
               "eth.roads.a2",
               "eth.roads.a3",
               "eth.roads.a4",
               "eth.roads.a5",
               "eth.roads.a6",
               "eth.roads.a7",
               "eth.roads.a8",
               "eth.roads.a9",
               "eth.roads.a10",
               "prop.express",
               "express1.Addis.Adama",
               "trunkroad",
               "express1.Addis.Adama.endpoints",
               "trunkroad.endpoints")


for(road in road.list){
  if(merge.fishnet.5x5km){
    fishnet.5x5km[[paste("distance.",road,sep="")]] <- as.numeric(mclapply(1:nrow(fishnet.5x5km.ed.points), gDistance_mc, points=fishnet.5x5km.ed.points, road=eval(parse(text=road))))
  }
  
  if(merge.fishnet.10x10km){
    fishnet.10x10km[[paste("distance.",road,sep="")]] <- as.numeric(mclapply(1:nrow(fishnet.10x10km.ed.points), gDistance_mc, points=fishnet.10x10km.ed.points, road=eval(parse(text=road))))
  }
  
  if(merge.fishnet.15x15km){
    fishnet.15x15km[[paste("distance.",road,sep="")]] <- as.numeric(mclapply(1:nrow(fishnet.15x15km.ed.points), gDistance_mc, points=fishnet.15x15km.ed.points, road=eval(parse(text=road))))
  }
}


##### *** Saving/ Exporting Fishnets *** #####
fishnet.path <- paste(file.path,"Data/Fishnets",sep="")

if(merge.fishnet.5x5km){
  writeOGR(fishnet.5x5km, fishnet.path, "fishnet5km", driver="ESRI Shapefile", overwrite_layer=TRUE)
  save(fishnet.5x5km, file=paste(fishnet.path,"/fishnet.5x5km.Rda",sep=""))
  write.dta(fishnet.5x5km@data, file=paste(fishnet.path,"/fishnet.5x5km.dta",sep=""))
}

if(merge.fishnet.10x10km){
  writeOGR(fishnet.10x10km, fishnet.path, "fishnet10km", driver="ESRI Shapefile", overwrite_layer=TRUE)
  save(fishnet.10x10km, file=paste(fishnet.path,"/fishnet.10x10km.Rda",sep=""))
  write.dta(fishnet.10x10km@data, file=paste(fishnet.path,"/fishnet.10x10km.dta",sep=""))
}

if(merge.fishnet.15x15km){
  writeOGR(fishnet.15x15km, fishnet.path, "fishnet15km", driver="ESRI Shapefile", overwrite_layer=TRUE)
  save(fishnet.15x15km, file=paste(fishnet.path,"/fishnet.15x15km.Rda",sep=""))
  write.dta(fishnet.15x15km@data, file=paste(fishnet.path,"/fishnet.15x15km.dta",sep=""))
}

# Script Time - - - - - - - - - - - - - - -
end.time.script <- Sys.time()
print(end.time.script - start.time.script)



