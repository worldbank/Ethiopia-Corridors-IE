

##### Preliminaries #####
library(nominatim)
library(readstata13)
library(rgdal)
library(doBy)
library(ggmap)
library(plyr)
library(xlsx)
library(geonames)
library(raster)
#source("C:/Users/wb521633/Documents/GIS Applications/Geocoding/geocoder_function.R")

file.path = "C:/Users/wb521633/Documents/Ethiopia/"

api.key <- "nxDKGoAiWCiRHBtf8J97chK4TqzVRp0l" # Open Street Map API Key: https://developer.mapquest.com

##### Load Ethiopia Shapefile #####
setwd(paste(file.path,"Data/ethiopiaworeda/",sep=""))
eth.woreda.2013 <- readOGR(dsn=".","Eth_Woreda_2013")
eth.woreda.2013 <- spTransform(eth.woreda.2013, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) 
eth.woreda.2013$WOR_P_CODE <- as.character(eth.woreda.2013$WOR_P_CODE)

##### Zone Shapefile #####
eth.woreda.2013.zone <- raster::aggregate(eth.woreda.2013, by=list(eth.woreda.2013$ZONENAME,eth.woreda.2013$REGIONNAME), FUN=mean)
eth.woreda.2013.zone@data <- subset(eth.woreda.2013.zone@data, select=c("Group.1","Group.2"))
names(eth.woreda.2013.zone) <- c("Zone","Region")

eth.woreda.2013.zone$lon.zone <- as.numeric(coordinates(eth.woreda.2013.zone)[,1])
eth.woreda.2013.zone$lat.zone <- as.numeric(coordinates(eth.woreda.2013.zone)[,2])

eth.woreda.2013.zone <- eth.woreda.2013.zone@data
eth.woreda.2013.zone$place.name.match <- ""
eth.woreda.2013.zone[eth.woreda.2013.zone$Zone == "West Arsi",]$place.name.match <- "Mirab Arsi, Ethiopia"
eth.woreda.2013.zone[eth.woreda.2013.zone$Zone == "South West Shewa",]$place.name.match <- "Southwest Shewa Zone, Ethiopia"
eth.woreda.2013.zone[eth.woreda.2013.zone$Zone == "Western",]$place.name.match <- "Mirabawi Tigray, Ethiopia"

eth.woreda.2013.zone <- eth.woreda.2013.zone[eth.woreda.2013.zone$place.name.match != "",]

##### Zone Crosswalk #####
zone <- read.csv(paste(file.path,"Data/adm_crosswalk/zone.csv",sep=""))

##### LSMS-ISA Data #####
if(FALSE){
hh <- read.dta13(paste(file.path,"Data/LSMS/2015/Household/sect1_hh_w3.dta",sep="")) 
hh_geo <- read.dta13(paste(file.path,"Data/LSMS/2015/Geovariables/ETH_HouseholdGeovars_y3.dta",sep="")) 
hh <- merge(hh, hh_geo, by=c("household_id2","ea_id2"))
hh$dummy <- 1
hh <- summaryBy(dummy ~ saq01 +	saq02 + saq03 + lat_dd_mod + lon_dd_mod, data=hh)
coordinates(hh) <- ~lon_dd_mod + lat_dd_mod
proj4string(hh) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

hh.OVER.eth.woreda.2013 <- over(hh, eth.woreda.2013)
hh$REGIONNAME <- hh.OVER.eth.woreda.2013$REGIONNAME
hh$ZONENAME <- hh.OVER.eth.woreda.2013$ZONENAME
hh$WOREDANAME <- hh.OVER.eth.woreda.2013$WOREDANAME
hh$REG_P_CODE <- hh.OVER.eth.woreda.2013$REG_P_CODE
hh$ZON_P_CODE <- hh.OVER.eth.woreda.2013$ZON_P_CODE
hh$WOR_P_CODE <- hh.OVER.eth.woreda.2013$WOR_P_CODE
}

##### Load Ethiopia Road Network #####
#era.road.2015 <- readOGR(paste(file.path,"Data/ERA Road/ERA Road 2015",sep=""),"Ethiopian_Road_ntk")
#era.road.2015 <- spTransform(era.road.2015, CRS(equal.distant.projection))

#era.road.2004 <- readOGR(paste(file.path,"Data/ERA Road/ERA Road 2004",sep=""),"Road_classificationR")
#era.road.2004 <- spTransform(era.road.2004, CRS(equal.distant.projection))


##### Prepping 2010 - 2012 Data #####
price.2010_2012 <- read.dta13(paste(file.path,"Data/price data/July 2010-December 2012/retail_july_2010_december_2012.dta",sep="")) 

price.2010_2012$region <- as.character(price.2010_2012$region)
price.2010_2012$zone <- as.character(price.2010_2012$zone)
price.2010_2012$wereda <- as.character(price.2010_2012$wereda)

price.2010_2012$region[price.2010_2012$region %in% as.character(1:9)] <- paste("0", price.2010_2012$region[price.2010_2012$region %in% as.character(1:9)], sep="")
price.2010_2012$zone[price.2010_2012$zone %in% as.character(1:9)] <- paste("0", price.2010_2012$zone[price.2010_2012$zone %in% as.character(1:9)], sep="")
price.2010_2012$wereda[price.2010_2012$wereda %in% as.character(1:9)] <- paste("0", price.2010_2012$wereda[price.2010_2012$wereda %in% as.character(1:9)], sep="")

price.2010_2012$WOR_P_CODE <- paste(price.2010_2012$region, price.2010_2012$zone, price.2010_2012$wereda, sep="")
price.2010_2012 <- subset(price.2010_2012, select=c("WOR_P_CODE","region","zone","wereda"))

##### Prepping 2013 - 2015 Data #####
price.2013_2015 <- read.dta13(paste(file.path,"Data/price data/Retail Price 2013-15.dta",sep="")) 

price.2013_2015$region <- as.character(price.2013_2015$A06)
price.2013_2015$zone <- as.character(price.2013_2015$Zone)
price.2013_2015$wereda <- as.character(price.2013_2015$wereda)

price.2013_2015$region[price.2013_2015$region %in% as.character(1:9)] <- paste("0", price.2013_2015$region[price.2013_2015$region %in% as.character(1:9)], sep="")
price.2013_2015$zone[price.2013_2015$zone %in% as.character(1:9)] <- paste("0", price.2013_2015$zone[price.2013_2015$zone %in% as.character(1:9)], sep="")
price.2013_2015$wereda[price.2013_2015$wereda %in% as.character(1:9)] <- paste("0", price.2013_2015$wereda[price.2013_2015$wereda %in% as.character(1:9)], sep="")

price.2013_2015$WOR_P_CODE <- paste(price.2013_2015$region, price.2013_2015$zone, price.2013_2015$wereda, sep="")
price.2013_2015 <- subset(price.2013_2015, select=c("WOR_P_CODE","region","zone","wereda"))

##### *** Mergine Price Data *** #####
price.locations <- rbind(price.2010_2012, price.2013_2015)
price.locations$N <- 1
price.locations <- summaryBy(N ~ WOR_P_CODE + region + zone + wereda, data=price.locations, keep.names=T, FUN=sum)

price.locations <- price.locations[as.numeric(price.locations$region) < 20,]

##### Zone ####
zone <- read.csv(paste(file.path,"Data/adm_crosswalk/zone.csv",sep=""))
zone$zone.id <- as.character(zone$zone.id)
zone$region.id <- as.character(zone$region.id)

zone$zone.id[as.numeric(zone$zone.id) < 10] <- paste("0", zone$zone.id[as.numeric(zone$zone.id) < 10], sep="")
zone$region.id[as.numeric(zone$region.id) < 10] <- paste("0", zone$region.id[as.numeric(zone$region.id) < 10], sep="")

names(zone) <- c("x", "zone.name","zone","region")

price.locations <- merge(price.locations, zone, by=c("zone","region"), all.x=T, all.y=F)


##### Map Names to IDs: From Cattle Paper #####
# https://cgspace.cgiar.org/bitstream/handle/10568/346/GeoDistribution_Cattle.pdf?sequence=7

price.locations$woreda_name <- ""
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("010104")] <- "Tahitay Koraro"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("010106")] <- "Aseged Tsembila"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("010205")] <- "Laelay Maychew"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("010204")] <- "Adawa"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("010304")] <- "Ganta Afeshum"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("010306")] <- "Wukro"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("010409")] <- "Mekele"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("010405")] <- "Endamehoni"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("020101")] <- "Asayita"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("020102")] <- "Dubti"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("020302")] <- "Awash Fentale"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("020301")] <- "Amibara"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("020502")] <- "Dawe"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("030119")] <- "Gondar"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("030112")] <- "Dembia"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("030209")] <- "Estie"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("030211")] <- "Debre Tabor"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("030317")] <- "Woldia"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("030308")] <- "Kobo"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("030424")] <- "Dessie"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("030423")] <- "Kombolcha"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("030521")] <- "Kewet"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("030534")] <- "Debre Birhan"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("030617")] <- "Debre Markos"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("030602")] <- "Hulet Eju Enesie"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("030718")] <- "Bahir Dar"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("030704")] <- "Dembecha"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("030805")] <- "Sekota"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("030806")] <- "Dahana"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("030901")] <- "Dangala"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("030905")] <- "Guangua"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("031002")] <- "Batti" # ??
#price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("0310")] <- "Chifie Golana" # ??
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("031002")] <- "Dawrahemado" # ??
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("040106")] <- "Gimbi"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("040123")] <- "Seyo"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("040215")] <- "Gutowayu"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("040207")] <- "Jamma Horo"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("040316")] <- "Mettu"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("040306")] <- "Bedele"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("040405")] <- "Kersa"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("040407")] <- "Goma"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("040503")] <- "Ambo"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("040522")] <- "Woliso Ena Goro"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("040607")] <- "Girar Jarso"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("040603")] <- "Hidabu Abote"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("040703")] <- "Adama"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("040712")] <- "Shashemene"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("040821")] <- "Tiyo"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("040818")] <- "Tena"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("040906")] <- "Chiro"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("040912")] <- "Kuni"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("041013")] <- "Gorogutu"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("041009")] <- "Haromaya"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("041111")] <- "Sinna Ena Dinsho"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("041103")] <- "Adaba"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("041209")] <- "Liben"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("041206")] <- "Hagere Mariam"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("050103")] <- "Shinlie"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("050104")] <- "Erer"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("050103")] <- "Jijiga"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("050105")] <- "Kebri Beyah"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("050903")] <- "Moyale"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("050902")] <- "Dolo Odo"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("060206")] <- "Pawie Liyu"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("060201")] <- "Dangur"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("060303")] <- "Asossa"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("060305")] <- "Bambasi"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("060403")] <- "Kemashi"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("060404")] <- "Agelo Meti"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("070106")] <- "Mesekanena Marako"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("070101")] <- "Goro"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("070201")] <- "Limu"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("070203")] <- "Badawacho"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("070304")] <- "Alaba"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("070302")] <- "Angacha"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("070402")] <- "Awassa"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("070407")] <- "Hula"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("070408")] <- "Bensa"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("070501")] <- "Wenago"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("070502")] <- "Yirga Chefie"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("070601")] <- "Sodo Zuria"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("070603")] <- "Damot Gale"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("070607")] <- "Humbo"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("071001")] <- "Arba Minch"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("071010")] <- "Gofa Zuria"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("071701")] <- "Mareka"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("071703")] <- "Loma"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("071801")] <- "Basketo"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("071901")] <- "Ela"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("070905")] <- "Gimbi"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("070907")] <- "Chena"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("070803")] <- "Yeki"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("070801")] <- "Masha"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("070702")] <- "Bako Gazer"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("070704")] <- "Hamer"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("071103")] <- "Bench"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("071102")] <- "Shewa Bench"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("071201")] <- "Yem"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("071301")] <- "Amaro"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("071401")] <- "Burji"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("071501")] <- "Konso"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("071601")] <- "Derashie"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("120104")] <- "Gambella"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("120205")] <- "Abobo"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("120210")] <- "Godere"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("130101")] <- "Hundene"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("150101")] <- "Dire Dawa"
price.locations$woreda_name[price.locations$WOR_P_CODE %in% c("150102")] <- "Gurgura"

price.locations$town_name <- ""
price.locations$town_name[price.locations$WOR_P_CODE %in% c("010104")] <- "Endaselasse"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("010106")] <- "Endabaguna"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("010205")] <- "Axum"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("010204")] <- "Adwa"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("010304")] <- "Adigrat"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("010306")] <- "Wukro"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("010409")] <- "Mekele"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("010405")] <- "Maychew"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("020101")] <- "Asayita"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("020102")] <- "Dubti"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("020302")] <- "Awash 7 Kilo"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("020301")] <- "Melka Werer"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("020502")] <- "Dawe"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("030119")] <- "Gondar"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("030112")] <- "Chuahit"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("030209")] <- "Estie (Mekane Yesus)"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("030211")] <- ""
price.locations$town_name[price.locations$WOR_P_CODE %in% c("030317")] <- "Woldia"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("030308")] <- "Kobo"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("030424")] <- "Dessie"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("030423")] <- "Kombolcha"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("030521")] <- "Shewa Robit"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("030534")] <- "Debre Berhan"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("030617")] <- "Debre Markos"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("030602")] <- "Mota"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("030718")] <- "Bahir Dar"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("030704")] <- "Dembecha"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("030805")] <- "Sekota"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("030806")] <- "Amdework"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("030901")] <- "Dangala"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("030905")] <- "Chagni"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("031002")] <- "Batti" # ??
#price.locations$town_name[price.locations$WOR_P_CODE %in% c("0310")] <- "" # ??
price.locations$town_name[price.locations$WOR_P_CODE %in% c("031002")] <- "Kemisie" # ??
price.locations$town_name[price.locations$WOR_P_CODE %in% c("040106")] <- "Gimbi"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("040123")] <- "Dembidolo"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("040215")] <- "Nekemte"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("040207")] <- "Shambu"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("040316")] <- "Mettu"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("040306")] <- "Bedele"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("040405")] <- "Jimma"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("040407")] <- "Agaro"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("040503")] <- "Ambo"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("040522")] <- "Woliso"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("040607")] <- "Fichie"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("040603")] <- "Ejerie"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("040703")] <- "Nazret (Adama)"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("040712")] <- "Shashemene"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("040821")] <- "Asela"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("040818")] <- "Diksis"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("040906")] <- "Asebe Teferi"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("040912")] <- "Bedesa"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("041013")] <- "Boroda"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("041009")] <- "Alemaya"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("041111")] <- "Robe"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("041103")] <- "Adaba"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("041209")] <- "Negele"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("041206")] <- "Hagere Mariam"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("050103")] <- "Shinilie"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("050104")] <- "" # ??
price.locations$town_name[price.locations$WOR_P_CODE %in% c("050103")] <- "Jijiga"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("050105")] <- "Hartishek"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("050903")] <- "Moyale"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("050902")] <- "Dolo"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("060206")] <- "Mender 7"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("060201")] <- "Mambuk"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("060303")] <- "Asossa"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("060305")] <- "Bambasi"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("060403")] <- "Kemashi"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("060404")] <- "Agelo Meti"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("070106")] <- "Butajira"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("070101")] <- "Wolkitie"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("070201")] <- "Hosaena"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("070203")] <- "Shonie"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("070304")] <- "Alaba"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("070302")] <- "Doyugena"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("070402")] <- "Awasa"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("070407")] <- "Hagere Selam"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("070408")] <- "Dayu"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("070501")] <- "Dilla"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("070502")] <- "Yirga Chefie"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("070601")] <- "Wolayta Sodo"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("070603")] <- "Boditi"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("070607")] <- "Humbo Tebela"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("071001")] <- "Arba Minch"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("071010")] <- "Sawla"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("071701")] <- "Waka"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("071703")] <- "Bestecherie"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("071801")] <- "Lasoka"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("071901")] <- "Ameya"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("070905")] <- "Bonga"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("070907")] <- "Chena"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("070803")] <- "Tepi"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("070801")] <- "Masha"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("070702")] <- "Jinka"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("070704")] <- "Dimeka"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("071103")] <- "Mizanteferi"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("071102")] <- "Shewa Bench"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("071201")] <- "Deri"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("071301")] <- "Kelie"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("071401")] <- "Soyama"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("071501")] <- "Karat"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("071601")] <- "Gidole"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("120104")] <- "Gambella"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("120205")] <- "Shebo Kire"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("120210")] <- "Meti"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("130101")] <- "Harar"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("150101")] <- "Dire Dawa"
price.locations$town_name[price.locations$WOR_P_CODE %in% c("150102")] <- "Melka Jebdu"

##### Merging Data from Hotspot Source #####
# https://data.humdata.org/dataset/hotspot-woredas/resource/de8995a4-a792-4260-a842-cefe744f0d35

adm.locations.hotspot.file <- read.xlsx(paste(file.path, "Data/adm_crosswalk/ethiopia-hotspot-woredas-july2016.xls", sep=""),1)
adm.locations.hotspot.file$Woreda_code <- as.character(adm.locations.hotspot.file$Woreda_code)
adm.locations.hotspot.file <- subset(adm.locations.hotspot.file, select=c("Woreda_code", "Woreda"))
names(adm.locations.hotspot.file) <- c("WOR_P_CODE","woreda.hotspotdata")

price.locations <- merge(price.locations, adm.locations.hotspot.file, by="WOR_P_CODE", all.x=T,all.y=F)
price.locations$woreda.hotspotdata <- as.character(price.locations$woreda.hotspotdata)

##### Merging Data from Cluster Source #####
adm.locations.cluster.file <- read.xlsx(paste(file.path, "Data/adm_crosswalk/eth_wash_cluster_4w_march_2016.xlsx", sep=""),1)
adm.locations.cluster.file$WOREDANAME <- as.character(adm.locations.cluster.file$WOREDANAME)
adm.locations.cluster.file <- subset(adm.locations.cluster.file, select=c("WOREDANAME", "WOR_P_CODE"))
names(adm.locations.cluster.file) <- c("woreda.clusterdata","WOR_P_CODE")
adm.locations.cluster.file$WOR_P_CODE <- as.character(adm.locations.cluster.file$WOR_P_CODE)

# Merge repeated values
adm.locations.cluster.file$count <- 1
adm.locations.cluster.file <- summaryBy(count ~ woreda.clusterdata + WOR_P_CODE, data=adm.locations.cluster.file)

price.locations <- merge(price.locations, adm.locations.cluster.file, by="WOR_P_CODE", all.x=T,all.y=F)
price.locations$woreda.clusterdata <- as.character(price.locations$woreda.clusterdata)

##### Prepping Data for Geocodes #####
price.locations$woreda.clusterdata[is.na(price.locations$woreda.clusterdata)] <- ""
price.locations$woreda.hotspotdata[is.na(price.locations$woreda.hotspotdata)] <- ""
price.locations$zone.name <- as.character(price.locations$zone.name)

price.locations$woreda_name[price.locations$woreda_name == ""] <- price.locations$woreda.clusterdata[price.locations$woreda_name == ""]
price.locations$woreda_name[price.locations$woreda_name == ""] <- price.locations$woreda.hotspotdata[price.locations$woreda_name == ""]
price.locations$woreda_name[price.locations$woreda_name == ""] <- price.locations$zone.name[price.locations$woreda_name == ""]

##### Change Name so will Geocode #####
price.locations$woreda_name[price.locations$woreda_name %in% c("Dangala")] <- "Dangila"
price.locations$woreda_name[price.locations$woreda_name %in% c("Mettu")] <- "Metu"
price.locations$woreda_name[price.locations$woreda_name %in% c("Yirga Chefie")] <- "Yirgachefe"
price.locations$woreda_name[price.locations$woreda_name %in% c("Tahitay Koraro")] <- "Tahtay Koraro"
price.locations$woreda_name[price.locations$woreda_name %in% c("Adawa")] <- "Adwa"
price.locations$woreda_name[price.locations$woreda_name %in% c("Hulet Eju Enesie")] <- "Hulet Ej Enese"
price.locations$woreda_name[price.locations$woreda_name %in% c("Kemashi")] <- "Kamashi"
price.locations$woreda_name[price.locations$woreda_name %in% c("Agelo Meti")] <- "Agalometi"
price.locations$woreda_name[price.locations$woreda_name %in% c("Hula")] <- "Hulla"
price.locations$woreda_name[price.locations$woreda_name %in% c("Yem")] <- "Yem SP Woreda"
price.locations$woreda_name[price.locations$woreda_name %in% c("Loma")] <- "Loma Bosa"
price.locations$woreda_name[price.locations$woreda_name %in% c("Basketo")] <- "Basketo SP Woreda"
price.locations$woreda_name[price.locations$woreda_name %in% c("Ela")] <- "Ela (Konta) SP Woreda"
price.locations$woreda_name[price.locations$woreda_name %in% c("Alaba")] <- "Alaba SP Woreda"
price.locations$woreda_name[price.locations$woreda_name %in% c("Ambo")] <- "Ambo Zuria"
price.locations$woreda_name[price.locations$woreda_name %in% c("Angacha")] <- "Anigacha"
price.locations$woreda_name[price.locations$woreda_name %in% c("Arba Minch")] <- "Arba Minch Zuria"
price.locations$woreda_name[price.locations$woreda_name %in% c("Aseged Tsembila")] <- "Asgede Tsimbila"
price.locations$woreda_name[price.locations$woreda_name %in% c("Asossa")] <- "Assosa"
price.locations$woreda_name[price.locations$woreda_name %in% c("Bahir Dar")] <- "Bahirdar Zuria"
price.locations$woreda_name[price.locations$woreda_name %in% c("Dangur")] <- "Dangura"
price.locations$woreda_name[price.locations$woreda_name %in% c("Derashie")] <- "Derashe"
price.locations$woreda_name[price.locations$woreda_name %in% c("Dessie")] <- "Dessie Zuria"
price.locations$woreda_name[price.locations$woreda_name %in% c("Gambella")] <- "Gambela Zuria"
price.locations$woreda_name[price.locations$woreda_name %in% c("Gorogutu")] <- "Goro Gutu"
price.locations$woreda_name[price.locations$woreda_name %in% c("Haromaya")] <- "Haro Maya"
price.locations$woreda_name[price.locations$woreda_name %in% c("Kolfe Keranio")] <- "Kolfe - Keran"
price.locations$woreda_name[price.locations$woreda_name %in% c("Metu")] <- "Metu Zuria"
price.locations$woreda_name[price.locations$woreda_name %in% c("Nifas Silk Lafto")] <- "Nefas Silk"
price.locations$woreda_name[price.locations$woreda_name %in% c("Shashemene")] <- "Shashemene Zuria"
price.locations$woreda_name[price.locations$woreda_name %in% c("Asayita")] <- "Asaita"
price.locations$woreda_name[price.locations$woreda_name %in% c("Woldia")] <- "Weldiya"
price.locations$woreda_name[price.locations$woreda_name %in% c("Bench")] <- "Bench (woreda)"
price.locations$woreda_name[price.locations$woreda_name %in% c("Girar Jarso")] <- "Gerar Jarso"
price.locations$woreda_name[price.locations$woreda_name %in% c("Gutowayu")] <- "Guto Wayu"
price.locations$woreda_name[price.locations$woreda_name %in% c("Horoguduru")] <- "Horo Guduru Welega Zone"
price.locations$woreda_name[price.locations$woreda_name %in% c("Hundene")] <- "Harar"
price.locations$woreda_name[price.locations$woreda_name %in% c("Jamma Horo")] <- "Jimma Horo"
price.locations$woreda_name[price.locations$woreda_name %in% c("Woliso Ena Goro")] <- "Waliso"
price.locations$woreda_name[price.locations$woreda_name %in% c("Pawie Liyu")] <- "Pawe Special woreda"
price.locations$woreda_name[price.locations$woreda_name %in% c("Shewa Bench")] <- "Guraferda"
price.locations$woreda_name[price.locations$town_name %in% c("Gimbi") & price.locations$zone.name %in% c("Mirab Wellega")] <- "Gimbi, Mirab Wellega"
price.locations$woreda_name[price.locations$town_name %in% c("Bonga") & price.locations$zone.name %in% c("Keffa")] <- "Bonga, Keffa"

price.locations$town_name[price.locations$town_name %in% c("Asayita")] <- "asaita"
price.locations$town_name[price.locations$town_name %in% c("Asossa")] <- "asosa"
price.locations$town_name[price.locations$town_name %in% c("Awash 7 Kilo")] <- "awash"
price.locations$town_name[price.locations$town_name %in% c("Mizanteferi")] <- "mizan teferi"
price.locations$town_name[price.locations$town_name %in% c("Kemisie")] <- "kemise"
price.locations$town_name[price.locations$town_name %in% c("Gambella")] <- "gambela"
price.locations$town_name[price.locations$town_name %in% c("Woldia")] <- "weldiya"
price.locations$town_name[price.locations$town_name %in% c("Woliso")] <- "waliso"
price.locations$town_name[price.locations$town_name %in% c("Dangala")] <- "Dangla"
price.locations$town_name[price.locations$town_name %in% c("Estie (Mekane Yesus)")] <- "Mekane Yesus"
price.locations$town_name[price.locations$town_name %in% c("Fichie")] <- "Fiche"
price.locations$town_name[price.locations$town_name %in% c("Hagere Mariam")] <- "Hagere Maram"
price.locations$town_name[price.locations$town_name %in% c("Ejerie")] <- "Ejere"
price.locations$town_name[price.locations$town_name %in% c("Mota")] <- "Motta"
price.locations$town_name[price.locations$town_name %in% c("Mettu")] <- "Metu"
price.locations$town_name[price.locations$town_name %in% c("Wolayta Sodo")] <- "Sodo"
price.locations$town_name[price.locations$town_name %in% c("Yirga Chefie")] <- "Yirga Chefe"

##### Geocoding: Merge with Ethiopia Shapefile [Based on Name] #####

eth.woreda.2013$lon <- as.numeric(coordinates(eth.woreda.2013)[,1])
eth.woreda.2013$lat <- as.numeric(coordinates(eth.woreda.2013)[,2])

eth.woreda.2013 <- eth.woreda.2013@data

names(eth.woreda.2013) <- paste(names(eth.woreda.2013),".shp",sep="")
names(eth.woreda.2013)[names(eth.woreda.2013) == "WOREDANAME.shp"] <- "woreda_name"

price.locations <- merge(price.locations, eth.woreda.2013, by="woreda_name", all.x=T,all.y=F)

#price.locations.geo <- price.locations[!is.na(price.locations$lon.shp),]
#price.locations.noGeo <- price.locations[is.na(price.locations$lon.shp),]

##### Geocoding #####
price.locations <- summaryBy(N~lon.shp+lat.shp+HRname.shp+woreda_name+town_name,data=price.locations,FUN=sum)

price.locations$woreda_name.geocode <- paste(price.locations$woreda_name, ", Ethiopia", sep="")

price.locations$id <- 1:nrow(price.locations)

geocodes.woreda <- geocoder(place.names=price.locations$woreda_name.geocode,
                            unique.id=price.locations$id,
                             osm.api.key=api.key,
                             geonames.account.name="ramarty",
                             geonames.wiki.max.search=2,
                             geonames.max.search=2,
                             minimal.df=T,
                             zoom=9,
                             location.pdf=F,
                             location.pdf.onlyIfNoMatches=T,
                             location.pdf.filename=paste(file.path,"geocodes_woreda.pdf",sep=""))



##### Merge in Zone Geocodes #####

geocodes.woreda <- merge(geocodes.woreda, eth.woreda.2013.zone, by.x="place.names", by.y="place.name.match", all.x=T)

##### Selecting which Lat/Lon to Use #####

geocodes.woreda$lat.USE <- NA
geocodes.woreda$lon.USE <- NA

price.locations <- merge(price.locations, geocodes.woreda, by.x="woreda_name.geocode", by.y="place.names",all.x=T)

#price.locations.a <- merge(price.locations, geocodes.woreda, by.x="woreda_name.geocode", by.y="place.names",all.x=T)



eth.woreda.2013.zone


geocodes.woreda.emn <- geocodes.woreda[geocodes.woreda$number.exact.match == 0,]

Debre Markos, Ethiopia --> dsk
Estie, Ethiopia --> geonames.wiki.1
Mirab Arsi, Ethiopia --> eth.region [West Arsi]
Southwest Shewa Zone, Ethiopia --> eth.region [South West Shewa]
Mirabawi Tigray, Ethiopia --> eth.region [Western // Tigray]

