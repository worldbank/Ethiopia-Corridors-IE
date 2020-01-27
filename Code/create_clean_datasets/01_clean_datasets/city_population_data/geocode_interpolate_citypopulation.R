# Geocode and interpolate city population

# - - - - - - - - - - - - - - - - - -#
##### *** Load and Prep Data *** #####
# - - - - - - - - - - - - - - - - - -#

##### City Population Data #####
city.data <- read_excel(file.path(rawdata_file_path, "city_population", "citypopulation_census.xls"), 1)
city.data <- as.data.frame(city.data)
city.data$name.raw <- city.data$name

# Remove trailing while space in string; https://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r
trim.trailing <- function (x) sub("\\s+$", "", x)

prep.name.for.geocode <- function(name){
  
  # Select name in brackets
  if(grepl("\\[", name)){
    name <- strsplit(name, split="\\[")[[1]][2]
    name <- gsub("]", "", name)
  }
  
  # Remove name in parentheses
  if(grepl("\\(", name)){
    name <- strsplit(name, split="\\(")[[1]][1]
    name <- trim.trailing(name)
  }
  
  return(name)
}

city.data$name <- unlist(lapply(city.data$name, prep.name.for.geocode))

##### Geocoded Data #####
cities.locs <- readOGR(dsn=file.path(rawdata_file_path,"eth_pplp_multiplesources_20160205"),
                       "eth_pplp_multiplesources_20160205")
cities.locs$lon <- coordinates(cities.locs)[,1]
cities.locs$lat <- coordinates(cities.locs)[,2]
cities.locs <- cities.locs@data
cities.locs <- subset(cities.locs, select=c(OBJECTID, featureNam, admin3Name, admin2Name, admin1Name, lat, lon))
names(cities.locs)[names(cities.locs) == "featureNam"] <- "name"
cities.locs$name <- as.character(cities.locs$name)

# - - - - - - - - - - - - - - - - - - - - - -#
##### *** Change names so will match *** #####
# - - - - - - - - - - - - - - - - - - - - - -#

city.data$name[!(city.data$name %in% cities.locs$name)]

city.data$name[city.data$name == "Abiy Addi"] <- "Abiye Adi"
city.data$name[city.data$name == "Abomsa"] <- "Abomsa (Tinsae Birhan)"
city.data$name[city.data$name == "Alemaya"] <- "Alem Maya"
city.data$name[city.data$name == "Asaita"] <- "Asayita"
city.data$name[city.data$name == "Asebe Teferi"] <- "Asbe Teferi"
city.data$name[city.data$name == "Asella"] <- "Asela"
city.data$name[city.data$name == "Ayikel"] <- "Aykel"
city.data$name[city.data$name == "Babille"] <- "Babile"
city.data$name[city.data$name == "Bedessa"] <- "Bedesa"
city.data$name[city.data$name == "Debre Berhan"] <- "Debre Birhan"
city.data$name[city.data$name == "Debre Marqos"] <- "Debre Markos"
city.data$name[city.data$name == "Dembi Dolo"] <- "Dembidolo"
city.data$name[city.data$name == "Gondar"] <- "Gonder"
city.data$name[city.data$name == "Hagere Hiywet"] <- "Hagere Hiwot"
city.data$name[city.data$name == "Hart Sheik"] <- "Hartishek"
city.data$name[city.data$name == "Hosaena"] <- "Hosaina"
city.data$name[city.data$name == "Injibara"] <- "Injbara"
city.data$name[city.data$name == "Kebri Dahar"] <- "Kebri Dehar"
city.data$name[city.data$name == "Maych'ew"] <- "Maychew"
city.data$name[city.data$name == "Mek'ele"] <- "Mekele"
city.data$name[city.data$name == "Mot'a"] <- "Mota"
city.data$name[city.data$name == "Shashamane"] <- "Shashemene"
city.data$name[city.data$name == "Togochale"] <- "Togo Chale"
city.data$name[city.data$name == "Yirga 'Alem"] <- "Yirga Alem"

# - - - - - - - - - - - - - - - - - - - - - -#
##### *** Merge Cities that Match *** #####
# - - - - - - - - - - - - - - - - - - - - - -#

cities.locs.subset <- cities.locs[cities.locs$name %in% city.data$name,]
city.data <- merge(city.data, cities.locs.subset, by="name", all.x=T)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - #
##### *** Manual Geocode Cities that Dont Match *** #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - #

city.data$name[!(city.data$name %in% cities.locs$name)]

##### Geocode from following wikipedia pages on city population website #####
# http://www.citypopulation.de/Ethiopia.html

city.data$lat[city.data$name == "Addis Zemen"] <- 12.116667 # https://tools.wmflabs.org/geohack/geohack.php?pagename=Addis_Zemen&params=12_07_N_37_47_E_region:ET_type:city_source:GNS-enwiki
city.data$lon[city.data$name == "Addis Zemen"] <- 37.783333

city.data$lat[city.data$name == "Awasa"] <- 7.05 # https://tools.wmflabs.org/geohack/geohack.php?pagename=Awasa&params=7_3_N_38_28_E_region:ET-SN_type:city(125315)
city.data$lon[city.data$name == "Awasa"] <- 38.466667

city.data$lat[city.data$name == "Awubere"] <- 9.783333 # https://tools.wmflabs.org/geohack/geohack.php?pagename=%C4%80wuber%C4%93&params=9_47_N_43_13_E_region:ET_type:city(35977)
city.data$lon[city.data$name == "Awubere"] <- 43.216667

city.data$lat[city.data$name == "Bonga"] <- 7.266667 # https://tools.wmflabs.org/geohack/geohack.php?pagename=Bonga&params=7_16_N_36_14_E_region:ET-SN_type:city(19664)
city.data$lon[city.data$name == "Bonga"] <- 36.233333

city.data$lat[city.data$name == "Dangila"] <- 11.266667 # https://tools.wmflabs.org/geohack/geohack.php?pagename=Dangila&params=11_16_N_36_50_E_
city.data$lon[city.data$name == "Dangila"] <- 36.833333

city.data$lat[city.data$name == "Debarq"] <- 13.133333 # https://tools.wmflabs.org/geohack/geohack.php?pagename=Debarq&params=13_08_N_37_54_E_region:ET_type:city
city.data$lon[city.data$name == "Debarq"] <- 37.9

city.data$lat[city.data$name == "Debre Zeyit"] <- 8.75 # https://tools.wmflabs.org/geohack/geohack.php?pagename=Bishoftu&params=8_45_N_38_59_E_region:ET_type:city(171115)
city.data$lon[city.data$name == "Debre Zeyit"] <- 38.983333

city.data$lat[city.data$name == "Derwonaji"] <- 9.833333 # https://tools.wmflabs.org/geohack/geohack.php?pagename=Derwonaji&params=9_50_N_43_02_E_region:ET-SO_type:city(35645)
city.data$lon[city.data$name == "Derwonaji"] <- 43.033333

city.data$lat[city.data$name == "Dessie"] <- 11.133333 # https://tools.wmflabs.org/geohack/geohack.php?pagename=Dessie&params=11_8_N_39_38_E_region:ET-AM_type:city(610431)
city.data$lon[city.data$name == "Dessie"] <- 39.633333

city.data$lat[city.data$name == "Dolo"] <- 4.166667 # https://tools.wmflabs.org/geohack/geohack.php?pagename=Dolo,_Ethiopia&params=4_10_N_42_04_E_region:ET_type:city(30970)
city.data$lon[city.data$name == "Dolo"] <- 42.066667

city.data$lat[city.data$name == "Genet"] <- 8.1 # https://tools.wmflabs.org/geohack/geohack.php?pagename=Genet,_Ethiopia&params=08_06_N_36_57_E_
city.data$lon[city.data$name == "Genet"] <- 36.95

city.data$lat[city.data$name == "Giyon"] <- 8.533333 # https://tools.wmflabs.org/geohack/geohack.php?pagename=Waliso&params=8_32_N_37_58_E_region:ET-OR_type:city
city.data$lon[city.data$name == "Giyon"] <- 37.966667

city.data$lat[city.data$name == "Hagere Mariam"] <- 5.583333 # https://tools.wmflabs.org/geohack/geohack.php?pagename=Bule_Hora_Town&params=5_35_N_38_15_E_
city.data$lon[city.data$name == "Hagere Mariam"] <- 38.25

city.data$lat[city.data$name == "Harar"] <- 9.311111 # https://tools.wmflabs.org/geohack/geohack.php?pagename=Harar&params=9_18_40_N_42_07_40_E_region:ET-HA_type:city(122000)
city.data$lon[city.data$name == "Harar"] <- 42.127778

city.data$lat[city.data$name == "Hurata"] <- 8.15 # https://tools.wmflabs.org/geohack/geohack.php?pagename=Huruta&params=08_09_N_39_21_E_type:city(16922)
city.data$lon[city.data$name == "Hurata"] <- 39.35

city.data$lat[city.data$name == "K'olito"] <- 7.312222 # https://tools.wmflabs.org/geohack/geohack.php?pagename=Alaba_Kulito&params=7_18_44_N_38_05_21_E_region:ET_type:city(27359)
city.data$lon[city.data$name == "K'olito"] <- 38.089167

city.data$lat[city.data$name == "Kebri Mangest"] <- 5.883333 # https://tools.wmflabs.org/geohack/geohack.php?pagename=Kebri_Mangest&params=5_53_N_38_59_E_region:ET_type:city(22938)
city.data$lon[city.data$name == "Kebri Mangest"] <- 38.983333

city.data$lat[city.data$name == "Kobo"] <- 12.15 # https://tools.wmflabs.org/geohack/geohack.php?pagename=Kobo,_Ethiopia&params=12_09_N_39_38_E_region:ET_type:city
city.data$lon[city.data$name == "Kobo"] <- 39.633333

city.data$lat[city.data$name == "Kombolcha"] <- 11.086667 # https://tools.wmflabs.org/geohack/geohack.php?pagename=Kombolcha&params=11_5_12_N_39_44_12_E_region:ET_type:city(188667)
city.data$lon[city.data$name == "Kombolcha"] <- 39.736667

city.data$lat[city.data$name == "Mek'i"] <- 8.15 # https://tools.wmflabs.org/geohack/geohack.php?pagename=Meki&params=08_9_N_38_49_E_type:city(36597)
city.data$lon[city.data$name == "Mek'i"] <- 38.816667

city.data$lat[city.data$name == "Negele Boran"] <- 5.333333 # https://tools.wmflabs.org/geohack/geohack.php?pagename=Negele_Borana&params=5_20_N_39_35_E_type:city(42958)_region:ET-OR
city.data$lon[city.data$name == "Negele Boran"] <- 39.583333

city.data$lat[city.data$name == "Soqota"] <- 12.633333 # https://tools.wmflabs.org/geohack/geohack.php?pagename=Soqota&params=12_38_N_39_02_E_region:ET_type:city(22346)
city.data$lon[city.data$name == "Soqota"] <- 39.033333

city.data$lat[city.data$name == "Tepi"] <- 7.2 # https://tools.wmflabs.org/geohack/geohack.php?pagename=Tepi&params=7_12_N_35_27_E_region:ET_type:city(19231)
city.data$lon[city.data$name == "Tepi"] <- 35.45

city.data$lat[city.data$name == "Togo Chale"] <- 9.600833 # https://tools.wmflabs.org/geohack/geohack.php?pagename=Tog_Wajaale&params=09_36_03_N_043_20_09_E_region:ET_type:city_source:GNS-enwiki
city.data$lon[city.data$name == "Togo Chale"] <- 43.335833

city.data$lat[city.data$name == "Wenji Gefersa"] <- 8.45 # https://tools.wmflabs.org/geohack/geohack.php?pagename=Wenji_Gefersa&params=8_27_N_39_17_E_region:ET
city.data$lon[city.data$name == "Wenji Gefersa"] <- 39.283333

##### Geocode through looking at map on city population website #####
# Look at map, and search in google mpas for similar name

city.data$lat[city.data$name == "Aman"] <- 6.957776 # https://www.google.com/maps/place/Greater+Aman,+Ethiopia/@6.9573495,35.5455694,14z/data=!3m1!4b1!4m5!3m4!1s0x17a91f7e5612429d:0x151eb1e7ee1fd438!8m2!3d6.95735!4d35.563122
city.data$lon[city.data$name == "Aman"] <- 35.563208

city.data$lat[city.data$name == "Burayu"] <- 9.070375 # https://www.google.com/maps/place/Gefersa,+Addis+Ababa,+Ethiopia/@9.0671538,38.6567854,15z/data=!4m5!3m4!1s0x164b8866a8754755:0x19c3efbf209d4d4b!8m2!3d9.066667!4d38.6666669
city.data$lon[city.data$name == "Burayu"] <- 38.663459

city.data$lat[city.data$name == "Enseno"] <- 8.067431 # https://www.google.com/maps/place/Enseno,+Ethiopia/@8.0666665,38.4491144,14z/data=!3m1!4b1!4m5!3m4!1s0x17b342a08060ce4b:0xb114158ea745ac17!8m2!3d8.066667!4d38.466667
city.data$lon[city.data$name == "Enseno"] <- 38.466495

city.data$lat[city.data$name == "Este"] <- 11.612577 # https://www.google.com/maps/place/Mekane+Yesus,+Ethiopia/@11.6129971,38.0417488,14z/data=!3m1!4b1!4m5!3m4!1s0x16445e76eaeff4fd:0xde946d59a05a15fa!8m2!3d11.6145028!4d38.0578556
city.data$lon[city.data$name == "Este"]<- 38.060760

city.data$lat[city.data$name == "Gutin"] <- 9.567108 # https://www.google.com/maps/place/Gutin,+Ethiopia/@9.5633837,36.615629,14z/data=!3m1!4b1!4m5!3m4!1s0x165232e85080605b:0xd8d0f28ad6de07a6!8m2!3d9.5658888!4d36.6363811
city.data$lon[city.data$name == "Gutin"] <- 36.634469

city.data$lat[city.data$name == "Logia"] <- 11.724730 # https://www.google.com/maps/place/Logia,+Ethiopia/@11.7246455,40.9586404,14z/data=!3m1!4b1!4m5!3m4!1s0x163ebf99cb9d939b:0x69b9f1d344d75a38!8m2!3d11.7232419!4d40.9763542
city.data$lon[city.data$name == "Logia"] <- 40.976279

city.data$lat[city.data$name == "Mekhoni"] <- 12.798990 # https://www.google.com/maps/place/Mehoni,+Ethiopia/@12.7994089,39.6358502,15z/data=!3m1!4b1!4m5!3m4!1s0x164021f88b1fb7ef:0x6c24a6fd0c5e90d5!8m2!3d12.7992043!4d39.6440744
city.data$lon[city.data$name == "Mekhoni"] <- 39.644412

# - - - - - - - - - - - - - - - - - - - -#
##### *** Interpolate Population *** #####
# - - - - - - - - - - - - - - - - - - - -#

city.data <- subset(city.data, select=c(name,lat,lon,census1984,census1994,census2007,census2015))
city.data$census1994 <- as.numeric(city.data$census1994)

##### Interpolate 2007 to 2015; extrapolate 1994 to 2007 #####
value.1 <- city.data$census2007
value.2 <- city.data$census2015
year.1 <- 2007
year.2 <- 2015
  
value.1.log <- log(value.1)
value.2.log <- log(value.2)
  
log.slope <- (value.2.log - value.1.log) / (year.2 - year.1)
num.years <- year.2 - year.1

i <- 1994 - 2007
for(yr in 1994:2016){
  city.data[[paste("census.interpolate.extrapolate.exp.",yr,sep="")]] <- round(exp(value.1.log + log.slope*i))
  i <- i + 1
}
  
##### Interpolate 1994 to 2007, where have values for 1994 #####
value.1 <- city.data$census1994
value.2 <- city.data$census2007
year.1 <- 1994
year.2 <- 2007

value.1.log <- log(value.1)
value.2.log <- log(value.2)

log.slope <- (value.2.log - value.1.log) / (year.2 - year.1)
num.years <- year.2 - year.1

i <- 0
for(yr in 1994:2007){
  city.data[[paste("census.interpolate.exp.",yr,sep="")]] <- round(exp(value.1.log + log.slope*i))
  i <- i + 1
}

##### Define Variables to use #####

city.data$pop.1994 <- city.data$census.interpolate.exp.1994
city.data$pop.1994[is.na(city.data$census1994)] <- city.data$census.interpolate.extrapolate.exp.1994[is.na(city.data$census1994)]

city.data$pop.1995 <- city.data$census.interpolate.exp.1995
city.data$pop.1995[is.na(city.data$census1994)] <- city.data$census.interpolate.extrapolate.exp.1995[is.na(city.data$census1994)]

city.data$pop.1996 <- city.data$census.interpolate.exp.1996
city.data$pop.1996[is.na(city.data$census1994)] <- city.data$census.interpolate.extrapolate.exp.1996[is.na(city.data$census1994)]

city.data$pop.1997 <- city.data$census.interpolate.exp.1997
city.data$pop.1997[is.na(city.data$census1994)] <- city.data$census.interpolate.extrapolate.exp.1997[is.na(city.data$census1994)]

city.data$pop.1998 <- city.data$census.interpolate.exp.1998
city.data$pop.1998[is.na(city.data$census1994)] <- city.data$census.interpolate.extrapolate.exp.1998[is.na(city.data$census1994)]

city.data$pop.1999 <- city.data$census.interpolate.exp.1999
city.data$pop.1999[is.na(city.data$census1994)] <- city.data$census.interpolate.extrapolate.exp.1999[is.na(city.data$census1994)]

city.data$pop.2000 <- city.data$census.interpolate.exp.2000
city.data$pop.2000[is.na(city.data$census1994)] <- city.data$census.interpolate.extrapolate.exp.2000[is.na(city.data$census1994)]

city.data$pop.2001 <- city.data$census.interpolate.exp.2001
city.data$pop.2001[is.na(city.data$census1994)] <- city.data$census.interpolate.extrapolate.exp.2001[is.na(city.data$census1994)]

city.data$pop.2002 <- city.data$census.interpolate.exp.2002
city.data$pop.2002[is.na(city.data$census1994)] <- city.data$census.interpolate.extrapolate.exp.2002[is.na(city.data$census1994)]

city.data$pop.2003 <- city.data$census.interpolate.exp.2003
city.data$pop.2003[is.na(city.data$census1994)] <- city.data$census.interpolate.extrapolate.exp.2003[is.na(city.data$census1994)]

city.data$pop.2004 <- city.data$census.interpolate.exp.2004
city.data$pop.2004[is.na(city.data$census1994)] <- city.data$census.interpolate.extrapolate.exp.2004[is.na(city.data$census1994)]

city.data$pop.2005 <- city.data$census.interpolate.exp.2005
city.data$pop.2005[is.na(city.data$census1994)] <- city.data$census.interpolate.extrapolate.exp.2005[is.na(city.data$census1994)]

city.data$pop.2006 <- city.data$census.interpolate.exp.2006
city.data$pop.2006[is.na(city.data$census1994)] <- city.data$census.interpolate.extrapolate.exp.2006[is.na(city.data$census1994)]

city.data$pop.2007 <- city.data$census.interpolate.extrapolate.exp.2007
city.data$pop.2008 <- city.data$census.interpolate.extrapolate.exp.2008
city.data$pop.2009 <- city.data$census.interpolate.extrapolate.exp.2009
city.data$pop.2010 <- city.data$census.interpolate.extrapolate.exp.2010
city.data$pop.2011 <- city.data$census.interpolate.extrapolate.exp.2011
city.data$pop.2012 <- city.data$census.interpolate.extrapolate.exp.2012
city.data$pop.2013 <- city.data$census.interpolate.extrapolate.exp.2013
city.data$pop.2014 <- city.data$census.interpolate.extrapolate.exp.2014
city.data$pop.2015 <- city.data$census.interpolate.extrapolate.exp.2015
city.data$pop.2016 <- city.data$census.interpolate.extrapolate.exp.2016

##### Export #####

city.data <- subset(city.data, select=c(name,lat,lon,
                                           pop.1994,
                                           pop.1995,
                                           pop.1996,
                                           pop.1997,
                                           pop.1998,
                                           pop.1999,
                                           pop.2000,
                                           pop.2001,
                                           pop.2002,
                                           pop.2003,
                                           pop.2004,
                                           pop.2005,
                                           pop.2006,
                                           pop.2007,
                                           pop.2008,
                                           pop.2009,
                                           pop.2010,
                                           pop.2011,
                                           pop.2012,
                                           pop.2013,
                                           pop.2014,
                                           pop.2015,
                                           pop.2016))

names(city.data) <- names(city.data) %>% str_replace_all("\\.", "_")

write.csv(city.data, file.path(finaldata_file_path, "city_population", "city_pop_geocoded.csv"), row.names=T)


