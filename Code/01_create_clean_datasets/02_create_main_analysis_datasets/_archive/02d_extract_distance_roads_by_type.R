# Extract Distance to Road by Type

# Load Points ------------------------------------------------------------------
points <- readRDS(file.path(outputs_for_grid, TYPE, "points.Rds"))
coordinates(points) <- ~long+lat
crs(points) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

points <- points %>% spTransform(CRS(UTM_ETH))

# Load Roads -------------------------------------------------------------------
#### Load
setwd(file.path(rawdata_file_path, "RoadNetworkPanelDataV3_1996_2016_Revised"))
for(year in seq(from=1996, to=2016, by=2)){
  assign(paste0("roads_", year),
        readOGR(dsn=".", layer=paste0("All_Network_",year)) %>% spTransform(UTM_ETH))
}

#### Add ID of 1. Use when aggregating
roads_1996$id <- 1
roads_1998$id <- 1
roads_2000$id <- 1
roads_2002$id <- 1
roads_2004$id <- 1
roads_2006$id <- 1
roads_2008$id <- 1
roads_2010$id <- 1
roads_2012$id <- 1
roads_2014$id <- 1
roads_2016$id <- 1

#### Road Type
roads_1996_asphaltconcrete <- roads_1996[roads_1996$SURFACETYP == "Asphalt Concrete",] %>% raster::aggregate(by="id")
roads_1998_asphaltconcrete <- roads_1998[roads_1998$SURFACETYP == "Asphalt Concrete",] %>% raster::aggregate(by="id")
roads_2000_asphaltconcrete <- roads_2000[roads_2000$SURFACETYP == "Asphalt Concrete",] %>% raster::aggregate(by="id")
roads_2002_asphaltconcrete <- roads_2002[roads_2002$SURFACETYP == "Asphalt Concrete",] %>% raster::aggregate(by="id")
roads_2004_asphaltconcrete <- roads_2004[roads_2004$SURFACETYP == "Asphalt Concrete",] %>% raster::aggregate(by="id")
roads_2006_asphaltconcrete <- roads_2006[roads_2006$SURFACETYP == "Asphalt Concrete",] %>% raster::aggregate(by="id")
roads_2008_asphaltconcrete <- roads_2008[roads_2008$SURFACETYP == "Asphalt Concrete",] %>% raster::aggregate(by="id")
roads_2010_asphaltconcrete <- roads_2010[roads_2010$SURFACETYP == "Asphalt Concrete",] %>% raster::aggregate(by="id")
roads_2012_asphaltconcrete <- roads_2012[roads_2012$SURFACETYP == "Asphalt Concrete",] %>% raster::aggregate(by="id")
roads_2014_asphaltconcrete <- roads_2014[roads_2014$SURFACETYP == "Asphalt Concrete",] %>% raster::aggregate(by="id")
roads_2016_asphaltconcrete <- roads_2016[roads_2016$SURFACETYP == "Asphalt Concrete",] %>% raster::aggregate(by="id")

#roads_1996_cobblestone <- roads_1996[roads_1996$SURFACETYP == "Cobbled Stone",] %>% raster::aggregate(by="id")
#roads_1998_cobblestone <- roads_1998[roads_1998$SURFACETYP == "Cobbled Stone",] %>% raster::aggregate(by="id")
#roads_2000_cobblestone <- roads_2000[roads_2000$SURFACETYP == "Cobbled Stone",] %>% raster::aggregate(by="id")
#roads_2002_cobblestone <- roads_2002[roads_2002$SURFACETYP == "Cobbled Stone",] %>% raster::aggregate(by="id")
#roads_2004_cobblestone <- roads_2004[roads_2004$SURFACETYP == "Cobbled Stone",] %>% raster::aggregate(by="id")
roads_2006_cobblestone <- roads_2006[roads_2006$SURFACETYP == "Cobbled Stone",] %>% raster::aggregate(by="id")
roads_2008_cobblestone <- roads_2008[roads_2008$SURFACETYP == "Cobbled Stone",] %>% raster::aggregate(by="id")
roads_2010_cobblestone <- roads_2010[roads_2010$SURFACETYP == "Cobbled Stone",] %>% raster::aggregate(by="id")
roads_2012_cobblestone <- roads_2012[roads_2012$SURFACETYP == "Cobbled Stone",] %>% raster::aggregate(by="id")
roads_2014_cobblestone <- roads_2014[roads_2014$SURFACETYP == "Cobbled Stone",] %>% raster::aggregate(by="id")
roads_2016_cobblestone <- roads_2016[roads_2016$SURFACETYP == "Cobbled Stone",] %>% raster::aggregate(by="id")

roads_1996_earth <- roads_1996[roads_1996$SURFACETYP == "Earth",] %>% raster::aggregate(by="id")
roads_1998_earth <- roads_1998[roads_1998$SURFACETYP == "Earth",] %>% raster::aggregate(by="id")
roads_2000_earth <- roads_2000[roads_2000$SURFACETYP == "Earth",] %>% raster::aggregate(by="id")
roads_2002_earth <- roads_2002[roads_2002$SURFACETYP == "Earth",] %>% raster::aggregate(by="id")
roads_2004_earth <- roads_2004[roads_2004$SURFACETYP == "Earth",] %>% raster::aggregate(by="id")
roads_2006_earth <- roads_2006[roads_2006$SURFACETYP == "Earth",] %>% raster::aggregate(by="id")
roads_2008_earth <- roads_2008[roads_2008$SURFACETYP == "Earth",] %>% raster::aggregate(by="id")
roads_2010_earth <- roads_2010[roads_2010$SURFACETYP == "Earth",] %>% raster::aggregate(by="id")
roads_2012_earth <- roads_2012[roads_2012$SURFACETYP == "Earth",] %>% raster::aggregate(by="id")
roads_2014_earth <- roads_2014[roads_2014$SURFACETYP == "Earth",] %>% raster::aggregate(by="id")
roads_2016_earth <- roads_2016[roads_2016$SURFACETYP == "Earth",] %>% raster::aggregate(by="id")

roads_1996_gravel <- roads_1996[roads_1996$SURFACETYP == "Gravel",] %>% raster::aggregate(by="id")
roads_1998_gravel <- roads_1998[roads_1998$SURFACETYP == "Gravel",] %>% raster::aggregate(by="id")
roads_2000_gravel <- roads_2000[roads_2000$SURFACETYP == "Gravel",] %>% raster::aggregate(by="id")
roads_2002_gravel <- roads_2002[roads_2002$SURFACETYP == "Gravel",] %>% raster::aggregate(by="id")
roads_2004_gravel <- roads_2004[roads_2004$SURFACETYP == "Gravel",] %>% raster::aggregate(by="id")
roads_2006_gravel <- roads_2006[roads_2006$SURFACETYP == "Gravel",] %>% raster::aggregate(by="id")
roads_2008_gravel <- roads_2008[roads_2008$SURFACETYP == "Gravel",] %>% raster::aggregate(by="id")
roads_2010_gravel <- roads_2010[roads_2010$SURFACETYP == "Gravel",] %>% raster::aggregate(by="id")
roads_2012_gravel <- roads_2012[roads_2012$SURFACETYP == "Gravel",] %>% raster::aggregate(by="id")
roads_2014_gravel <- roads_2014[roads_2014$SURFACETYP == "Gravel",] %>% raster::aggregate(by="id")
roads_2016_gravel <- roads_2016[roads_2016$SURFACETYP == "Gravel",] %>% raster::aggregate(by="id")

roads_1996_majorgravel <- roads_1996[roads_1996$SURFACETYP == "Major Gravel",] %>% raster::aggregate(by="id")
roads_1998_majorgravel <- roads_1998[roads_1998$SURFACETYP == "Major Gravel",] %>% raster::aggregate(by="id")
roads_2000_majorgravel <- roads_2000[roads_2000$SURFACETYP == "Major Gravel",] %>% raster::aggregate(by="id")
roads_2002_majorgravel <- roads_2002[roads_2002$SURFACETYP == "Major Gravel",] %>% raster::aggregate(by="id")
roads_2004_majorgravel <- roads_2004[roads_2004$SURFACETYP == "Major Gravel",] %>% raster::aggregate(by="id")
roads_2006_majorgravel <- roads_2006[roads_2006$SURFACETYP == "Major Gravel",] %>% raster::aggregate(by="id")
roads_2008_majorgravel <- roads_2008[roads_2008$SURFACETYP == "Major Gravel",] %>% raster::aggregate(by="id")
roads_2010_majorgravel <- roads_2010[roads_2010$SURFACETYP == "Major Gravel",] %>% raster::aggregate(by="id")
roads_2012_majorgravel <- roads_2012[roads_2012$SURFACETYP == "Major Gravel",] %>% raster::aggregate(by="id")
roads_2014_majorgravel <- roads_2014[roads_2014$SURFACETYP == "Major Gravel",] %>% raster::aggregate(by="id")
roads_2016_majorgravel <- roads_2016[roads_2016$SURFACETYP == "Major Gravel",] %>% raster::aggregate(by="id")

# Calculate Distance -----------------------------------------------------------
points_1996 <- points
points_1996$distance_asphaltconcrete <- gDistance_chunks(points, roads_1996_asphaltconcrete, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
#points_1996$distance_cobblestone <- gDistance_chunks(points, roads_1996_cobblestone, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_1996$distance_earth <- gDistance_chunks(points, roads_1996_earth, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_1996$distance_gravel <- gDistance_chunks(points, roads_1996_gravel, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_1996$distance_majorgravel <- gDistance_chunks(points, roads_1996_majorgravel, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_1996$year <- 1996

points_1998 <- points
points_1998$distance_asphaltconcrete <- gDistance_chunks(points, roads_1998_asphaltconcrete, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
#points_1998$distance_cobblestone <- gDistance_chunks(points, roads_1998_cobblestone, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_1998$distance_earth <- gDistance_chunks(points, roads_1998_earth, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_1998$distance_gravel <- gDistance_chunks(points, roads_1998_gravel, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_1998$distance_majorgravel <- gDistance_chunks(points, roads_1998_majorgravel, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_1998$year <- 1998

points_2000 <- points
points_2000$distance_asphaltconcrete <- gDistance_chunks(points, roads_2000_asphaltconcrete, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
#points_2000$distance_cobblestone <- gDistance_chunks(points, roads_2000_cobblestone, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_2000$distance_earth <- gDistance_chunks(points, roads_2000_earth, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_2000$distance_gravel <- gDistance_chunks(points, roads_2000_gravel, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_2000$distance_majorgravel <- gDistance_chunks(points, roads_2000_majorgravel, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_2000$year <- 2000

points_2002 <- points
points_2002$distance_asphaltconcrete <- gDistance_chunks(points, roads_2002_asphaltconcrete, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
#points_2002$distance_cobblestone <- gDistance_chunks(points, roads_2002_cobblestone, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_2002$distance_earth <- gDistance_chunks(points, roads_2002_earth, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_2002$distance_gravel <- gDistance_chunks(points, roads_2002_gravel, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_2002$distance_majorgravel <- gDistance_chunks(points, roads_2002_majorgravel, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_2002$year <- 2002

points_2004 <- points
points_2004$distance_asphaltconcrete <- gDistance_chunks(points, roads_2004_asphaltconcrete, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
#points_2004$distance_cobblestone <- gDistance_chunks(points, roads_2004_cobblestone, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_2004$distance_earth <- gDistance_chunks(points, roads_2004_earth, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_2004$distance_gravel <- gDistance_chunks(points, roads_2004_gravel, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_2004$distance_majorgravel <- gDistance_chunks(points, roads_2004_majorgravel, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_2004$year <- 2004

points_2006 <- points
points_2006$distance_asphaltconcrete <- gDistance_chunks(points, roads_2006_asphaltconcrete, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_2006$distance_cobblestone <- gDistance_chunks(points, roads_2006_cobblestone, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_2006$distance_earth <- gDistance_chunks(points, roads_2006_earth, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_2006$distance_gravel <- gDistance_chunks(points, roads_2006_gravel, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_2006$distance_majorgravel <- gDistance_chunks(points, roads_2006_majorgravel, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_2006$year <- 2006

points_2008 <- points
points_2008$distance_asphaltconcrete <- gDistance_chunks(points, roads_2008_asphaltconcrete, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_2008$distance_cobblestone <- gDistance_chunks(points, roads_2008_cobblestone, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_2008$distance_earth <- gDistance_chunks(points, roads_2008_earth, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_2008$distance_gravel <- gDistance_chunks(points, roads_2008_gravel, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_2008$distance_majorgravel <- gDistance_chunks(points, roads_2008_majorgravel, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_2008$year <- 2008

points_2010 <- points
points_2010$distance_asphaltconcrete <- gDistance_chunks(points, roads_2010_asphaltconcrete, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_2010$distance_cobblestone <- gDistance_chunks(points, roads_2010_cobblestone, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_2010$distance_earth <- gDistance_chunks(points, roads_2010_earth, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_2010$distance_gravel <- gDistance_chunks(points, roads_2010_gravel, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_2010$distance_majorgravel <- gDistance_chunks(points, roads_2010_majorgravel, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_2010$year <- 2010

points_2012 <- points
points_2012$distance_asphaltconcrete <- gDistance_chunks(points, roads_2012_asphaltconcrete, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_2012$distance_cobblestone <- gDistance_chunks(points, roads_2012_cobblestone, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_2012$distance_earth <- gDistance_chunks(points, roads_2012_earth, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_2012$distance_gravel <- gDistance_chunks(points, roads_2012_gravel, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_2012$distance_majorgravel <- gDistance_chunks(points, roads_2012_majorgravel, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_2012$year <- 2012

points_2014 <- points
points_2014$distance_asphaltconcrete <- gDistance_chunks(points, roads_2014_asphaltconcrete, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_2014$distance_cobblestone <- gDistance_chunks(points, roads_2014_cobblestone, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_2014$distance_earth <- gDistance_chunks(points, roads_2014_earth, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_2014$distance_gravel <- gDistance_chunks(points, roads_2014_gravel, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_2014$distance_majorgravel <- gDistance_chunks(points, roads_2014_majorgravel, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_2014$year <- 2014

points_2016 <- points
points_2016$distance_asphaltconcrete <- gDistance_chunks(points, roads_2016_asphaltconcrete, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_2016$distance_cobblestone <- gDistance_chunks(points, roads_2016_cobblestone, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_2016$distance_earth <- gDistance_chunks(points, roads_2016_earth, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_2016$distance_gravel <- gDistance_chunks(points, roads_2016_gravel, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_2016$distance_majorgravel <- gDistance_chunks(points, roads_2016_majorgravel, CHUNK_SIZE_DIST_ROADS, MCCORS_DIST_ROADS)  
points_2016$year <- 2016

# Append Data ------------------------------------------------------------------
points_allyears <- bind_rows(points_1996@data,
                             points_1996@data %>% mutate(year = 1997),
                             points_1998@data,
                             points_1998@data %>% mutate(year = 1999),
                             points_2000@data,
                             points_2000@data %>% mutate(year = 2001),
                             points_2002@data,
                             points_2002@data %>% mutate(year = 2003),
                             points_2004@data,
                             points_2004@data %>% mutate(year = 2005),
                             points_2006@data,
                             points_2006@data %>% mutate(year = 2007),
                             points_2008@data,
                             points_2008@data %>% mutate(year = 2009),
                             points_2010@data,
                             points_2010@data %>% mutate(year = 2011),
                             points_2012@data,
                             points_2012@data %>% mutate(year = 2013),
                             points_2014@data,
                             points_2014@data %>% mutate(year = 2015),
                             points_2016@data)

# Export -----------------------------------------------------------------------
saveRDS(points_allyears, file.path(outputs_for_grid, TYPE, "points_distance_roads_bytype.Rds"))

