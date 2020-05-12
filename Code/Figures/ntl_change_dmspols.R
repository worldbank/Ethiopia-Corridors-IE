# Changes in NTL in Ethiopia 

# https://datacarpentry.org/r-raster-vector-geospatial/05-raster-multi-band-in-r/
# https://stackoverflow.com/questions/19289358/how-can-i-plot-a-image-with-x-y-r-g-b-coordinates-using-ggplot2
# Setup ------------------------------------------------------------------------
if(Sys.info()[["user"]] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE"
if(Sys.info()[["user"]] == "robmarty") project_file_path <- "~/Dropbox/World Bank/IEs/Ethiopia IE"

library(raster)
library(ggplot2)
library(rgdal)
library(dplyr)
library(ggmap)
library(sp)

DOWNLOAD_RAW_BASEMAP <- F

# Load and Prep Data ----------------------------------------------------------- 
# Basemap
if(DOWNLOAD_RAW_BASEMAP){
  # lon=38.78882, lat=8.092817
  
  register_google(key="AIzaSyDiyIIKZvdL5IzmHG1wYFLdjNpkmMS0VMg")
  
  basemap <- get_map(
                    #location = c(lon=40.015736 , lat=9.260966), # WITHOUT TITLE
                    location = c(lon=40.021322 , lat=9.772973), # WITH TITLE
                     maptype="satellite",
                     zoom=6,
                     source="google",
                     api_key = "AIzaSyDiyIIKZvdL5IzmHG1wYFLdjNpkmMS0VMg")
  
  saveRDS(basemap, file=file.path(project_file_path, "Data", "RawData", "Google Basemaps", "ethiopia_nearaddis_hawasa_basemap.Rds"))
} else{
  basemap <- readRDS(file.path(project_file_path, "Data", "RawData", "Google Basemaps", "ethiopia_nearaddis_hawasa_basemap.Rds"))
}

#### Bounding Box Around Basemap
box <- unlist(attr(basemap, which = "bb"))[c(2, 4, 1, 3)]

adjust <- 0

x_coord <- c(box[1]-adjust,  box[1]-adjust,  box[2]-adjust, box[2]-adjust, box[1]-adjust)
y_coord <- c(box[3]-adjust, box[4]-adjust, box[4]-adjust, box[3]-adjust, box[3]-adjust)
xym <- cbind(x_coord, y_coord)
p = Polygon(xym)
ps = Polygons(list(p),1)
cities_box = SpatialPolygons(list(ps))
crs(cities_box) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#### Rasters
ntl_2013 <- raster(file.path(project_file_path, "Data", "RawData", "DMSP_OLS", "GEE", paste0("eth_dmspols_",1996,".tif"))) %>% crop(cities_box)
ntl_2016 <- raster(file.path(project_file_path, "Data", "RawData", "DMSP_OLS", "GEE", paste0("eth_dmspols_",2012,".tif"))) %>% crop(cities_box)

ntl_2013.df <- as(ntl_2013, "SpatialPixelsDataFrame") %>% as.data.frame
ntl_2016.df <- as(ntl_2016, "SpatialPixelsDataFrame") %>% as.data.frame
colnames(ntl_2013.df) <- c("value_2013", "x", "y") 
colnames(ntl_2016.df) <- c("value_2016", "x", "y") 

ntl_2016.df$value_2013 <- ntl_2013.df$value_2013

ntl_2016.df$change <- ntl_2016.df$value_2016 - ntl_2016.df$value_2013

ntl_2016.df$changelog <- log(abs(ntl_2016.df$change)+1)
ntl_2016.df$changelog[ntl_2016.df$change < 0] <- -ntl_2016.df$changelog[ntl_2016.df$change < 0]

# Remove Certain Observations
#ntl_2016.df$changelog[ntl_2016.df$change < 0 & ntl_2016.df$value_2013 < 2] <- NA
#ntl_2016.df$changelog[ntl_2016.df$change > 0 & ntl_2016.df$value_2016 < .2] <- NA
ntl_2016.df$changelog[ntl_2016.df$changelog == 0] <- NA

ntl_2016.df$changelog_abs_norm <- abs(ntl_2016.df$changelog)
ntl_2016.df$changelog_abs_norm <- ntl_2016.df$changelog_abs_norm / max(ntl_2016.df$changelog_abs_norm, na.rm=T)
ntl_2016.df$changelog_abs_norm <- ntl_2016.df$changelog_abs_norm ^(.15)

# Increase/ Decrease Variable
ntl_2016.df$incdec <- NA
ntl_2016.df$incdec[ntl_2016.df$change > 0] <- "Increase"
ntl_2016.df$incdec[ntl_2016.df$change < 0] <- "Decrease"

#### Roads
setwd(file.path(project_file_path, "Data", "RawData", "RoadNetworkPanelDataV3_1996_2016_Revised"))
roads_1996 <- readOGR(dsn=".", layer="All_Network_1996") %>% spTransform("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") 
roads_2012 <- readOGR(dsn=".", layer="All_Network_2012") %>% spTransform("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") 

roads_1996$id <- row.names(roads_1996)
roads_1996_tidy <- tidy(roads_1996)
roads_1996_tidy <- merge(roads_1996_tidy, roads_1996, by="id")

roads_2012$id <- row.names(roads_2012)
roads_2012_tidy <- tidy(roads_2012)
roads_2012_tidy <- merge(roads_2012_tidy, roads_2012, by="id")

#### Ethiopia Polygon
setwd(file.path(project_file_path, "Data", "RawData", "GADM"))
eth_adm0 <- getData('GADM', country='ETH', level=0) %>% crop(bbox(cities_box)-.01)

# Plot -------------------------------------------------------------------------
#roads_2012_tidy <- roads_2012_tidy[1:20000,]
#roads_1996_tidy <- roads_1996_tidy[1:20000,]

ntl_map <- ggmap(basemap) +
  geom_polygon(data=cities_box, aes(x=long, y=lat, group=group),
               fill="black",
               alpha=.45) + 
  geom_polygon(data=eth_adm0, aes(x=long, y=lat, group=group),
               fill="black", color=NA, alpha=0.6) +
  geom_path(data=roads_2012_tidy, aes(x=long, y=lat, group=group, color="New Road"),
            alpha=0.45, size=.05) +
  geom_path(data=roads_1996_tidy, aes(x=long, y=lat, group=group, color="1996"),
            alpha=0.45, size=.05) +
  geom_tile(data=ntl_2016.df[ntl_2016.df$value_2016 != 0,], aes(x=x, y=y, fill="Newly Lit")) +
  geom_tile(data=ntl_2016.df[ntl_2016.df$value_2013 != 0,], aes(x=x, y=y, fill="1996")) +
  theme_void() +
  labs(fill = "Nighttime\nLights",
       title = "Change in Road Network and\nNighttime Lights from 1996 to 2012",
       color= "Road\nNetwork") +
  scale_color_manual(values=c("magenta", "green")) + # 
  scale_fill_manual(values=c("red", "cyan1")) +
  theme(legend.position = c(.895, .69), 
        legend.background = element_rect(fill=alpha("black", 0.77),
                                         size=.5, linetype="solid", 
                                         colour = "black"), # "wheat"
        plot.title = element_text(color="ivory", size=22, face="bold", hjust=0.5, vjust=-20, family="Times"),
        legend.text=element_text(color="ivory",size=14, family="Times"),
        legend.title=element_text(color="ivory",size=14, hjust=0.5, family="Times", face="bold"),
        legend.margin=margin(c(2,2,2,2))) +
  guides(color = guide_legend(override.aes = list(size=0.4))) 
  #theme(legend.position="none") 
ggsave(ntl_map, file=file.path(project_file_path, "Figures", "change_ntl_roads_96_12.png"), height=7, width=7, dpi = 500)


