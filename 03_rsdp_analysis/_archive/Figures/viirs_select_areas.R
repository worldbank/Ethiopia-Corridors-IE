# Changes in NTL in Ethiopia 

# Setup ------------------------------------------------------------------------
if(Sys.info()[["user"]] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE"
if(Sys.info()[["user"]] == "robmarty") project_file_path <- "~/Dropbox/World Bank/IEs/Ethiopia IE"

# Load Data --------------------------------------------------------------------
eth_adm0 <- getData('GADM', country='ETH', level=0)

cities <- c("Mekele", 13.497833, 39.468296) %>% t %>% as.data.frame
names(cities) <- c("city","lat","long")

cities$city <- cities$cit %>% as.character
cities$lat <- cities$lat %>% as.character %>% as.numeric
cities$long <- cities$long %>% as.character %>% as.numeric

coordinates(cities) <- ~long+lat
crs(cities) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

cities_buff <- gBuffer(cities, width=25/111.12, byid=T)

for(year in 1992:2013){
  ntl <- raster(file.path(project_file_path, "Data", "RawData", "NTL Rasters Annual", "GEE", "Median", paste0("eth_viirs_",year,"_median.tif"))) %>% crop(cities_buff) %>% mask(cities_buff)
  ntl[][ntl[] < 3] <- 0
  
  ntl[] <- log(ntl[])
  
  ntl.df <- as(ntl, "SpatialPixelsDataFrame")
  ntl.df <- as.data.frame(ntl.df)
  colnames(ntl.df) <- c("value", "x", "y") 
  
  ntl_map <- ggplot() +
    geom_polygon(data=cities_buff, aes(x=long,y=lat,group=group),fill="black") +
    geom_tile(data=ntl.df[ntl.df$value > 0,], aes(x=x,y=y,fill=value)) +
    labs(colour="") +
    coord_equal() +
    theme_void() +
    theme(legend.key=element_blank(),
          legend.text = element_text(size=11),
          legend.title = element_text(size=11)) +
    labs(title=year) +
    #scale_fill_gradient(name="Nighttime Lights", 
    #                    low = "black", high = "yellow",
    #                    breaks=c(quantile(viirs.df$value, 0.10),
    #                             quantile(viirs.df$value, 0.50),
    #                             quantile(viirs.df$value, 0.99999)),
    #                    labels=c("Minimum","","Maximum")) +
    scale_fill_gradient2(name="Nighttime Lights", 
                         low = "yellow", mid="orange", high = "red",midpoint=3) +
    theme(legend.position="none",
          plot.title = element_text(hjust = 0.5, size=18))
  ggsave(ntl_map, filename=file.path(project_file_path, "Figures", "viirs","mekele", paste0("dmspols_",year,".png")), height=9, width=7)
}