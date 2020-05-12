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

cities_buff <- gBuffer(cities, width=10/111.12, byid=T)

time <- 1
for(year in 1992:2013){
  globcover <- raster(file.path(project_file_path, "Data", "RawData", "esa_globcover", "scratch", "ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif"), time) %>% crop(cities_buff) %>% mask(cities_buff)
  
  globcover.df <- as(globcover, "SpatialPixelsDataFrame")
  globcover.df <- as.data.frame(globcover.df)
  colnames(globcover.df) <- c("value", "x", "y") 
  
  #globcover.df$landclass[globcover.df$value %in% c(200,201,202)] <- "Base Areas"
  #globcover.df$landclass[globcover.df$value %in% c(190)] <- "Urban"
  #globcover.df$landclass[globcover.df$value %in% c(50,60,61,62,70,71,72,80,81,82,90,100,160,170)] <- "Forest"
  #globcover.df$landclass[globcover.df$value %in% c(110,130)] <- "Grassland"
  #globcover.df$landclass[globcover.df$value %in% c(120,121,122)] <- "Shrubland"
  #globcover.df$landclass[globcover.df$value %in% c(140,150,151,152,153)] <- "Sparse Vegetation"
  #globcover.df$landclass[globcover.df$value %in% c(180)] <- "Wetland"
  #globcover.df$landclass[globcover.df$value %in% c(210)] <- "Water"
  #globcover.df$landclass[globcover.df$value %in% c(10,11,12,20,30,40)] <- "Agriculture"
  
  globcover.df$landclass[globcover.df$value %in% c(190)] <- "Urban"
  globcover.df$landclass[globcover.df$value %in% c(50,60,61,62,70,71,72,80,81,82,90,100,160,170)] <- "Forest"
  globcover.df$landclass[globcover.df$value %in% c(110,130)] <- "Grassland"
  globcover.df$landclass[globcover.df$value %in% c(120,121,122,200,201,202)] <- "Shrubland"
  globcover.df$landclass[globcover.df$value %in% c(140,150,151,152,153)] <- "Sparse Vegetation"
  globcover.df$landclass[globcover.df$value %in% c(180)] <- "Wetland"
  globcover.df$landclass[globcover.df$value %in% c(210)] <- "Water"
  globcover.df$landclass[globcover.df$value %in% c(10,11,12,20,30,40)] <- "Agriculture"

  globcover.df$landclass <- as.factor(globcover.df$landclass)

  ntl_map <- ggplot() +
    geom_tile(data=globcover.df, aes(x=x,y=y,fill=landclass)) +
    labs(colour="") +
    coord_equal() +
    theme_void() +
    theme(legend.key=element_blank(),
          legend.text = element_text(size=16),
          legend.title = element_text(size=13)) +
    labs(title=year,fill="Land Class") +
    scale_fill_manual(values=c("goldenrod2","springgreen4","lawngreen","olivedrab","red","blue")) +
    #scale_fill_gradient(name="Nighttime Lights", 
    #                    low = "black", high = "yellow",
    #                    breaks=c(quantile(viirs.df$value, 0.10),
    #                             quantile(viirs.df$value, 0.50),
    #                             quantile(viirs.df$value, 0.99999)),
    #                    labels=c("Minimum","","Maximum"))
    theme(
          plot.title = element_text(hjust = 0.5, size=16))
  ggsave(ntl_map, filename=file.path(project_file_path, "Figures", "globcover","mekele", paste0("globcover_",year,".png")), height=9, width=7)
  time <- time + 1
  print(time)
}