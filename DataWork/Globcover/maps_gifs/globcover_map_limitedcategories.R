# Globcover Map of Ethiopia

# 3.1.1 / Table 3-3
# http://maps.elie.ucl.ac.be/CCI/viewer/download/ESACCI-LC-Ph2-PUGv2_2.0.pdf

# Load Data --------------------------------------------------------------------
eth_adm <- readRDS(file.path(data_file_path, "GADM", "RawData", "gadm36_ETH_0_sp.rds"))

# Load Data --------------------------------------------------------------------
time <- 5 # time = 5 = 1996
for(year in 1996:2016){
  print(year)

  ## Load Globcover Band
  if(year %in% 1996:2015){
    globcover <- raster(file.path(data_file_path, "Globcover", "RawData", "1992_2015_data", "ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif"), time) 
  } else{
    globcover <- raster(file.path(data_file_path, "Globcover", "RawData", "2016_2018_data", paste0("C3S-LC-L4-LCCS-Map-300m-P1Y-",year,"-v2.1.1.tif"))) 
  }
  
  ## Crop/Mask
  globcover <- globcover %>%
    crop(eth_adm) %>%
    mask(eth_adm)
  
  # Simplify Globcover Categories ------------------------------------------------
  globcover.df <- as(globcover, "SpatialPixelsDataFrame")
  globcover.df <- as.data.frame(globcover.df)
  colnames(globcover.df) <- c("value", "x", "y") 
  
  globcover.df$landclass <- NA
  globcover.df$landclass[globcover.df$value %in% c(200,201,202)] <- "Bare Areas"
  globcover.df$landclass[globcover.df$value %in% c(190)] <- "Urban"
  globcover.df$landclass[globcover.df$value %in% c(50,60,61,62,70,71,72,80,81,82,90,100,160,170)] <- "Forest"
  globcover.df$landclass[globcover.df$value %in% c(110,130)] <- "Grassland"
  globcover.df$landclass[globcover.df$value %in% c(120,121,122)] <- "Shrubland"
  globcover.df$landclass[globcover.df$value %in% c(140,150,151,152,153)] <- "Sparse Vegetation"
  globcover.df$landclass[globcover.df$value %in% c(180)] <- "Wetland"
  globcover.df$landclass[globcover.df$value %in% c(210)] <- "Water"
  globcover.df$landclass[globcover.df$value %in% c(10,11,12,20,30,40)] <- "Agriculture"
  
  ### FOUR CATEGORIES
  # globcover.df$landclass[globcover.df$landclass %in% c("Forest", "Grassland", "Shrubland", "Sparse Vegetation", "Bare Areas")] <- "Vegetation"
  # globcover.df$landclass[globcover.df$landclass %in% c("Water", "Wetland")] <- "Water/Wetlands"
  
  ### OTHER 
  globcover.df$landclass[globcover.df$landclass %in% c("Forest", "Grassland", "Shrubland", "Sparse Vegetation")] <- "Vegetation"
  globcover.df$landclass[globcover.df$landclass %in% c("Water", "Wetland")] <- "Water/Wetlands"
  
  globcover.df$landclass <- as.factor(globcover.df$landclass)
  
  # Country Map ----------------------------------------------------------------
  globcover_map <- ggplot() +
    #geom_polygon(data=eth_adm0, aes(x=long,y=lat,group=group),fill=NA,color="black",size=.5) +
    geom_tile(data=globcover.df, aes(x=x,y=y,fill=landclass)) +
    labs(colour="") +
    coord_equal() +
    theme_void() +
    theme(plot.background = element_rect(fill = "black",
                                         color = "black"),
          legend.key=element_blank(),
          legend.position = c(0.8, 0.75),
          plot.title = element_text(hjust = 0.5, size=16, face = "bold", color = "white"),
          legend.text = element_text(size=14, color = "white"),
          legend.title = element_text(size=14, color = "white")) +
    labs(title=year,
         fill="Land Class") +
    scale_fill_manual(values=c("lightgoldenrod1", "cornsilk", "red","chartreuse3","dodgerblue")) 
  ggsave(globcover_map, filename=file.path(data_file_path, "Globcover", "Outputs", "figures",
                                           "country_level_gif",
                                           "pngs", paste0("globcover_", year, ".png")),
                                           height = 6,
                                           width = 8)
  time <- time + 1

}


