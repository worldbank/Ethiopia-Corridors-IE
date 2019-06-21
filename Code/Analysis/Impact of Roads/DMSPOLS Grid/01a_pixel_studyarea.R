# Impact of Expressway Expansion
# Ethiopia IE

# Setup ------------------------------------------------------------------------
if(Sys.info()[["user"]] == "r521633") project_file_path <- "/home/wb521633/IEs/Ethiopia IE"
if(Sys.info()[["user"]] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE"
if(Sys.info()[["user"]] == "robmarty") project_file_path <- "~/Dropbox/World Bank/IEs/Ethiopia IE"

tables_file_path <- file.path(project_file_path, "Results", "Tables")
figures_file_path <- file.path(project_file_path, "Results", "Figures")

library(lfe)
library(reshape)
library(dplyr)
library(ggplot2)
library(data.table)
library(coefplot)
library(stringr)
library(doBy)
library(stargazer)
library(raster)
source("https://raw.githubusercontent.com/ramarty/rgeos_chunks/master/R/rgeos_chunks.R")

DIST_THRESH <- 2 #km to be considered near a road

dataset <- "cluster_all"

# Load Data --------------------------------------------------------------------
#### Ethiopia
eth <- getData('GADM', country='ETH', level=0)

#### Road
setwd(file.path(rawdata_file_path, "RoadNetworkPanelDataV3_1996_2016_Revised"))
roads_2016 <- readOGR(dsn=".", layer=paste0("All_Network_","2016")) %>% spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
roads_2016 <- roads_2016[roads_2016$Speed2012 > 0,]

head(roads_2016)
roads_2016$improved_rd <- as.numeric(roads_2016$Speed2012 > roads_2016$Speed1996)
roads_2016$improved_rd_50above <- (roads_2016$improved_rd %in% 1) & (roads_2016$Speed2012 >= 50)
roads_2016$improved_rd_below50 <- (roads_2016$improved_rd %in% 1) & (roads_2016$Speed2012 < 50)

roads_2016_improved_rd <- roads_2016[roads_2016$improved_rd %in% 1,]
roads_2016_improved_rd_50above <- roads_2016[roads_2016$improved_rd_50above %in% 1,]
roads_2016_improved_rd_below50 <- roads_2016[roads_2016$improved_rd_below50 %in% 1,]

# Buffer -----------------------------------------------------------------------
roads_2016_improved_rd_buff <- gBuffer_chunks(roads_2016_improved_rd,
                                         width=15/111.12,
                                         chunk_size=10)

map <- ggplot() +
  geom_polygon(data=eth,
               aes(x=long, y=lat, group=group),fill="gray75") +
  #geom_path(data=roads_2016[1:1000,],
  #          aes(x=long, y=lat, group=group),color="gray70",size=0.2) +
  geom_polygon(data=roads_2016_improved_rd_buff,
               aes(x=long, y=lat, group=group, fill="Study Area")) +
  geom_path(data=roads_2016_improved_rd_50above,
               aes(x=long, y=lat, group=group,
                   color="Speed Limit\n50 km/hr\nand Above\nat Endline\n "),
            size=0.4) +
  geom_path(data=roads_2016_improved_rd_below50,
               aes(x=long, y=lat, group=group,
                   color="Speed Limit\nBelow\n50 km/hr\nat Endline\n "),
            size=0.4) +
  scale_color_manual(values=c("deepskyblue","darkorange")) +
  scale_fill_manual(values=c("gray40")) +
  theme_void() +
  theme(legend.title = element_text(size=14),
        legend.text = element_text(size=13)) +
  coord_quickmap() +
  labs(color="Improved Road",
       fill = "") +
  guides(color = guide_legend(override.aes = list(size = 1)))
ggsave(map, filename=file.path(figures_file_path, "improved_roads_abvoebelow50.png"),
       height=6,width=6)

