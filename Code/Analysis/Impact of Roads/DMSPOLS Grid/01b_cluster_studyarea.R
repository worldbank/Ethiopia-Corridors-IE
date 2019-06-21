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
library(broom)
source("https://raw.githubusercontent.com/ramarty/rgeos_chunks/master/R/rgeos_chunks.R")

set.seed(42)

# Load Data --------------------------------------------------------------------
#### Ethiopia
eth <- getData('GADM', country='ETH', level=0)

#### Urban Clusters
clumps_sf <- read_sf(file.path(finaldata_file_path, "urban_cluster_dataset", "individual_files", "urban_cluster.geojson")) %>% as("Spatial")

clumps_sf$color <- sample(x=c("firebrick1","deepskyblue","chartreuse1","gold","darkorange","darkorchid1"),size=nrow(clumps_sf), replace=T)



clumps_sf$id <- row.names(clumps_sf)
clumps_sf_tidy <- tidy(clumps_sf)
clumps_sf_tidy <- merge(clumps_sf_tidy, clumps_sf, by="id")

# Figure -----------------------------------------------------------------------
map <- ggplot() +
  geom_polygon(data=eth,
               aes(x=long, y=lat, group=group),fill="gray20") +
  geom_polygon(data=clumps_sf_tidy,
               aes(x=long, y=lat, group=group),fill=clumps_sf_tidy$color,
               color="gray60",size=0.1) +
  theme_void() +
  coord_quickmap()
ggsave(map, filename=file.path(figures_file_path, "ntl_clusters.png"),
       height=6,width=6)

