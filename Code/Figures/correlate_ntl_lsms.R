# Correlate LSMS with NTL

# Setup ------------------------------------------------------------------------
if(Sys.info()[["user"]] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE"
if(Sys.info()[["user"]] == "robmarty") project_file_path <- "~/Dropbox/World Bank/IEs/Ethiopia IE"

library(readstata13)
library(doBy)
library(rgdal)
library(raster)
library(velox)
library(rgeos)
library(ggplot2)

# Load Data --------------------------------------------------------------------
#### LSMS
lsms_df <- read.dta13(file.path(project_file_path, "Data", "RawData", "LSMS", "Merged Data", "lsms_merged.dta"))
lsms_df <- lsms_df[lsms_df$wave %in% 3, ]
lsms_df <- lsms_df[!is.na(lsms_df$latitude),]
lsms_df$coord_id <- paste0(lsms_df$longitude, lsms_df$latitude)
coord_id_df <- table(lsms_df$coord_id) %>% as.data.frame
names(coord_id_df) <- c("coord_id","num_HH")
coord_id_df$coord_id <- as.character(coord_id_df$coord_id)
lsms_df <- summaryBy(total_cons_ann+nom_totcons_aeq+food_cons_ann+nonfood_cons_ann+educ_cons_ann+cons_quint~ longitude+latitude+coord_id, data=lsms_df, keep.names=T, FUN=median, na.rm=T)
lsms_df <- merge(lsms_df, coord_id_df, by="coord_id")

coordinates(lsms_df) <- ~longitude+latitude
crs(lsms_df) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
lsms_df <- gBuffer(lsms_df, width=1/111.12, byid=T)

#### VIIRS
viirs <- raster(file.path(project_file_path, "Data", "RawData", "NTL Rasters Annual", "GEE", "Median", "eth_viirs_2015_median.tif"))

# Extract NTL to Data ----------------------------------------------------------
lsms_df$viirs <- velox(viirs)$extract(sp=lsms_df, fun=median) %>% as.numeric
lsms_df <- lsms_df[!is.na(lsms_df$viirs),]
lsms_df <- lsms_df[lsms_df$num_HH > 3,]

quartiles <- quantile(lsms_df$viirs, probs = seq(0, 1, 0.25)) %>% as.numeric
lsms_df$viirs_q <- NA

lsms_df$viirs_ln <- log(lsms_df$viirs)
lm(total_cons_ann ~ viirs_ln  , data=lsms_df) %>% summary
cor.test(lsms_df$viirs_ln, lsms_df$total_cons_ann)

for(i in 1:(length(quartiles)-1)){
  lsms_df$viirs_q[lsms_df$viirs >= quartiles[i] & lsms_df$viirs < quartiles[i+1]] <- i
  if(i == (length(quartiles)-1)) lsms_df$viirs_q[lsms_df$viirs >= quartiles[i] & lsms_df$viirs <= quartiles[i+1]] <- i
}

p <- ggplot() +
  geom_boxplot(data=lsms_df@data, aes(x=viirs_q, y=total_cons_ann, group=viirs_q),
               fill="bisque3") + 
  theme_minimal() +
  labs(x="Nighttime Lights Quartile", 
       y="Median Consumption\nWithin Enumeration Area",
       title = "Consumption Across\nNighttime Light Quartiles") +
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=13),
        axis.title.y = element_text(size=13),
        plot.title = element_text(hjust = 0.5, size=15, face="bold"))
p
ggsave(p, filename = file.path(project_file_path, "Figures", "viirs_consumption_2015.png"),height=6, width=5)


