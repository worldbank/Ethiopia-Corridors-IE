# Figures for Annual Report - 2021

# Load Data --------------------------------------------------------------------
mh_road <- readRDS(file.path(project_file_path, "Data", "RSDP Roads", "FinalData", "modjo_hawassa.Rds"))

viirs_15 <- raster(file.path(project_file_path, "Data", "Nighttime Lights", "VIIRS", "RawData", "Annual", "median", "eth_viirs_median_2015.tif"))
viirs_16 <- raster(file.path(project_file_path, "Data", "Nighttime Lights", "VIIRS", "RawData", "Annual", "median", "eth_viirs_median_2016.tif"))
viirs_17 <- raster(file.path(project_file_path, "Data", "Nighttime Lights", "VIIRS", "RawData", "Annual", "median", "eth_viirs_median_2017.tif"))
viirs_18 <- raster(file.path(project_file_path, "Data", "Nighttime Lights", "VIIRS", "RawData", "Annual", "median", "eth_viirs_median_2017.tif"))
viirs_19 <- raster(file.path(project_file_path, "Data", "Nighttime Lights", "VIIRS", "RawData", "Annual", "median", "eth_viirs_median_2017.tif"))
viirs_20 <- raster(file.path(project_file_path, "Data", "Nighttime Lights", "VIIRS", "RawData", "Annual", "median", "eth_viirs_median_2020.tif"))

# Average NTL Trends -----------------------------------------------------------
compute_ntl_stats <- function(year, buffer){
  print(year)
  
  mh_road_buff <- gBuffer(mh_road, width = buffer/111.12, byid=T)
  
  viirs <- raster(file.path(project_file_path, "Data", "Nighttime Lights", "VIIRS", "RawData", "Annual", "median", paste0("eth_viirs_median_",year,".tif")))
  
  viirs <- viirs %>% crop(mh_road_buff) %>% mask(mh_road_buff)
  
  data.frame(year = year,
             buffer = buffer,
             mean = viirs[] %>% mean(na.rm = T),
             median = viirs[] %>% median(na.rm = T))
}

ntl_stats <- map_df(2012:2020, compute_ntl_stats, 10)

ntl_stats %>%
  ggplot(aes(x = year, y = mean)) +
  geom_line(size = 2,
            color = "black") +
  geom_point() +
  ylim(c(0, 0.5)) +
  labs(x = NULL,
       y = "Luminosity",
       title = "Average Luminosity within 10km of the\nModjo-Hawassa Expressway from 2012 - 2020") +
  theme_ipsum()

# Maps -------------------------------------------------------------------------
## Buffer
mh_road_10km <- mh_road %>% gBuffer(width = 10/111.12, byid=T)

## 2016
buffer_plot <- 15

viirs_15_mh <- viirs_15 %>%
  crop(mh_road %>% gBuffer(width = buffer_plot/111.12, byid=T))

viirs_15_mh_df <- viirs_16_mh %>%
  coordinates() %>%
  as.data.frame()
viirs_15_mh_df$value <- viirs_15_mh[]
viirs_15_mh_df$value_l <- log(viirs_15_mh_df$value+1)

## 2020
viirs_20_mh <- viirs_20 %>%
  crop(mh_road %>% gBuffer(width = buffer_plot/111.12, byid=T))

viirs_20_mh_df <- viirs_20_mh %>%
  coordinates() %>%
  as.data.frame()
viirs_20_mh_df$value <- viirs_20_mh[]
viirs_20_mh_df$value_l <- log(viirs_20_mh_df$value+1)

## Figures
viirs_15_mh_df$value_l %>% summary()
viirs_20_mh_df$value_l %>% summary()

p15 <- ggplot() +
  geom_raster(data = viirs_15_mh_df,
              aes(x = x, y=y, fill = value_l)) +
  geom_polygon(data = mh_road_10km,
               aes(x = long, y=lat, group = group),
               fill = NA,
               color = "white",
               size = 0.1) +
  scale_fill_gradient2(low = "black",
                       mid = "yellow",
                       high = "gold",
                       midpoint = 2,
                       limits = c(-0.03, 3.8)) +
  labs(title = "2015") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  coord_quickmap()

p20 <- ggplot() +
  geom_raster(data = viirs_20_mh_df,
              aes(x = x, y=y, fill = value_l)) +
  geom_polygon(data = mh_road_10km,
               aes(x = long, y=lat, group = group),
               fill = NA,
               color = "white",
               size = 0.1) +
  scale_fill_gradient2(low = "black",
                       mid = "yellow",
                       high = "gold",
                       midpoint = 2,
                       limits = c(0.1, 3.8)) +
  labs(title = "2020") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  coord_quickmap()

ggarrange(p15, p20)




