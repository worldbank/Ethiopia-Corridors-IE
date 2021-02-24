# Road Improvement Map

# Load Data --------------------------------------------------------------------
roads <- readRDS(file.path(data_file_path, "RSDP Roads", "FinalData", "RoadNetworkPanelData_1996_2016.Rds"))
eth_adm <- readRDS(file.path(data_file_path, "GADM", "RawData", "gadm36_ETH_0_sp.rds"))

dmsp1996 <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", "RawData", "Individual Files", "eth_dmspols_1996.tif")) %>% crop(eth_adm)
dmsp2012 <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", "RawData", "Individual Files", "eth_dmspols_2012.tif")) %>% crop(eth_adm)

gc1996 <- raster(file.path(data_file_path, "Globcover","RawData", "1992_2015_data", "ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif"), 1) %>% crop(eth_adm) %>% mask(eth_adm)
gc2016 <- raster(file.path(data_file_path, "Globcover","RawData", "2016_2018_data", "C3S-LC-L4-LCCS-Map-300m-P1Y-2016-v2.1.1.tif")) %>% crop(eth_adm) %>% mask(eth_adm)

# Nighttime Lights -------------------------------------------------------------
#### Prep Data
## 1996
dmsp1996_df <- dmsp1996 %>%
  coordinates() %>%
  as.data.frame() 
dmsp1996_df$value <- dmsp1996[]
dmsp1996_df$value_log <- log(dmsp1996_df$value + 1)

## 2012
dmsp2012_df <- dmsp2012 %>%
  coordinates() %>%
  as.data.frame() 
dmsp2012_df$value <- dmsp2012[]
dmsp2012_df$value_log <- log(dmsp2012_df$value + 1)

#### Map
make_dmsp_figure <- function(df, title){
  
  ggplot() +
    #geom_raster(data = df,
    #            aes(x = x, y = y),
    #            fill = "black") +
    geom_polygon(data = eth_adm,
                 aes(x = long, y = lat, group = group),
                 color = "white", fill = "black",
                 size = .15) +
    geom_raster(data = df[df$value_log > 0,] , 
                aes(x = x, y = y,
                    fill = value_log)) + 
    scale_fill_gradient2(low = "yellow",
                         mid = "gold",
                         high = "firebrick2",
                         midpoint = 2,
                         limits = c(0, 4.2)) +
    labs(title = title) +
    coord_quickmap() +
    theme_void() +
    theme(legend.position = "none",
          plot.background = element_rect(fill = "black",
                                         color = "black"),
          plot.title = element_text(hjust = 0.5, color = "white"))
  
}

p_dmsp1996 <- make_dmsp_figure(dmsp1996_df, "1996")
p_dmsp2012 <- make_dmsp_figure(dmsp2012_df, "2012")

p_dmsp <- ggarrange(p_dmsp1996, p_dmsp2012, nrow = 1) %>%
  annotate_figure(top = text_grob("Nighttime Lights", color = "white", face = "bold", size = 14, vjust = 1)) +
  bgcolor("black") + 
  border("black")

ggsave(p_dmsp,  filename = "~/Desktop/dmsp.png", height = 4, width = 8)

# GlobCover-Urban --------------------------------------------------------------
#### Prep Data
## 1996
gc1996_df <- gc1996 %>%
  coordinates() %>%
  as.data.frame() 
gc1996_df$value <- as.numeric(gc1996[] == 190)

## 2016
gc2016_df <- gc2016 %>%
  coordinates() %>%
  as.data.frame() 
gc2016_df$value <- as.numeric(gc2016[] == 190)

#### Map
make_gc_figure <- function(df, title){
  
  ggplot() +
    # geom_raster(data = df,
    #             aes(x = x, y = y),
    #             fill = "black") +
    geom_polygon(data = eth_adm,
                 aes(x = long, y = lat, group = group),
                 color = "white", fill = "black",
                 size = .15) +
    geom_raster(data = df[df$value > 0,] , 
                aes(x = x, y = y),
                fill = "red") + 
    labs(title = title) +
    coord_quickmap() +
    theme_void() +
    theme(legend.position = "none",
          plot.background = element_rect(fill = "black",
                                         color = "black"),
          plot.title = element_text(hjust = 0.5, color = "white"))
  
}

p_gc1996 <- make_gc_figure(gc1996_df, "1996")
p_gc2016 <- make_gc_figure(gc2016_df, "2016")

p_gc <- ggarrange(p_gc1996, p_gc2016, nrow = 1) %>%
  annotate_figure(top = text_grob("GlobCover-Urban", color = "white", face = "bold", size = 14, vjust = 1)) +
  bgcolor("black") + 
  border("black")

#ggsave(p_gc,  filename = "~/Desktop/test.png", height = 4, width = 8)

# RSDP Upgrades ----------------------------------------------------------------
## Road completion year
roads@data <- roads@data %>%
  dplyr::rename(completion_year = Complete_G) %>%
  dplyr::select(completion_year)

roads_existing <- roads[roads$completion_year <= 1996,]
roads_improved <- roads[roads$completion_year > 1996,]

## Tidy dataframe
roads_improved$id <- row.names(roads_improved)
roads_improved_tidy <- tidy(roads_improved)
roads_improved_tidy <- merge(roads_improved_tidy, roads_improved@data, by = "id")

## Factor 
p_rsdp <- ggplot() +
  geom_polygon(data = eth_adm,
               aes(x = long, y = lat, group = group),
               fill = NA, color = "white", size=.2) +
  geom_path(data = roads_existing,
            aes(x = long, y = lat, group = group),
            color = "gray",
            size = .15) +
  geom_path(data = roads_improved_tidy,
            aes(x = long, y = lat, group = group, color = completion_year),
            size = .15) +
  theme_void() +
  scale_colour_gradientn(colours = rev(brewer.pal(n = 11, name = "Spectral"))) +
  labs(color = "Road\nImprovement\nYear",
       title = "Road Improvements") +
  theme(plot.background = element_rect(fill = "black",
                                       color = "black"),
        plot.title = element_text(hjust = 0.5, color = "white", face = "bold"),
        legend.title = element_text(color = "white", hjust = 0.5),
        legend.text = element_text(color = "white")) +
  coord_quickmap() 

# Append and Export ------------------------------------------------------------
p_all <- ggarrange(p_dmsp,
                   p_gc, 
                   p_rsdp,
                   ncol = 1) +
  bgcolor("black") + 
  border("black")

ggsave(p_all, filename = file.path(paper_figures,
                                   "maps_NTL_GC_RSDP.png"),
       height = 12,
       width = 8)









