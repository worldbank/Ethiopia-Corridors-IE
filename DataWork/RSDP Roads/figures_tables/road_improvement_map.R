# Road Improvement Map

# Load Data --------------------------------------------------------------------
roads <- readRDS(file.path(data_file_path, "RSDP Roads", "FinalData", "RoadNetworkPanelData_1996_2016.Rds"))
eth_adm <- readRDS(file.path(data_file_path, "GADM", "RawData", "gadm36_ETH_0_sp.rds"))

# Prep Data --------------------------------------------------------------------
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
ggplot() +
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
  labs(color = "Road\nImprovement\nYear") +
  theme(plot.background = element_rect(fill = "black",
                                       color = "black"),
        legend.title = element_text(color = "white", hjust = 0.5),
        legend.text = element_text(color = "white")) +
  coord_quickmap() +
  ggsave(file.path(data_file_path, "RSDP Roads", "Outputs", "figures",
                   "road_improvement_map.png"),
         height = 6,
         width = 8)









