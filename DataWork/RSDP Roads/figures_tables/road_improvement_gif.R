# Road Improvement Map

# Load Data --------------------------------------------------------------------
roads <- readRDS(file.path(data_file_path, "RSDP Roads", "FinalData", "RoadNetworkPanelData_1996_2016.Rds"))
eth_adm <- readRDS(file.path(data_file_path, "GADM", "RawData", "gadm36_ETH_0_sp.rds"))

# Prep Data --------------------------------------------------------------------
## Road completion year
roads@data <- roads@data %>%
  select_if(str_detect(names(.), "Speed"))

## Tidy dataframe
roads$id <- row.names(roads)
roads_tidy <- tidy(roads)
roads_tidy <- merge(roads_tidy, roads@data, by = "id")

## Define color scale: spectral, but make reds brighter
colors <- rev(brewer.pal(n = 11, name = "Spectral"))
colors[10] <- "#ff2929" 
colors[11] <- "#ff0000" 

for(year in 1996:2016){
  print(year)
  
  roads_tidy_i <- roads_tidy[roads_tidy[[paste0("Speed", year)]] > 0,]
  roads_tidy_i$Speed <- roads_tidy_i[[paste0("Speed", year)]]
  
  roads_tidy_i$Speed[roads_tidy_i$Speed > 70] <- 70
  
  p <- ggplot() +
    geom_polygon(data = eth_adm,
                 aes(x = long, y = lat, group = group),
                 fill = NA, color = "white", size=.15) +
    geom_path(data = roads_tidy_i,
              aes(x = long, y = lat, group = group, color = Speed),
              size = .15) +
    theme_void() +
    scale_colour_gradientn(colours = colors,
                           limits = c(0, 70),
                           breaks = c(10, 30, 50, 70),
                           labels = c("10", "30", "50", ">70")) +
    labs(color = "Speed\nLimit (km/hr)",
         title = year) +
    theme(plot.background = element_rect(fill = "black",
                                         color = "black"),
          legend.title = element_text(color = "white", hjust = 0.5),
          legend.text = element_text(color = "white"),
          legend.position = c(0.83, 0.75),
          plot.title = element_text(hjust = 0.5, face = "bold", color = "white")) +
    coord_quickmap() 
  
    ggsave(p, filename = file.path(data_file_path, "RSDP Roads", "Outputs", "figures",
                     "road_improvement_gif", "pngs",
                     paste0("roads_",year,".png")),
           height = 6,
           width = 8)
}









