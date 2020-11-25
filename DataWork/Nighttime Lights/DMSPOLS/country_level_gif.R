# Country Level DMSP-OLS GIF

# Load Data --------------------------------------------------------------------
eth_adm <- readRDS(file.path(data_file_path, "GADM", "RawData", "gadm36_ETH_0_sp.rds"))

# Map --------------------------------------------------------------------------
for(year in 1992:2013){
  
  ## Load/prep dmspols
  dmspols <- raster(file.path(data_file_path, "Nighttime Lights", "DMSPOLS", "RawData", 
                              "Individual Files", paste0("eth_dmspols_", year, ".tif")))
  
  dmspols_df <- dmspols %>%
    coordinates() %>%
    as.data.frame() 
  dmspols_df$value <- dmspols[]
  
  dmspols_df$value <- log(dmspols_df$value + 1)
  
  ## Map
  p <- ggplot() +
    geom_raster(data = dmspols_df ,
                aes(x = x, y = y),
                fill = "black") +
    geom_polygon(data = eth_adm,
                  aes(x = long, y = lat, group = group),
                  color = "white",
                 size = .2) +
    geom_raster(data = dmspols_df[dmspols_df$value > 0,] , 
                aes(x = x, y = y,
                    fill = value)) + 
    scale_fill_gradient2(low = "yellow",
                         mid = "gold",
                         high = "firebrick2",
                         midpoint = 2,
                         limits = c(0, 4.2)) +
    labs(title = year) +
    coord_quickmap() +
    theme_void() +
    theme(legend.position = "none",
          plot.background = element_rect(fill = "black",
                                         color = "black"),
          plot.title = element_text(hjust = 0.5, face = "bold", color = "white"))
  ggsave(p, filename = file.path(data_file_path, "Nighttime Lights", "DMSPOLS",
                                 "Outputs", "figures", "country_level_gif", "pngs",
                                 paste0("dmspols_", year, ".png")),
         height = 6,
         width = 8)
}


