# Stock of Roads Improved by Phase

# Load Data --------------------------------------------------------------------
## RSDP Roads
roads <- readRDS(file.path(data_file_path, "RSDP Roads", "FinalData", "RoadNetworkPanelData_1996_2016.Rds"))
roads <- spTransform(roads, CRS(UTM_ETH))

## Province Data
eth <- readOGR(dsn = file.path(data_file_path, "Woreda Boundaries - 2013", "RawData"),
               layer = "Eth_Woreda_2013")
eth <- spTransform(eth, CRS(UTM_ETH))
eth <- gBuffer(eth, width = 0, byid = T)
province_sdf <- raster::aggregate(eth, by="REGIONNAME")

# Process Data -----------------------------------------------------------------
# Dataframe that shows, for each province, total road length and length
# improved in each phase

results_df <- lapply(province_sdf$REGIONNAME, function(region){
  
  print(region)
  
  ## Polygon of one region
  province_sdf_i <- province_sdf[province_sdf$REGIONNAME %in% region,]
  
  ## Restrict roads to that region
  roads_i <- raster::intersect(roads, province_sdf_i)
  
  out <- data.frame(
    province = region,
    rd_length = gLength(roads_i) / 1000,
    improved_phase_1 = gLength(roads_i[roads_i$rsdp_phase %in% 1,]) / 1000,
    improved_phase_2 = gLength(roads_i[roads_i$rsdp_phase %in% 2,]) / 1000,
    improved_phase_3 = gLength(roads_i[roads_i$rsdp_phase %in% 3,]) / 1000,
    improved_phase_4 = gLength(roads_i[roads_i$rsdp_phase %in% 4,]) / 1000
  )
  
  return(out)
}) %>%
  bind_rows()

## Proportion improved in each phase
results_df <- results_df %>%
  mutate(prop_improved_phase_1 = improved_phase_1 / rd_length,
         prop_improved_phase_2 = improved_phase_2 / rd_length,
         prop_improved_phase_3 = improved_phase_3 / rd_length,
         prop_improved_phase_4 = improved_phase_4 / rd_length)

## Long to Wide
results_long_df <- results_df %>%
  dplyr::select(province, 
                improved_phase_1, 
                improved_phase_2,
                improved_phase_3,
                improved_phase_4,
                prop_improved_phase_1, 
                prop_improved_phase_2,
                prop_improved_phase_3,
                prop_improved_phase_4) %>%
  pivot_longer(-province) %>%
  mutate(phase = name %>%
           str_replace_all("prop_improved_|improved_", "") %>%
           str_replace_all("_", "") %>%
           tools::toTitleCase()) %>%
  mutate(proportion = str_detect(name, "prop_")) %>%
  mutate(prop_type = case_when(
    proportion == T ~ "Proportion of Road\nNetwork Improved",
    proportion == F ~ "Kilometers of\nRoads Improved"
  ))


# Figure -----------------------------------------------------------------------
# scales_x <- list(
#   `Proportion of Road\nNetwork Improved` = scale_x_continuous(limits = c(0, 1)),
#   `Kilometers of\nRoads Improved` = scale_x_continuous(limits = c(0, 
#                                                                   max(results_long_df$value)))
# )

results_long_df %>%
  ggplot() +
  geom_col(aes(x = province, y = value, group = phase, fill = phase),
           position = position_stack(reverse = TRUE),
           color = "black", size = 0.1) +
  coord_flip() +
  labs(x = NULL,
       y = NULL,
       fill = NULL,
       title = NULL) +
  #scale_y_continuous(limits = c(0, 1)) +
  scale_y_continuous(labels = comma) +
  scale_x_discrete(limits = rev) +
  theme_minimal() +
  theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(face = "bold", color = "black", size = 12),
        strip.text = element_text(face = "bold", size = 13, hjust = 0),
        panel.spacing = unit(2, "lines"),
        legend.position = "bottom") +
  scale_fill_manual(values = wes_palette("Zissou1", n = 4, type = "continuous") %>% rev(),
                    labels = c("Phase I", "Phase II", "Phase III", "Phase IV")) +
  #facet_grid_sc(rows = vars(drv), scales = list(x = scales_x)) +
  facet_wrap(~prop_type,
             scales = "free_x") +
  ggsave(filename = file.path(paper_figures, "N_prop_improved_phase_province.png"),
         height = 4.5, width = 5.6)





