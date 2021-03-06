# Create Minimal Spanning Tree

# Load Data --------------------------------------------------------------------
mst_cost <- readRDS(file.path(data_file_path, "RSDP Phase I-III Roads and Targetted Locations", "FinalData", 
                            "rsdpi_iii_targetted_loc_leastcost_mst.Rds"))

mst_dist <- readRDS(file.path(data_file_path, "RSDP Phase I-III Roads and Targetted Locations", "FinalData", 
                             "rsdpi_iii_targetted_loc_eucdist_mst.Rds"))

mst_cost_regions <- readRDS(file.path(data_file_path, "RSDP Phase I-III Roads and Targetted Locations", "FinalData", 
                              "rsdpi_iii_targetted_loc_leastcost_mst_region_appended.Rds"))

mst_dist_regions <- readRDS(file.path(data_file_path, "RSDP Phase I-III Roads and Targetted Locations", "FinalData", 
                              "rsdpi_iii_targetted_loc_eucdist_mst_region_appended.Rds"))

mst_dist         <- spTransform(mst_dist, CRS("+init=epsg:4326"))
mst_dist_regions <- spTransform(mst_dist_regions, CRS("+init=epsg:4326"))

roads <- readRDS(file.path(data_file_path, "RSDP Roads", "FinalData", "RoadNetworkPanelData_1996_2016.Rds"))
improved_roads <- roads[roads$Complete_G %in% 1997:2009,]

eth <- readRDS(file.path(data_file_path, "GADM", "RawData", "gadm36_ETH_0_sp.rds")) 

# Individual Maps --------------------------------------------------------------
lc_mst <- ggplot() +
  geom_polygon(data = eth,
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = NA, alpha = 0.9) +
  geom_path(data = improved_roads,
            aes(x = long, y = lat, group = group,
                color = "Improved Roads"),
            size = 0.9) +
  geom_path(data = mst_cost,
            aes(x = long, y = lat, group = group,
                color = "MST"),
            size = 0.35) +
  scale_color_manual(values = c("dodgerblue1", "darkorange")) +
  labs(color = NULL,
       title = "Least Cost MST") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size=11, face = "bold")) + 
  coord_quickmap()

ld_mst <- ggplot() +
  geom_polygon(data = eth,
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = NA, alpha = 0.9) + # cornsilk1
  geom_path(data = improved_roads,
            aes(x = long, y = lat, group = group,
                color = "Improved Roads"),
            size = 0.9) +
  geom_path(data = mst_dist,
            aes(x = long, y = lat, group = group,
                color = "MST"),
            size = 0.35) +
  scale_color_manual(values = c("dodgerblue1", "darkorange")) +
  labs(color = NULL,
       title = "Minimum Distance MST") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size=11, face = "bold")) + 
  coord_quickmap()

lc_mst_regions <- ggplot() +
  geom_polygon(data = eth,
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = NA, alpha = 0.9) +
  geom_path(data = improved_roads,
            aes(x = long, y = lat, group = group,
                color = "Improved Roads"),
            size = 0.9) +
  geom_path(data = mst_cost_regions,
            aes(x = long, y = lat, group = group,
                color = "MST"),
            size = 0.35) +
  scale_color_manual(values = c("dodgerblue1", "red")) +
  labs(color = NULL,
       title = "Least Cost MST") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size=11, face = "bold")) + 
  coord_quickmap()

ld_mst_regions <- ggplot() +
  geom_polygon(data = eth,
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = NA, alpha = 0.9) + # cornsilk1
  geom_path(data = improved_roads,
            aes(x = long, y = lat, group = group,
                color = "Improved Roads"),
            size = 0.9) +
  geom_path(data = mst_dist_regions,
            aes(x = long, y = lat, group = group,
                color = "MST"),
            size = 0.35) +
  scale_color_manual(values = c("dodgerblue1", "red")) +
  labs(color = NULL,
       title = "Minimum Distance MST") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size=11, face = "bold")) + 
  coord_quickmap()

fig <- ggarrange(ld_mst, lc_mst, common.legend = T, legend = "right")
fig <- ggarrange(ld_mst, lc_mst, common.legend = T, legend = "right")

ggsave(fig, filename = file.path(paper_figures, "mst_maps_rsdp123.png"),
       height = 2.5, width = 7)
