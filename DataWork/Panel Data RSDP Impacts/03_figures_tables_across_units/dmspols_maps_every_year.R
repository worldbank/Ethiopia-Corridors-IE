# Road Improvement Map

# Load Data --------------------------------------------------------------------
eth_adm <- readRDS(file.path(data_file_path, "GADM", "RawData", "gadm36_ETH_0_sp.rds")) 

dmsp1996 <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS_DMSPOLS_Intercalibrated", "Harmonized_DN_NTL_1992_calDMSP.tif")) %>% crop(eth_adm) %>% mask(eth_adm)
dmsp2013 <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS_DMSPOLS_Intercalibrated", "Harmonized_DN_NTL_2013_calDMSP.tif")) %>% crop(eth_adm) %>% mask(eth_adm)
dmsp2014 <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS_DMSPOLS_Intercalibrated", "Harmonized_DN_NTL_2014_simVIIRS.tif")) %>% crop(eth_adm) %>% mask(eth_adm)
dmsp2018 <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS_DMSPOLS_Intercalibrated", "Harmonized_DN_NTL_2018_simVIIRS.tif")) %>% crop(eth_adm) %>% mask(eth_adm)

# Prep Data for Figure ---------------------------------------------------------
prep_dmsp <- function(dmsp){
  
  dmsp_df <- dmsp %>%
    coordinates() %>%
    as.data.frame() 
  dmsp_df$value <- dmsp[]
  dmsp_df$value_log <- log(dmsp_df$value + 1)
  
  return(dmsp_df)
}

dmsp1996_df <- prep_dmsp(dmsp1996)
dmsp2013_df <- prep_dmsp(dmsp2013)
dmsp2014_df <- prep_dmsp(dmsp2014)
dmsp2018_df <- prep_dmsp(dmsp2018)

# Figures ----------------------------------------------------------------------
p_dmsp1996 <- ggplot() +
  geom_polygon(data = eth_adm,
               aes(x = long, y = lat, group = group),
               fill = NA,
               color = "white",
               size = 0.1) +
  geom_raster(data = dmsp1996_df[dmsp1996_df$value_log > 0,] , 
              aes(x = x, y = y,
                  fill = value_log)) + 
  scale_fill_gradient2(low = "#262300", # "#262300",
                       mid = "#726600", #  "#5B5200", # "#393300", #  "#393300",
                       high = "gold",
                       midpoint = 2, # 2
                       limits = c(0, 4.2)) + # 0, 4.2
  labs(title = "1996 - DMSP-OLS") +
  coord_quickmap() +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "black",
                                       color = "black"),
        plot.title = element_text(hjust = 0.5, color = "white"))

p_dmsp2013 <- ggplot() +
  geom_polygon(data = eth_adm,
               aes(x = long, y = lat, group = group),
               fill = NA,
               color = "white",
               size = 0.1) +
  geom_raster(data = dmsp2013_df[dmsp2013_df$value_log > 0,] , 
              aes(x = x, y = y,
                  fill = value_log)) + 
  scale_fill_gradient2(low = "#262300",
                       mid = "#726600", #  "#5B5200", # "#393300", #  "#393300",
                       high = "gold",
                       midpoint = 2, # 2
                       limits = c(0, 4.2)) + # 0, 4.2
  labs(title = "2013 - DMSP-OLS") +
  coord_quickmap() +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "black",
                                       color = "black"),
        plot.title = element_text(hjust = 0.5, color = "white"))

p_dmsp2014 <- ggplot() +
  geom_polygon(data = eth_adm,
               aes(x = long, y = lat, group = group),
               fill = NA,
               color = "white",
               size = 0.1) +
  geom_raster(data = dmsp2014_df[dmsp2014_df$value_log > 0,] , 
              aes(x = x, y = y,
                  fill = value_log)) + 
  scale_fill_gradient2(low = "black",
                       mid = "#262300", #  "#393300",
                       high = "gold",
                       midpoint = 2, # 2
                       limits = c(0, 4.2)) + # 0, 4.2
  labs(title = "2014 - Simulated DMSP-OLS") +
  coord_quickmap() +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "black",
                                       color = "black"),
        plot.title = element_text(hjust = 0.5, color = "white"))

p_dmsp2018 <- ggplot() +
  geom_polygon(data = eth_adm,
               aes(x = long, y = lat, group = group),
               fill = NA,
               color = "white",
               size = 0.1) +
  geom_raster(data = dmsp2018_df[dmsp2018_df$value_log > 0,] , 
              aes(x = x, y = y,
                  fill = value_log)) + 
  scale_fill_gradient2(low = "black",
                       mid = "#262300", #  "#393300",
                       high = "gold",
                       midpoint = 2, # 2
                       limits = c(0, 4.2)) + # 0, 4.2
  labs(title = "2018 - Simulated DMSP-OLS") +
  coord_quickmap() +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "black",
                                       color = "black"),
        plot.title = element_text(hjust = 0.5, color = "white"))

# Appended Figure --------------------------------------------------------------
p <- ggarrange(p_dmsp1996,
               p_dmsp2013,
               p_dmsp2014,
               p_dmsp2018) + bgcolor("black")

ggsave(p, filename = file.path(paper_figures,
                               "dmspols_multiple_years.png"),
       height = 6,
       width = 7)











# Nighttime Lights -------------------------------------------------------------
prep_dmsp <- function(year){
  if(year <= 2013){
    dmsp <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS_DMSPOLS_Intercalibrated", paste0("Harmonized_DN_NTL_",year,"_calDMSP.tif"))) %>% crop(eth_adm) %>% mask(eth_adm)
  } else{
    dmsp <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS_DMSPOLS_Intercalibrated", paste0("Harmonized_DN_NTL_",year,"_simVIIRS.tif"))) %>% crop(eth_adm) %>% mask(eth_adm)
  }
  
  dmsp_df <- dmsp %>%
    coordinates() %>%
    as.data.frame() 
  dmsp_df$value <- dmsp[]
  dmsp_df$value_log <- log(dmsp_df$value + 1)
  
  return(dmsp_df)
}

years_vec <- seq(from=1996,to=2016,by=4)

poly_allyears <- lapply(years_vec, function(year){
  eth_adm$year <- year
  return(eth_adm)
}) %>% do.call(what = "rbind")

raster_allyears <- lapply(years_vec, function(year){
  df <- prep_dmsp(year)
  df <- df[df$value_log > 0,]
  df$year <- year
  return(df)
}) %>%
  bind_rows()

poly_allyears$id <- row.names(poly_allyears)
poly_allyears_tidy <- tidy(poly_allyears)
poly_allyears_tidy <- merge(poly_allyears_tidy, poly_allyears, by="id")

p_all <- ggplot() +
  #geom_raster(data = df,
  #            aes(x = x, y = y),
  #            fill = "black") +
  geom_polygon(data = poly_allyears_tidy,
               aes(x = long, y = lat, group = group),
               color = "white", fill = "black",
               size = .15) +
  geom_raster(data = raster_allyears[raster_allyears$value_log > 0,] , 
              aes(x = x, y = y,
                  fill = value_log)) + 
  scale_fill_gradient2(low = "yellow",
                       mid = "gold",
                       high = "darkorange1",
                       midpoint = 2, # 2
                       limits = c(0, 4.2)) +
  #labs(title = year) +
  coord_quickmap() +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "black",
                                       color = "black"),
        plot.title = element_text(hjust = 0.5, color = "white")) +
  facet_wrap(~year, ncol = 2)




ggsave(p_all, filename = file.path(paper_figures,
                                   "dmspols_even_years.png"),
       height = 12,
       width = 8)









