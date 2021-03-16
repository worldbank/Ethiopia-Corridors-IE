# Road Improvement Map

# Load Data --------------------------------------------------------------------
dmsp1996 <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS_DMSPOLS_Intercalibrated", "Harmonized_DN_NTL_1992_calDMSP.tif")) %>% crop(eth_adm) %>% mask(eth_adm)
dmsp2013 <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS_DMSPOLS_Intercalibrated", "Harmonized_DN_NTL_2013_calDMSP.tif")) %>% crop(eth_adm) %>% mask(eth_adm)
dmsp2014 <- raster(file.path(data_file_path, "Nighttime Lights", "VIIRS_DMSPOLS_Intercalibrated", "Harmonized_DN_NTL_2014_simVIIRS.tif")) %>% crop(eth_adm) %>% mask(eth_adm)

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









