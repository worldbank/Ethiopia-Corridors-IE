# Woreda Summary Trends

# Load Data --------------------------------------------------------------------
unit <- "woreda" # "clusters_of_ntl", "clusters_of_globcover_urban"
data <- readRDS(file.path(panel_rsdp_imp_data_file_path, unit, "merged_datasets", "panel_data_clean.Rds"))

data <- data %>%
  group_by(cell_id) %>%
  mutate(dmspols_zhang_sum2_1996 = dmspols_zhang_sum2[year == 1996]) %>%
  ungroup()

data_sum <- data %>%
  mutate(bin = (dmspols_zhang_sum2_1996 > 0)) %>%
  group_by(bin, year) %>%
  dplyr::summarize(dmspols_zhang = mean(dmspols_zhang),
                   dmspols_zhang_2_sum = mean(dmspols_zhang_sum2, na.rm=T),
                   dmspols_zhang_6_sum = mean(dmspols_zhang_sum6, na.rm=T),
                   globcover_urban_sum = mean(globcover_urban_sum, na.rm=T)) %>%
  pivot_longer(cols = -c(bin, year)) %>%
  #dplyr::filter(year <= 2012) %>%
  dplyr::filter(!is.na(value))

data_sum$name[data_sum$name %in% "dmspols_zhang_2_sum"] <- "NTL > 2"
data_sum$name[data_sum$name %in% "dmspols_zhang_6_sum"] <- "NTL > 6"
data_sum$name[data_sum$name %in% "dmspols_zhang"] <- "NTL"
data_sum$name[data_sum$name %in% "globcover_urban_sum"] <- "Urban (Globcover)"

N_lit <- data %>%
  filter(dmspols_zhang_sum2_1996 > 0,
         year == 1996) %>%
  nrow()

N_unlit <- data %>%
  filter(dmspols_zhang_sum2_1996 %in% 0,
         year == 1996) %>%
  nrow()

p_lit <- data_sum %>%
  filter(bin %in% 1) %>%
  ggplot() +
  geom_line(aes(x = year, 
                y = value),
            size = 1,
            color = "dodgerblue4") +
  labs(x = NULL,
       y = NULL,
       title = paste0("Woredas Lit at Baseline [N = ",N_lit,"]")) +
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold", size = 11, hjust = 0.5)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 4)) +
  facet_wrap(~name,
             scales = "free_y") 

p_unlit <- data_sum %>%
  filter(bin %in% 0) %>%
  ggplot() +
  geom_line(aes(x = year, 
                y = value),
            size = 1,
            color = "darkorange1") +
  labs(x = NULL,
       y = NULL,
       title = paste0("Woredas Unlit at Baseline [N = ",N_unlit,"]")) +
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold", size = 11, hjust = 0.5)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 4)) +
  facet_wrap(~name,
             scales = "free_y") 

p <- ggarrange(p_lit, p_unlit,
               ncol = 2) %>%
  annotate_figure(top = text_grob("Outcome variables over time, by baseline levels of nighttime lights", color = "black", face = "bold", size = 14))

ggsave(p, filename = file.path(paper_figures, "ntl_trends_by_baseline.png"), height = 4, width = 8.5)
