# Analysis

# EXPLORING STUFF

data <- readRDS(file.path(panel_rsdp_imp_data_file_path, "woreda", "merged_datasets", "panel_data_clean.Rds"))

#### PLOTS
data_sum <- data %>%
  group_by(year) %>%
  summarise_if(is.numeric, sum, na.rm=T) 

data_sum %>%
  ggplot() +
  geom_line(aes(x = year, y=road_length_30above, color="30")) +
  geom_line(aes(x = year, y=road_length_45above, color="45")) +
  geom_line(aes(x = year, y=road_length_50above, color="50")) +
  geom_line(aes(x = year, y=road_length_70above, color="70")) 

data_sum %>%
  filter(year <= 2012) %>%
  ggplot() +
  geom_line(aes(x = year, y=dmspols, color="DMSP-OLS")) +
  geom_line(aes(x = year, y=dmspols_zhang, color="DMSP-OLS, Zhang"))

#### FIRST DIFF
# https://stackoverflow.com/questions/48211235/first-difference-data-frame
data_1diff <- data %>%
  arrange(year) %>%
  mutate(year = year %>% as.character()) %>%
  group_by(cell_id) %>%
  mutate_if(is.numeric, list(~ .x - lag(.x))) %>%
  mutate(year = year %>% as.numeric()) %>%
  arrange(year)

lm(dmspols ~ road_length_70above + lag(road_length_70above) + lag(road_length_70above, 2), data = data_1diff) %>%
  summary()

lm(dmspols ~ road_length_70above + lag(road_length_70above) + lag(road_length_70above, 2), data = data) %>%
  summary()


