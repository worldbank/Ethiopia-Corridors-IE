# Figures of Dependent Variables

# Load Data --------------------------------------------------------------------
data <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "merged_datasets", "grid_data_clean.Rds"))

data <- data %>%
  group_by(year) %>%
  summarise(dmspols_zhang_ihs = mean(dmspols_zhang_ihs, na.rm=T),
            dmspols_zhang_6 = mean(dmspols_zhang_6, na.rm=T),
            globcover_cropland = mean(globcover_cropland, na.rm=T),
            globcover_urban = mean(globcover_urban, na.rm=T)) %>%
  ungroup()

ggplot() +
  geom_line(data=data, aes(x=year, y=globcover_cropland))
