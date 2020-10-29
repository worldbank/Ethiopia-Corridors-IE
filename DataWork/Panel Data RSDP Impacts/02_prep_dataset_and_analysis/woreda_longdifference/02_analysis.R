# Analysis

data <- readRDS(file.path(panel_rsdp_imp_data_file_path, "woreda", "merged_datasets",
                          "longdiff_data_clean_base1996_end2012.Rds"))

summary(data$road_length_10above)
data$bin <- as.numeric(data$road_length_70above > 10)

lm(dmspols_ihs ~ bin, data = data) %>% summary()




