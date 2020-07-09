# Figures of Dependent Variables

library(estimatr)

# Load Data --------------------------------------------------------------------
#data <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "merged_datasets", "grid_data_clean.Rds"))
data <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "merged_datasets", "grid_data.Rds"))

data <- data[data$distance_anyroad2016 < 5000,]
data <- data[data$year %in% 2000,]

data$distance_anyimproved_ever

data$distance_anyimproved_ever_bin <- as.numeric(data$distance_anyimproved_ever < 5000)
data$distance_mst_bin <- as.numeric(data$distance_mst < 5000)
data$woreda_hdx_w_uid <- data$woreda_hdx_w_uid %>% as.numeric()

write.csv(data[,c("distance_anyimproved_ever_bin", "distance_mst_bin", "woreda_hdx_w_uid")], "~/Desktop/eth_data.csv", row.names=F)


lmr_out <- lm_robust(distance_anyimproved_ever_bin ~ distance_mst_bin, data = data, fixed_effects = ~ woreda_hdx_w_uid)

lm1 <- felm(distance_anyimproved_ever_bin ~ distance_mst_bin | woreda_hdx_w_uid | 0 | woreda_hdx_w_uid, data=data)

summary(lm1)

lmr_out <- lm_robust(mpg ~ hp, data = mtcars, fixed_effects = ~ cyl)


data_oneyear$distance_anyroad2016 %>% max()
lm1 <- lm(distance_improvedroad_anytime_bin ~ distance_mst_bin + factor(woreda_hdx_w_uid), data=data_oneyear)
