# Create Minimal Spanning Tree

mst_lc <- file.path(data_file_path, "RSDP Phase I-IV Roads and Targetted Locations", "FinalData", 
                    "mst_by_region") %>%
  list.files(pattern = "*.Rds", full.names = T) %>%
  str_subset("rsdpi_iv_targetted_loc_leastcost_mst_") %>%
  lapply(readRDS) %>%
  do.call(what = "rbind")

mst_ed <- file.path(data_file_path, "RSDP Phase I-IV Roads and Targetted Locations", "FinalData", 
                    "mst_by_region") %>%
  list.files(pattern = "*.Rds", full.names = T) %>%
  str_subset("rsdpi_iv_targetted_loc_eucdist_mst_") %>%
  lapply(readRDS) %>%
  do.call(what = "rbind")

saveRDS(mst_lc, file.path(data_file_path, "RSDP Phase I-IV Roads and Targetted Locations", "FinalData",
                          "rsdpi_iv_targetted_loc_leastcost_mst_region_appended.Rds"))

saveRDS(mst_ed, file.path(data_file_path, "RSDP Phase I-IV Roads and Targetted Locations", "FinalData",
                          "rsdpi_iv_targetted_loc_eucdist_mst_region_appended.Rds"))


