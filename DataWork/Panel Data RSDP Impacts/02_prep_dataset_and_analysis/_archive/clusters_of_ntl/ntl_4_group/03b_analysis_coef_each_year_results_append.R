# Append Individual Results

file.path(panel_rsdp_imp_data_file_path, 
          "clusters_of_ntl", "results_datasets", 
          "individual_datasets4") %>%
  list.files(full.names = T,
             pattern = "*.Rds") %>%
  lapply(readRDS) %>%
  bind_rows() %>%
  saveRDS(file.path(panel_rsdp_imp_data_file_path, "clusters_of_ntl", "results_datasets",
                    "did_coef_every_year4.Rds"))

