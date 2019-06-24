# Impact of Expressway Expansion
# Ethiopia IE

dataset <- "points_5percent"
HEIGHT <- 6
WIDTH <- 14
p_dodge_width <- 1

# Load and Prep Data -----------------------------------------------------------
results_all <- readRDS(file.path(finaldata_file_path, "lead_lag_results_coefficients", paste0("lead_lag_results.Rds_",dataset)))

results_all$FE[results_all$FE == "year"] <- "Year"
results_all$FE[results_all$FE == "cell_idyear"] <- "Cell ID & Year"
results_all$FE[results_all$FE == "GADM_ID_3year"] <- "Woreda & Year"

results_all$DV_full <- ""
results_all$DV_full[results_all$DV == "dmspols"] <- "DMSP-OLS (Raw)"
results_all$DV_full[results_all$DV == "dmspols_zhang"] <- "DMSP-OLS (Intercalibrated)"
results_all$DV_full[results_all$DV == "dmspols_1"] <- "DMSP-OLS > 0 (Raw)"
results_all$DV_full[results_all$DV == "dmspols_5"] <- "DMSP-OLS > 5 (Raw)"
results_all$DV_full[results_all$DV == "dmspols_zhang_1"] <- "DMSP-OLS > 0 (Intercalibrated)"
results_all$DV_full[results_all$DV == "dmspols_zhang_5"] <- "DMSP-OLS > 5 (Intercalibrated)"
results_all$DV_full[results_all$DV == "ndvi"] <- "NDVI"
results_all$DV_full[results_all$DV == "ndvi_cropland"] <- "NDVI:  Cropland"
results_all$DV_full[results_all$DV == "globcover_urban"] <- "Globcover: Urban"
results_all$DV_full[results_all$DV == "globcover_cropland"] <- "Globcover: Cropland"

results_all$ntl_base_full <- ""
results_all$ntl_base_full[results_all$dmspols_1997_bin == 1] <- "Zero"
results_all$ntl_base_full[results_all$dmspols_1997_bin == 2] <- "Low"
results_all$ntl_base_full[results_all$dmspols_1997_bin == 3] <- "High"
results_all$ntl_base_full[results_all$dmspols_1997_bin == 123] <- "All"

# Figures ----------------------------------------------------------------------
DV <- "dmspols"
improved_road <- "All" # 50above, All, below50
ntl_base <- 1
constant_sample <- FALSE

for(DV in c("dmspols", "dmspols_zhang",
            "dmspols_zhang_1",
            "ndvi", "ndvi_cropland",
            "globcover_urban", "globcover_cropland")){
  for(improved_road in "All"){
    for(ntl_base in c(1,2,3,123)){
      for(constant_sample in FALSE){
      
        results_all_i <- results_all[results_all$DV %in% DV,]
        results_all_i <- results_all_i[results_all_i$improved_road %in% improved_road,]
        results_all_i <- results_all_i[results_all_i$dmspols_1997_bin %in% ntl_base,]
        
        fig <- ggplot(data = results_all_i,
               aes(x = years_since_improved, 
                   y = b, ymin = p025, ymax = p975,
                   group = FE, color = FE)) +
          geom_vline(xintercept=0,size=3,alpha=0.15) +
          geom_point(position = position_dodge(width = p_dodge_width),size=3) + 
          geom_linerange(position = position_dodge(width = p_dodge_width),size=1.25) +
          labs(x="Years Since Improved Road Constructed",
               y="Coefficient",
               color = "Fixed\nEffects",
               title = paste0(results_all_i$DV_full,
                              "\nNTL Baseline: ",results_all_i$ntl_base_full)) +
          scale_x_continuous(breaks = seq(-30, 30, by = 2)) +
          theme_minimal() +
          theme(axis.text = element_text(size=14, color="black"),
                axis.title = element_text(size=15, color="black"),
                legend.text = element_text(size=14),
                legend.title = element_text(size=14),
                plot.title = element_text(hjust = 0.5, face="bold")) 
        
        ggsave(fig, filename = file.path(figures_file_path, "lead_lag_coefs", paste0("leadlad_DV",DV,
                                                                                     "_improved_road",improved_road,
                                                                                     "_ntl_base",ntl_base,
                                                                                     "_constant",constant_sample,
                                                                                     ".png")), height = HEIGHT, width = WIDTH)
        
        
        
      }
    }
  }
}







