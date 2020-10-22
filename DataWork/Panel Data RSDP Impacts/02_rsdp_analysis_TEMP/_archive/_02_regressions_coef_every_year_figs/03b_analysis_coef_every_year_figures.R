# Impact of Expressway Expansion
# Ethiopia IE

dataset <- "points_5percent"
HEIGHT <- 15
WIDTH <- 11
p_dodge_width <- .5

# Load and Prep Data -----------------------------------------------------------
results_all <- readRDS(file.path(finaldata_file_path, "lead_lag_results_coefficients", paste0("lead_lag_results.Rds_",dataset)))

results_all$FE[results_all$FE == "year"] <- "Year"
results_all$FE[results_all$FE == "cell_idyear"] <- "Cell ID & Year"
results_all$FE[results_all$FE == "GADM_ID_3year"] <- "Woreda & Year"

results_all$DV_full <- ""
results_all$DV_full[results_all$DV == "dmspols_log"] <- "DMSP-OLS (Log)"
results_all$DV_full[results_all$DV == "dmspols"] <- "DMSP-OLS"
results_all$DV_full[results_all$DV == "dmspols_zhang"] <- "DMSP-OLS (Intercalibrated)"
results_all$DV_full[results_all$DV == "dmspols_1"] <- "DMSP-OLS > 0"
results_all$DV_full[results_all$DV == "dmspols_5"] <- "DMSP-OLS > 5"
results_all$DV_full[results_all$DV == "dmspols_zhang_1"] <- "DMSP-OLS > 0 (Intercalibrated)"
results_all$DV_full[results_all$DV == "dmspols_zhang_5"] <- "DMSP-OLS > 5 (Intercalibrated)"
results_all$DV_full[results_all$DV == "ndvi"] <- "NDVI"
results_all$DV_full[results_all$DV == "ndvi_cropland"] <- "NDVI:  Cropland"
results_all$DV_full[results_all$DV == "globcover_urban"] <- "Globcover: Urban"
results_all$DV_full[results_all$DV == "globcover_cropland"] <- "Globcover: Cropland"

results_all$ntl_base_full <- ""
results_all$ntl_base_full[results_all$dmspols_1997_bin == 1] <- "Baseline Nighttime Lights: Zero"
results_all$ntl_base_full[results_all$dmspols_1997_bin == 2] <- "Baseline Nighttime Lights: Low"
results_all$ntl_base_full[results_all$dmspols_1997_bin == 3] <- "Baseline Nighttime Lights: High"
results_all$ntl_base_full[results_all$dmspols_1997_bin == 123] <- "Full Sample"
results_all$ntl_base_full <- results_all$ntl_base_full %>% factor(levels = c("Full Sample",
                                                                             "Baseline Nighttime Lights: Zero",
                                                                             "Baseline Nighttime Lights: Low",
                                                                             "Baseline Nighttime Lights: High"))

results_all$region_type[results_all$region_type %in% "Dense;Sparse"] <- "All"

results_all$improved_road[results_all$improved_road %in% "50above"] <- "Roads 50km/hr and Above"
results_all$improved_road[results_all$improved_road %in% "below50"] <- "Roads Below 50km/hr"

results_all$years_since_improved <- results_all$variable %>% 
  str_replace_all("years_since_improved_all", "") %>%
  str_replace_all("years_since_improved_50above", "") %>%
  str_replace_all("years_since_improved_below50","") %>%
  as.numeric

# Figures ----------------------------------------------------------------------
DV <- "globcover_urban"
improved_road <- "All" # 50above, All, below50
ntl_base <- 123
FE <- "Cell ID & Year"
region_type <- "All"

for(DV in c("dmspols", "dmspols_zhang", "dmspols_log",
        "dmspols_zhang_1",
        "ndvi", "ndvi_cropland",
        "globcover_urban", "globcover_cropland")){
  for(FE in c("Cell ID & Year", "Woreda & Year", "Year")){
    print(paste(DV, improved_road, region_type, FE))

    results_all_i <- results_all[results_all$DV %in% DV,]
    #results_all_i <- results_all_i[results_all_i$improved_road %in% improved_road,]
    #results_all_i <- results_all_i[results_all_i$dmspols_1997_bin %in% ntl_base,]
    results_all_i <- results_all_i[results_all_i$FE %in% FE,]
    #results_all_i <- results_all_i[results_all_i$region_type %in% region_type,]
    
    FE_name <- FE %>% str_replace_all("[[:punct:]]","") %>% str_replace_all(" ","") %>% tolower
    
    # ALL ROADS ------------------------------------------------------------
    fig <- ggplot(data = results_all_i[results_all_i$improved_road == "All",],
                  aes(x = years_since_improved, 
                      y = b, ymin = p025, ymax = p975,
                      group = region_type, color = region_type)) +
      geom_vline(xintercept=0,size=3,alpha=0.15) +
      geom_point(position = position_dodge(width = p_dodge_width),size=3) + 
      geom_linerange(position = position_dodge(width = p_dodge_width),size=1.25) +
      labs(x="Years Since Improved Road Constructed",
           y="Coefficient",
           color = "Region",
           title = paste0(results_all_i$DV_full)) +
      scale_x_continuous(breaks = seq(-30, 30, by = 2)) +
      theme_minimal() +
      theme(axis.text = element_text(size=14, color="black"),
            axis.title = element_text(size=15, color="black"),
            legend.text = element_text(size=14),
            legend.title = element_text(size=14),
            plot.title = element_text(hjust = 0.5, face="bold"),
            strip.text = element_text(size=14))  +
      facet_wrap( ~ ntl_base_full, nrow=4, scales="free") 

    ggsave(fig, filename = file.path(figures_file_path, "lead_lag_coefs", paste0("leadlad_DV",DV,
                                                                                 "_improved_road","ALL",
                                                                                 #"_ntl_base",ntl_base,
                                                                                 "_FE",FE_name,
                                                                                 ".png")), height = HEIGHT, width = WIDTH, dpi=80)
    
    # 50 Above/Below Roads -------------------------------------------------
    fig <- ggplot(data = results_all_i[results_all_i$improved_road != "All",],
                  aes(x = years_since_improved, 
                      y = b, ymin = p025, ymax = p975,
                      group = region_type, color = region_type)) +
      geom_vline(xintercept=0,size=3,alpha=0.15) +
      geom_point(position = position_dodge(width = p_dodge_width),size=3) + 
      geom_linerange(position = position_dodge(width = p_dodge_width),size=1.25) +
      labs(x="Years Since Improved Road Constructed",
           y="Coefficient",
           color = "Region",
           title = paste0(results_all_i$DV_full)) +
      scale_x_continuous(breaks = seq(-30, 30, by = 2)) +
      theme_minimal() +
      theme(axis.text = element_text(size=14, color="black"),
            axis.title = element_text(size=15, color="black"),
            legend.text = element_text(size=14),
            legend.title = element_text(size=14),
            plot.title = element_text(hjust = 0.5, face="bold"),
            strip.text = element_text(size=14))  +
      facet_wrap( ~ ntl_base_full + improved_road, nrow=4, scales="free") 
    
    ggsave(fig, filename = file.path(figures_file_path, "lead_lag_coefs", paste0("leadlad_DV",DV,
                                                                                 "_improved_road","belowabove50",
                                                                                 #"_ntl_base",ntl_base,
                                                                                 "_FE",FE_name,
                                                                                 ".png")), height = HEIGHT, width = 16, dpi=80)
    
    
    
  }
}










