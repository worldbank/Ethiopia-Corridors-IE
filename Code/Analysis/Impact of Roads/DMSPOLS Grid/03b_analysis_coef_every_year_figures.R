# Impact of Expressway Expansion
# Ethiopia IE

dataset <- "points_5percent"
HEIGHT <- 12
WIDTH <- 30
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

results_all$gadm_id_1_name <- ""
results_all$gadm_id_1_name[results_all$gadm_id_1 %in% "1"] <- "Addis Ababa"
results_all$gadm_id_1_name[results_all$gadm_id_1 %in% "2"] <- "Southern Nations"
results_all$gadm_id_1_name[results_all$gadm_id_1 %in% "3"] <- "Tigray"
results_all$gadm_id_1_name[results_all$gadm_id_1 %in% "4"] <- "Afar"
results_all$gadm_id_1_name[results_all$gadm_id_1 %in% "5"] <- "Amhara"
results_all$gadm_id_1_name[results_all$gadm_id_1 %in% "6"] <- "Benshangul-Gumaz"
results_all$gadm_id_1_name[results_all$gadm_id_1 %in% "7"] <- "Dire Dawa"
results_all$gadm_id_1_name[results_all$gadm_id_1 %in% "8"] <- "Gambela Peoples"
results_all$gadm_id_1_name[results_all$gadm_id_1 %in% "9"] <- "Harari People"
results_all$gadm_id_1_name[results_all$gadm_id_1 %in% "10"] <- "Oromia"
results_all$gadm_id_1_name[results_all$gadm_id_1 %in% "11"] <- "Somali"
results_all$gadm_id_1_name[results_all$gadm_id_1 %in% "all"] <- "All"

results_all$gadm_group_city_region <- ""
results_all$gadm_group_city_region[results_all$gadm_id_1_name %in% c("Addis Ababa",
                                                                     "Dire Dawa",
                                                                     "Harari People")] <- "City"
results_all$gadm_group_city_region[results_all$gadm_id_1_name %in% c("Afar",
                                                                     "Amhara",
                                                                     "Benshangul-Gumaz",
                                                                     "Gambela Peoples",
                                                                     "Oromia",
                                                                     "Somali",
                                                                     "Southern Nations",
                                                                     "Tigray")] <- "By Region"
results_all$gadm_group_city_region[results_all$gadm_id_1_name %in% "All"] <- "All"

# TESTING ----------------------------------------------------------------------
if(F){
DV <- "globcover_cropland"
improved_road <- "All" # 50above, All, below50
ntl_base <- 123
constant_sample <- FALSE
FE <- "Cell ID & Year"

results_all_i <- results_all[results_all$DV %in% DV,]
results_all_i <- results_all_i[results_all_i$improved_road %in% improved_road,]
results_all_i <- results_all_i[results_all_i$dmspols_1997_bin %in% ntl_base,]
results_all_i <- results_all_i[results_all_i$FE %in% FE,]
#results_all_i <- results_all_i[!(results_all_i$gadm_id_1 %in% c("5","7","1")),]

fig <- ggplot(data = results_all_i,
       aes(x = years_since_improved, 
           y = b, ymin = p025, ymax = p975,
           group = gadm_id_1_name, color = gadm_id_1_name)) +
  geom_vline(xintercept=0,size=3,alpha=0.15) +
  geom_point(position = position_dodge(width = p_dodge_width),size=3) + 
  geom_linerange(position = position_dodge(width = p_dodge_width),size=1.25)  +
  facet_wrap( ~ gadm_group_city_region)
ggsave(fig, filename = file.path("~/Desktop/test.png"), height=10, width=30)
}

# Figures ----------------------------------------------------------------------
DV <- "globcover_urban"
improved_road <- "All" # 50above, All, below50
ntl_base <- 123
constant_sample <- FALSE
FE <- "Cell ID & Year"

for(DV in c("dmspols", "dmspols_zhang",
            "dmspols_zhang_1",
            "ndvi", "ndvi_cropland",
            "globcover_urban", "globcover_cropland")){
  for(improved_road in "All"){
    for(ntl_base in c(1,2,3,123)){
      for(constant_sample in FALSE){
        for(FE in c("Cell ID & Year", "Woreda & Year", "Year")){
      
          results_all_i <- results_all[results_all$DV %in% DV,]
          results_all_i <- results_all_i[results_all_i$improved_road %in% improved_road,]
          results_all_i <- results_all_i[results_all_i$dmspols_1997_bin %in% ntl_base,]
          results_all_i <- results_all_i[results_all_i$FE %in% FE,]
          results_all_i <- results_all_i[results_all_i$gadm_group_city_region != "City",]
          results_all_i <- results_all_i[results_all_i$gadm_id_1_name != "Amhara",]
          
          fig <- ggplot(data = results_all_i,
                        aes(x = years_since_improved, 
                            y = b, ymin = p025, ymax = p975,
                            group = gadm_id_1_name, color = gadm_id_1_name)) +
            geom_vline(xintercept=0,size=3,alpha=0.15) +
            geom_point(position = position_dodge(width = p_dodge_width),size=3) + 
            geom_linerange(position = position_dodge(width = p_dodge_width),size=1.25) +
            labs(x="Years Since Improved Road Constructed",
                 y="Coefficient",
                 color = "Region",
                 title = paste0(results_all_i$DV_full,
                                "\nNTL Baseline: ",results_all_i$ntl_base_full)) +
            scale_x_continuous(breaks = seq(-30, 30, by = 2)) +
            theme_minimal() +
            theme(axis.text = element_text(size=14, color="black"),
                  axis.title = element_text(size=15, color="black"),
                  legend.text = element_text(size=14),
                  legend.title = element_text(size=14),
                  plot.title = element_text(hjust = 0.5, face="bold"))  +
            facet_wrap( ~ gadm_group_city_region, nrow=2, scales="free") 
      
          #ggsave(fig, filename = file.path("~/Desktop/test.png"), height=12, width=30)
          
          FE_name <- FE %>% str_replace_all("[[:punct:]]","") %>% str_replace_all(" ","") %>% tolower
          
          ggsave(fig, filename = file.path(figures_file_path, "lead_lag_coefs", paste0("leadlad_DV",DV,
                                                                                       "_improved_road",improved_road,
                                                                                       "_ntl_base",ntl_base,
                                                                                       "_FE",FE_name,
                                                                                       "_constant",constant_sample,
                                                                                       ".png")), height = HEIGHT, width = WIDTH, dpi=80)
          
          
          
        }
      }
    }
  }
}







