# Impact of RSDP

# Load Data --------------------------------------------------------------------
results_all <- readRDS(file.path(finaldata_file_path, "lead_lag_results_coefficients", "results_by_phase_5percent.Rds"))

results_all$years_since_improved <- results_all$variable %>% str_replace_all("years_since_improved", "") %>% as.numeric
results_all$coef_lb <- results_all$Estimate - results_all$`Cluster s.e.`*1.96
results_all$coef_ub <- results_all$Estimate + results_all$`Cluster s.e.`*1.96

results_all$group <- paste(results_all$road_variable, results_all$phase_cat)

results_all$road_variable[results_all$road_variable == "50above"] <- "50 km/hr and Above"
results_all$road_variable[results_all$road_variable == "below50"] <- "Below 50 km/hr"
results_all$road_variable[results_all$road_variable == "all"] <- "All Roads"

results_all$phase_cat[results_all$phase_cat == "phase_12"] <- "Phase 1 and 2"
results_all$phase_cat[results_all$phase_cat == "phase_34"] <- "Phase 3 and 4"
results_all$phase_cat[results_all$phase_cat == "phase_all"] <- "All Phases"

results_all$dep_var_clean <- ""
results_all$dep_var_clean[results_all$dep_var == "dmspols_zhang"] <- "NTL"
results_all$dep_var_clean[results_all$dep_var == "dmspols_zhang_1"] <- "NTL > 0"
results_all$dep_var_clean[results_all$dep_var == "dmspols_zhang_5"] <- "NTL > 5"
results_all$dep_var_clean[results_all$dep_var == "globcover_cropland"] <- "Cropland"
results_all$dep_var_clean[results_all$dep_var == "globcover_urban"] <- "Urban"
results_all$dep_var_clean[results_all$dep_var == "ndvi"] <- "NDVI"
results_all$dep_var_clean[results_all$dep_var == "ndvi_cropland"] <- "NDVI (Cropland Areas)"

for(dep_var in unique(results_all$dep_var)){
  
  print(dep_var)
  
  dep_var_clean <- results_all$dep_var_clean[results_all$dep_var == dep_var][1]
  
  p <- ggplot(data=results_all[results_all$dep_var == dep_var,], 
         aes(x=years_since_improved, y=Estimate, 
             ymin = coef_lb, ymax = coef_ub,
             group=road_variable, color=road_variable)) +
    facet_wrap(~ phase_cat, nrow = 1, scales="free") +
    geom_vline(xintercept=0, linetype="solid", color = "gray70") +
    geom_linerange(size=.5,
                   position = position_dodge(width=.6)) +
    geom_point(size=1.5,
               position = position_dodge(width=.6)) +
    theme_minimal() +
    labs(title = dep_var_clean,
         color="Road Type") +
    theme(plot.title = element_text(hjust=0.5)) 
  ggsave(p, filename = file.path(figures_file_path, "phaseresults_allyears", paste0("phaseresults_allyears_",dep_var,".png")), height=4, width=12)
}



