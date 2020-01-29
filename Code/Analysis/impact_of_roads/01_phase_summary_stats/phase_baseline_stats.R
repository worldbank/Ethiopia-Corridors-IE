# Phase Road States

NEAR_THRESHOLD <- 5 * 1000
ROUND_NUM <- 2

# Load and Prep Road Data ------------------------------------------------------
points <- readRDS(file.path(finaldata_file_path, DATASET_TYPE, "merged_datasets", "grid_data_clean.Rds"))

rsdp_rounds_stacked <- bind_rows(
  points[points$distance_rsdp_phase1 <= NEAR_THRESHOLD,] %>% filter(year %in% 1997) %>% mutate(rsdp_round = 1),
  points[points$distance_rsdp_phase2 <= NEAR_THRESHOLD,] %>% filter(year %in% 2003) %>% mutate(rsdp_round = 2),
  points[points$distance_rsdp_phase3 <= NEAR_THRESHOLD,] %>% filter(year %in% 2008) %>% mutate(rsdp_round = 3), 
  points[points$distance_rsdp_phase4 <= NEAR_THRESHOLD,] %>% filter(year %in% 2011) %>% mutate(rsdp_round = 4) 
)

summary_df <- rsdp_rounds_stacked %>%
  group_by(rsdp_round) %>%
  summarise(dmspols_mean = mean(dmspols),
            dmspols_prop_lit = mean(dmspols > 0),
            dmspols_prop_lit_N = sum(dmspols > 0),
            globcover_cropland_prop = mean(globcover_cropland),
            globcover_urban_prop = mean(globcover_urban),
            globcover_urban_N = sum(globcover_urban > 0),
            distance_city_addisababa_km_mean = mean(distance_city_addisababa)/1000) %>%
  t() %>%
  as.data.frame() %>%
  dplyr::rename(phase_1 = V1,
        phase_2 = V2,
        phase_3 = V3,
        phase_4 = V4) %>%
  rownames_to_column() %>%
  filter(rowname != "rsdp_round")
summary_df$rowname[summary_df$rowname %in% "dmspols_mean"] <- "NTL, Mean"
summary_df$rowname[summary_df$rowname %in% "dmspols_prop_lit"] <- "NTL, Prop Cells Lit"
summary_df$rowname[summary_df$rowname %in% "dmspols_prop_lit_N"] <- "NTL, N Cells Lit"
summary_df$rowname[summary_df$rowname %in% "globcover_cropland_prop"] <- "Prop Cropland (Globcove)"
summary_df$rowname[summary_df$rowname %in% "globcover_urban_prop"] <- "Prop Urban (Globcover)"
summary_df$rowname[summary_df$rowname %in% "globcover_urban_N"] <- "N Cells with Urban Area (Globcover)"
summary_df$rowname[summary_df$rowname %in% "distance_city_addisababa_km_mean"] <- "Avg Dist to Addis Ababa (km)"

# Generate Table ---------------------------------------------------------------
summary_df <- summary_df %>%
  mutate(latex = paste(rowname, " & ", 
                       phase_1 %>% round(ROUND_NUM), " & ", 
                       phase_2 %>% round(ROUND_NUM), " & ", 
                       phase_3 %>% round(ROUND_NUM), " & ", 
                       phase_4 %>% round(ROUND_NUM), " \\\\ "))

sink(file.path(tables_file_path, "phase_baseline_stats.tex"))
cat("\\begin{tabular}{l | cccc} ")
cat("\\hline ")
cat("Variable & \\multicolumn{4}{c}{Phase} \\\\ ")
cat(" & I & II & III & IV \\\\ ")
cat("\\hline ")
for(i in 1:nrow(summary_df)){
  
  cat(summary_df$latex[i])
  
}
cat("\\hline ")
cat("\\end{tabular} ")
sink()


