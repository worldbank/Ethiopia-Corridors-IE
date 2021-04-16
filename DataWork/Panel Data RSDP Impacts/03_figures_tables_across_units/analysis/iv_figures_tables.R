# Figures and Tables Summarizing IV Results

# Load Data --------------------------------------------------------------------
dataset_types <- c("woreda",
                   "clusters_of_globcover_urban",
                   "clusters_of_ntlall",
                   "dmspols_grid_ethiopia")

sum_df <- map_df(dataset_types, function(DATASET_TYPE){
  
  bind_rows(
    readRDS(file.path(project_file_path, "Data", "Panel Data RSDP Impacts",
                      "Data", DATASET_TYPE, "results_datasets", 
                      "iv_rsdp123_summary_stats.Rds")),
    readRDS(file.path(project_file_path, "Data", "Panel Data RSDP Impacts",
                      "Data", DATASET_TYPE, "results_datasets", 
                      "iv_rsdp1234_summary_stats.Rds"))
  )
  
})

results_df <- map_df(dataset_types, function(DATASET_TYPE){
  
  bind_rows(
    readRDS(file.path(project_file_path, "Data", "Panel Data RSDP Impacts",
                      "Data", DATASET_TYPE, "results_datasets", 
                      "iv_rsdp123_results.Rds")),
    readRDS(file.path(project_file_path, "Data", "Panel Data RSDP Impacts",
                      "Data", DATASET_TYPE, "results_datasets", 
                      "iv_rsdp1234_results.Rds"))
  )
  
})

# Summary Tables ----------------------------------------------------------------
sum_df <- sum_df %>%
  arrange(rsdp, dataset) %>%
  mutate(dataset = case_when(
    dataset %in% "woreda" ~ "Woreda",
    dataset %in% "clusters_of_globcover_urban" ~ "Cities: Globcover",
    dataset %in% "clusters_of_ntlall" ~ "Cities: Nighttime Lights",
    dataset %in% "dmspols_grid_ethiopia" ~ "1x1km Grid"
  )) %>%
  mutate(latex_table1 = paste(rsdp, "&",
                              dataset, "&",
                              n_original %>% prettyNum(big.mark=",",scientific=FALSE), "&",
                              n          %>% prettyNum(big.mark=",",scientific=FALSE), "&",
                              n_treated  %>% prettyNum(big.mark=",",scientific=FALSE), "&",
                              near_mst_euc_1  %>% prettyNum(big.mark=",",scientific=FALSE), "&",
                              near_mst_euc_region_1  %>% prettyNum(big.mark=",",scientific=FALSE), "&",
                              near_mst_lc_1  %>% prettyNum(big.mark=",",scientific=FALSE), "&",
                              near_mst_lc_region_1  %>% prettyNum(big.mark=",",scientific=FALSE), "\\\\ \n")) 


sink(file.path(paper_tables, "iv_sumstat_allunits.tex"))

sum_rsdp123_df <- sum_df[sum_df$rsdp %in% "RSDP I-III",]
sum_rsdp1234_df <- sum_df[sum_df$rsdp %in% "RSDP I-V",]

cat("\\begin{tabular}{llll | lllll} \n")
cat("\\hline \n")
cat("RSDP & Unit & N Total & N, Targeted   & N       & N MST      & N MST Least    & N MST      & N MST Least  \\\\ \n")
cat("     &      &         & Areas Removed & Treated & Least Dist & Dist: Regional & Least Cost & Cost: Regional \\\\ \n")
cat("\\hline \n")

for(i in 1:nrow(sum_rsdp123_df)) cat(sum_rsdp123_df$latex_table1[i])
cat("\\hline \n")
for(i in 1:nrow(sum_rsdp1234_df)) cat(sum_rsdp1234_df$latex_table1[i])

cat("\\hline \n")
cat("\\end{tabular}")

sink()

# Prep Data --------------------------------------------------------------------
results_df <- results_df %>%
  filter(!(variable %in% c("(Intercept)",
                           "distance_rsdp123_targettedlocs_log",
                           "distance_rsdp1234_targettedlocs_log"))) %>%
  mutate(variable = variable %>%
           str_replace_all("\\(fit\\)", "") %>% 
           str_replace_all("\\`", "")) %>%
  mutate(variable = case_when(
    variable %in% "near_rsdp123" ~ "Near RSDP",
    variable %in% "near_rsdp1234" ~ "Near RSDP",
    
    variable %in% "near_rsdp123Xdmspols_1996_bin4_2" ~ "Near RSDP X NTL Low",
    variable %in% "near_rsdp123Xdmspols_1996_bin4_3" ~ "Near RSDP X NTL Medium",
    variable %in% "near_rsdp123Xdmspols_1996_bin4_4" ~ "Near RSDP X NTL High",
    variable %in% "near_rsdp1234Xdmspols_1996_bin4_2" ~ "Near RSDP X NTL Low",
    variable %in% "near_rsdp1234Xdmspols_1996_bin4_3" ~ "Near RSDP X NTL Medium",
    variable %in% "near_rsdp1234Xdmspols_1996_bin4_4" ~ "Near RSDP X NTL High",
    variable %in% "near_rsdp123Xdistance_city_addisababa" ~ "Near RSDP X Dist Addis",
    variable %in% "near_rsdp1234Xdistance_city_addisababa" ~ "Near RSDP X Dist Addis"
  )) %>%
  mutate(variable = variable %>%
           factor(c("Near RSDP",
                    "Near RSDP X NTL Low",
                    "Near RSDP X NTL Medium",
                    "Near RSDP X NTL High",
                    "Near RSDP X Dist Addis") %>% rev)) %>%
  mutate(dataset = case_when(
    dataset %in% "woreda" ~ "Woreda",
    dataset %in% "clusters_of_globcover_urban" ~ "Cities: Globcover",
    dataset %in% "clusters_of_ntlall" ~ "Cities: Nighttime Lights",
    dataset %in% "dmspols_grid_ethiopia" ~ "1x1km Grid"
  ) %>%
    factor(c("1x1km Grid",
             "Cities: Globcover",
             "Cities: Nighttime Lights",
             "Woreda") %>% rev))

## Text
results_df$stars <- ""
results_df$stars[results_df$pvalue <= 0.1] <- "*"
results_df$stars[results_df$pvalue <= 0.05] <- "**"
results_df$stars[results_df$pvalue <= 0.01] <- "***"

results_df$b_round <- results_df$b %>% round(3)

results_df$text <- paste0(results_df$b_round, results_df$stars)

make_figure <- function(rsdp_i,
                        df_i,
                        interaction_i,
                        results_df){
  results_df %>%
    filter(rsdp %in% rsdp_i,
           dv %in% df_i,
           interaction %in% interaction_i) %>%
    ggplot(aes(y = dataset,
               x = b, xmin = p025, xmax = p975,
               group = variable,
               color = variable)) +
    geom_vline(xintercept = 0,
               size = 0.1) +
    geom_linerange(position = position_dodge(width = 0.9)) +
    geom_point(position = position_dodge(width = 0.9)) +
    geom_text(aes(label = text), 
              position = position_dodge(width = 0.9),
              vjust = -0.5,
              size = 2.25) +
    labs(x = "Coefficient (+/- 95% CI)",
         color = "Indep. Var",
         y = NULL) +
    guides(color = guide_legend(reverse=T)) +
    #theme(aspect.ratio = 2/1) +
    theme(strip.text.x = element_text(size = 7, face = "bold")) +
    facet_wrap(~ivtype,
               scales = "free_x",
               nrow = 1)
}

for(rsdp_i in c("RSDP I-III",
                "RSDP I-V")){
  for(dv_i in c("dmspols_ihs", "globcover_crop", "globcover_urban")){
    
    if(rsdp_i %in% "RSDP I-V"){
      results_df_i <- results_df[!(results_df$interaction == "basentl" & results_df$dataset %in% "Cities: Nighttime Lights"),]
      
    } else{
      results_df_i <- results_df
    }
    
    if(dv_i %in% "dmspols_ihs") dv_name <- "Nighttime Lights"
    if(dv_i %in% "globcover_crop") dv_name <- "Cropland"
    if(dv_i %in% "globcover_urban") dv_name <- "Urban"
    
    none <- make_figure(rsdp_i,
                        dv_i,
                        "none",
                        results_df_i)
    
    basentl <- make_figure(rsdp_i,
                           dv_i,
                           "basentl",
                           results_df_i)
    
    addis <- make_figure(rsdp_i,
                         dv_i,
                         "addis",
                         results_df_i)
    
    p <- ggarrange(none,
                   basentl,
                   addis,
                   heights = c(0.2,0.4,0.3),
                   ncol = 1)
    
    
    p <- annotate_figure(p,
                         top = text_grob(paste0("Dependent Variable: ", dv_name),
                                         color = "black", face = "bold", size = 14)
    )
    
    ggsave(p, filename = file.path(paper_figures,
                                   paste0("iv_figures",
                                          rsdp_i %>% str_replace_all(" |[:punct:]", "") %>% tolower(),
                                          "_",
                                          dv_i,
                                          ".png")),
           height = 8.5, width = 9.75)
    
  }
}






