# Analysis: Coefficient Each Year - Results

# Exports dataframe of results, to be used to make figures

# https://cran.r-project.org/web/packages/did/vignettes/did-basics.html

#### Parameters
OVERWRITE_FILES <- F

library(did)

# Load Data --------------------------------------------------------------------
dataset <- "clusters_of_ntlall"
dep_var <- "globcover_urban_ihs"
indep_var <- "year_improvedroad"
ntl_group <- "all"

for(dataset in c("woreda", 
                 "clusters_of_ntlall",
                 "clusters_of_globcover_urban",
                 "dmspols_grid_nearroad")){
  for(dep_var in c("globcover_urban_ihs", 
                   "dmspols_harmon_ihs")){
    for(indep_var in c("year_improvedroad",
                       "year_improvedroad_50aboveafter",
                       "year_improvedroad_below50after")){
      for(ntl_group in c("all", 
                         "low", 
                         "high")){
        
        print(paste(dataset, dep_var, indep_var, ntl_group, sep = " - "))
        
        ## Load data
        if(dataset == "dmspols_grid_nearroad"){
          data <- readRDS(file.path(panel_rsdp_imp_data_file_path, dataset, "merged_datasets", "grid_data_clean.Rds"))
          data <- data %>% group_by(woreda_id) %>% mutate(dmspols_1996sum = sum(dmspols[year == 1996], na.rm=T))
          cluster_var <- "woreda_id"
        } else{
          data <- readRDS(file.path(panel_rsdp_imp_data_file_path, dataset, "merged_datasets", "panel_data_clean.Rds"))
          data <- data %>% group_by(cell_id) %>% mutate(dmspols_1996sum = dmspols_sum[year == 1996])
          cluster_var <- NULL
        }
        
        #data <- data[data$cell_id %in% c(1:10, 200:210, 300:310, 400:410, 500:505),]
        
        ## Dep Var / Indep Var Variables
        if(dep_var %in% "globcover_urban_ihs" & dataset %in% "dmspols_grid_nearroad") dep_var <- "globcover_urban"
        
        data$dep_var   <- data[[dep_var]]
        data$indep_var <- data[[indep_var]]
        
        ## Median Group
        m <- data$dmspols_1996sum[data$dmspols_1996sum > 0] %>% median(na.rm=T)
        data$ntl_group <- NA
        data$ntl_group[data$dmspols_1996sum <= m] <- "1"
        data$ntl_group[data$dmspols_1996sum > m] <- "2"
        
        if(ntl_group %in% "low")  data <- data[data$ntl_group %in% "1",]
        if(ntl_group %in% "high") data <- data[data$ntl_group %in% "2",]
        
        ## Subset
        data = data %>%
          ungroup() %>%
          dplyr::filter(year >= 1996,
                        year <= 2016,
                        !is.na(indep_var))
        
        # This way of selecting specific variables is robust to some names (ie, woreda_id)
        # not being in all the datasets
        data <- data[,names(data) %in% c("dep_var", "indep_var", "cell_id", "year", "woreda_id")]
        
        ## Title
        dataset_name <- case_when(dataset %in% "woreda"                      ~ "Woreda",
                                  dataset %in% "clusters_of_globcover_urban" ~ "Cities - GC-Urban",
                                  dataset %in% "clusters_of_ntlall"          ~ "Cities - NTL",
                                  dataset %in% "dmspols_grid_nearroad"       ~ "1x1km Grid")
        
        indepvar_name <- case_when(indep_var %in% "year_improvedroad"              ~ "All Roads",
                                   indep_var %in% "year_improvedroad_50aboveafter" ~ "Roads >= 50km/hr After Upgrade",
                                   indep_var %in% "year_improvedroad_below50after" ~ "Roads < 50km/hr After Upgrade")
        
        depvar_name <- case_when(dep_var %in% "globcover_urban_ihs" ~ "Urban",
                                 dep_var %in% "globcover_urban" ~ "Urban",
                                 dep_var %in% "dmspols_harmon_ihs" ~ "NTL")
        
        ntl_group_name <- case_when(ntl_group %in% "low" ~ "Low",
                                    ntl_group %in% "high" ~ "High",
                                    ntl_group %in% "all" ~ "All")
        
        title <- paste0("Dep Var: ",
                        depvar_name,
                        ";  Indep Var: ",
                        indepvar_name,
                        ";  NTL Group: ",
                        ntl_group_name,
                        ";  Dataset: ",
                        dataset_name)
        
        example_attgt <- att_gt(yname = "dep_var",
                                tname = "year",
                                idname = "cell_id",
                                gname = "indep_var",
                                xformla = ~1,
                                data = data,
                                control_group = "notyettreated",
                                clustervars = cluster_var,
                                print_details = T
        )
        
        ## Aggregate ATTs
        agg.simple.dynamic <- aggte(example_attgt, type = "dynamic")
        p_dynamic <- ggdid(agg.simple.dynamic)
        
        agg.simple.group <- aggte(example_attgt, type = "group")
        p_group <- ggdid(agg.simple.group)
        
        ## Save Figure
        p_all <- ggarrange(p_dynamic, 
                           p_group,
                           nrow = 1)
        
        p_all <- annotate_figure(p_all,
                                 top = text_grob(title, color = "black", face = "bold", size = 12))
        
        ggsave(p_all, filename = file.path(paper_figures,
                                           paste0("did_attgt_",
                                                  dataset, "_", dep_var, "_", indep_var, "_", ntl_group, ".png")),
               height = 3, width = 10)
        
        #### Save Data
        ## Dynamic
        dynamic_df <- data.frame(time               = agg.simple.dynamic$egt,
                                 att                = agg.simple.dynamic$att.egt,
                                 se                 = agg.simple.dynamic$se.egt,
                                 critical_value_95p = as.numeric(agg.simple.dynamic$crit.val.egt)) %>%
          mutate(dataset = dataset,
                 dep_var = dep_var,
                 indep_var = indep_var,
                 ntl_group = ntl_group)
        
        saveRDS(dynamic_df, 
                file.path(panel_rsdp_imp_data_file_path,
                          "all_units",
                          "results_datasets",
                          "individual_datasets",
                          paste0("dynamic_did_attgt_",
                                 dataset, "_", dep_var, "_", indep_var, "_", ntl_group, ".Rds")))
        
        ## Group
        group_df <- data.frame(group              = agg.simple.group$egt,
                               att                = agg.simple.group$att.egt,
                               se                 = agg.simple.group$se.egt,
                               critical_value_95p = as.numeric(agg.simple.group$crit.val.egt)) %>%
          mutate(dataset = dataset,
                 dep_var = dep_var,
                 indep_var = indep_var,
                 ntl_group = ntl_group)
        
        saveRDS(group_df, 
                file.path(panel_rsdp_imp_data_file_path,
                          "all_units",
                          "results_datasets",
                          "individual_datasets",
                          paste0("group_did_attgt_",
                                 dataset, "_", dep_var, "_", indep_var, "_", ntl_group, ".Rds")))
        
        
      }
    }
  }
}

