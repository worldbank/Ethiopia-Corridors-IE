# Impact of Expressway Expansion
# Ethiopia IE

dataset <- "points_5percent"

# TODO: Collapse coefficients in later years into one >10;<10

# Load Data --------------------------------------------------------------------
if(dataset == "cluster_all"){
  data <- readRDS(file.path(project_file_path, "Data", "FinalData", "urban_cluster_dataset", "urban_cluster_data_analysisvars.Rds"))
  data$cell_id <- data$cluster_id
}

if(dataset == "points"){
  data <- readRDS(file.path(project_file_path, "Data", "FinalData", "dmspols_grid_dataset", "dmspols_level_dataset_analysisvars.Rds"))
}

if(dataset == "points_5percent"){
  data <- readRDS(file.path(project_file_path, "Data", "FinalData", "dmspols_grid_dataset", "dmspols_level_dataset_5percentsample_analysisvars.Rds"))
  data$gc_urban_mean <- data$globcover_urban
}

data <- data[(data$year >= 1996) & (data$year <= 2016),]

# Create/Clean Variables -------------------------------------------------------
### Region Type
data$region_type <- ifelse(data$GADM_ID_1 %in% c("Afar", "Benshangul-Gumaz", "Somali"), "Sparse", "Dense")

# Year Since Improved - Grouped Together later/early years
data$years_since_improved_all <- data$years_since_improved_all %>% as.character() %>% as.numeric
data$years_since_improved_50above <- data$years_since_improved_50above %>% as.character() %>% as.numeric
data$years_since_improved_below50 <- data$years_since_improved_below50 %>% as.character() %>% as.numeric

data$years_since_improved_all[data$years_since_improved_all >= 10] <- 10
data$years_since_improved_50above[data$years_since_improved_50above >= 10] <- 10
data$years_since_improved_below50[data$years_since_improved_below50 >= 10] <- 10

data$years_since_improved_all[data$years_since_improved_all <= -10] <- -10
data$years_since_improved_50above[data$years_since_improved_50above <= -10] <- -10
data$years_since_improved_below50[data$years_since_improved_below50 <= -10] <- -10

data$years_since_improved_all <- data$years_since_improved_all %>% as.factor() %>% relevel(ref="-1")
data$years_since_improved_50above <- data$years_since_improved_50above %>% as.factor() %>% relevel(ref="-1")
data$years_since_improved_below50 <- data$years_since_improved_below50 %>% as.factor() %>% relevel(ref="-1")

# Functions --------------------------------------------------------------------
lm_confint_tidy <- function(lm){
  lm_confint <- confint(lm) %>% 
    as.data.frame
  names(lm_confint) <- c("p025", "p975")
  lm_confint$b <- (lm_confint$p025 + lm_confint$p975)/2
  lm_confint$variable <- row.names(lm_confint)
  
  lm_confint <- lm_confint[!grepl("cluster_id)|year)|Intercept)", lm_confint$variable),]
  lm_confint$years_since_improved <- gsub(".*)", "", lm_confint$variable) %>% as.numeric
  
  return(lm_confint)
}

# Regressions ------------------------------------------------------------------
DV <- "dmspols"
FE <- "cell_id"
dmspols_1997_bin_choice <- "all"
region_type <- "All"

# Initialize dataframe to store results
results_all <- data.frame(NULL)
model_id <- 1

for(DV in c("dmspols", "dmspols_zhang",
            "dmspols_1", "dmspols_5",
            "dmspols_zhang_1", "dmspols_zhang_5",
            "ndvi", "ndvi_cropland",
            "globcover_urban", "globcover_cropland")){
  for(FE in c("year", "cell_id + year", "GADM_ID_3 + year")){
    for(dmspols_1997_bin_choice in c("zero","low","high","all")){
      for(region_type in c("Dense","Sparse", "All")){

        tryCatch({
          
          # Print current step in for loop
          print(paste(DV, FE, dmspols_1997_bin_choice, region_type))
          
          # If region type is ALL, give all types
          if(region_type %in% "All"){
            region_type <- unique(data$region_type)
          }
          
          # Create nighttime lights base variable to subset on
          if(dmspols_1997_bin_choice == "zero") ntl_base <- 1 
          if(dmspols_1997_bin_choice == "low") ntl_base <- 2 
          if(dmspols_1997_bin_choice == "high") ntl_base <- 3 
          if(dmspols_1997_bin_choice == "all") ntl_base <- 1:3 
          
          # Define formula
          improved_all_formula <- paste(DV, "~ years_since_improved_all | ", FE, " | 0 | GADM_ID_3") %>% as.formula
          improved_byspeed_formula <- paste(DV, "~ years_since_improved_50above + years_since_improved_below50 | ", FE, " | 0 | GADM_ID_3") %>% as.formula
          
          # Regressions and create dataframes of results
          improved_all_felm <- felm(improved_all_formula, data=data[(data$dmspols_1997_group %in% ntl_base) & 
                                                                      (data$region_type %in% region_type),]) %>% 
            lm_confint_tidy %>% 
            dplyr::mutate(model_category = "All Roads") %>%
            dplyr::mutate(improved_road = "All") %>%
            dplyr::mutate(DV = DV) %>%
            dplyr::mutate(FE = gsub("\\+| ","",FE)) %>%
            dplyr::mutate(dmspols_1997_bin = paste(ntl_base, collapse="")) %>%
            dplyr::mutate(region_type = region_type %>% paste(collapse=";", sep=""))
          improved_all_felm$model_id <- paste0("all_", model_id)
          
          improved_byspeed_felm <- felm(improved_byspeed_formula, data=data[data$dmspols_1997_group %in% ntl_base,]) %>% 
            lm_confint_tidy %>% 
            dplyr::mutate(model_category = "By Speed") %>%
            dplyr::mutate(DV = DV) %>%
            dplyr::mutate(FE = gsub("\\+| ","",FE)) %>%
            dplyr::mutate(dmspols_1997_bin = paste(ntl_base, collapse="")) %>%
            dplyr::mutate(region_type = region_type %>% paste(collapse=";", sep=""))
          
          improved_byspeed_felm$improved_road <- ifelse(grepl("50above", improved_byspeed_felm$variable), "50above", "below50")
          improved_byspeed_felm$model_id <- paste0("byspeed_", model_id)
          
          # Append results to master dataframe
          results_i <- bind_rows(improved_all_felm, improved_byspeed_felm)
          results_all <- bind_rows(results_all, results_i)
          
          model_id <- model_id + 1
        }, error=function(e){})
      
      }
    }
  }
}

# Export Results ---------------------------------------------------------------
saveRDS(results_all, file.path(finaldata_file_path, "lead_lag_results_coefficients", paste0("lead_lag_results.Rds_",dataset)))
write.csv(results_all, file.path(finaldata_file_path, "lead_lag_results_coefficients", paste0("lead_lag_results_",dataset,".Rds")), row.names=F)

