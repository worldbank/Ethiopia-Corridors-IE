# Summary Statistics

# Define Helper Functions ------------------------------------------------------

if_na_return <- function(x, 
                         value = "N/A"){
  # If "x" is na, then return "value"
  
  if(is.na(x)){
    x <- value
  }
  
  return(x)
}

if_zero_return <- function(x, 
                           value = "N/A"){
  # If "x" is na, then return "value"
  
  if(x %in% 0){
    x <- value
  }
  
  return(x)
}

# Sum Stats --------------------------------------------------------------------
data_kebele_full   <- readRDS(file.path(panel_rsdp_imp_data_file_path, "kebele",                "merged_datasets", "panel_data_clean.Rds"))
data_grid_near_rd  <- readRDS(file.path(panel_rsdp_imp_data_file_path, "dmspols_grid_nearroad", "merged_datasets", "panel_data_clean.Rds"))
data_grid_full     <- readRDS(file.path(panel_rsdp_imp_data_file_path, "dmspols_grid_ethiopia", "merged_datasets", "panel_data_clean.Rds"))

data_kebele_near_rd <- data_kebele_full[!is.na(data_kebele_full$years_since_improvedroad),]

# Function for Sum Stats -------------------------------------------------------
make_sum_stats <- function(data, 
                           variables,
                           ROUND_NUM_NTL = 2,
                           ROUND_NUM_URBAN = 2){
  
  for(var in variables){
    
    if(var %in% "dmspols_harmon")          var_name <- "NTL"
    if(var %in% "dmspols_zhang")          var_name <- "NTL"
    if(var %in% "dmspols_zhang_base0na")  var_name <- "NTL ($>$ 0 at Baseline)"
    if(var %in% "dmspols_zhang_2")        var_name <- "NTL $\\geq$ 2"
    if(var %in% "dmspols_zhang_6")        var_name <- "NTL $\\geq$ 6"
    if(var %in% "dmspols_zhang_sum2")     var_name <- "NTL $\\geq$ 2"
    if(var %in% "dmspols_zhang_sum6")     var_name <- "NTL $\\geq$ 6"
    if(var %in% "globcover_urban")        var_name <- "Urban"
    if(var %in% "globcover_cropland")     var_name <- "Cropland"
    if(var %in% "globcover_urban_sum")    var_name <- "Urban"
    if(var %in% "globcover_cropland_sum") var_name <- "Cropland"
    if(var %in% "ndvi")                   var_name <- "NDVI"
    if(var %in% "ndvi_cropland")          var_name <- "NDVI in Cropland"
    
    if(var %in% c("globcover_urban", "globcover_cropland")){
      ROUND_NUM <- ROUND_NUM_URBAN
    } else{
      ROUND_NUM <- ROUND_NUM_NTL
    }
    
    mean_1996 <- data[[var]][data$year %in% 1996] %>% mean(na.rm = T) %>% round(ROUND_NUM)
    mean_2013 <- data[[var]][data$year %in% 2013] %>% mean(na.rm = T) %>% round(ROUND_NUM)
    mean_2016 <- data[[var]][data$year %in% 2016] %>% mean(na.rm = T) %>% round(ROUND_NUM) %>% if_na_return()
    
    sumNon_1996 <- sum(data[[var]][data$year %in% 1996] > 0, na.rm = T) 
    sumNon_2013 <- sum(data[[var]][data$year %in% 2013] > 0, na.rm = T) 
    sumNon_2016 <- sum(data[[var]][data$year %in% 2016] > 0, na.rm = T) %>% if_zero_return()
    
    cat(var_name, " & ",
        mean_1996 %>% prettyNum(big.mark=",",scientific=FALSE), " & ",
        mean_2013 %>% prettyNum(big.mark=",",scientific=FALSE), " & ",
        mean_2016 %>% prettyNum(big.mark=",",scientific=FALSE), " & ",
        sumNon_1996 %>% prettyNum(big.mark=",",scientific=FALSE), " & ",
        sumNon_2013 %>% prettyNum(big.mark=",",scientific=FALSE), " & ",
        sumNon_2016 %>% prettyNum(big.mark=",",scientific=FALSE), " \\\\ \n")
    
  }
}

# Make Table -------------------------------------------------------------------
sink(file.path(paper_tables,
               "depvar_sumstats.tex"))

cat("\\begin{tabular}{l | ccc | ccc } \n")
cat("\\hline \n")
cat("Variable & \\multicolumn{3}{c|}{Average} & \\multicolumn{3}{c}{Number of Units with Value $>$ 0} \\\\ \n")
cat("         & 1996 & 2013 & 2016           &  1996 & 2013 & 2016                  \\\\ \n")
cat("\\hline \n")

## 1x1km Grid - Full
N <- data_grid_full %>% filter(year %in% 1996) %>% nrow() %>% prettyNum(big.mark=",",scientific=FALSE)
cat("\\multicolumn{4}{l}{{\\bf 1x1km Grid}} & ")
cat(paste0("\\multicolumn{3}{r}{N Units: ", N, "} \\\\ \n"))
make_sum_stats(data_grid_full, 
               variables = c("dmspols_harmon", 
                             "globcover_urban",
                             "globcover_cropland"),
               ROUND_NUM_URBAN = 4)

## 1x1km Grid - Near Road
cat("\\hline \n")
N <- data_grid_near_rd %>% dplyr::filter(year %in% 1996) %>% nrow() %>% prettyNum(big.mark=",",scientific=FALSE)
cat("\\multicolumn{5}{l}{{\\bf 1x1km Grid - Near Improved Road}} & ")
cat(paste0("\\multicolumn{2}{r}{N Units: ", N, "} \\\\ \n"))
make_sum_stats(data_grid_near_rd,
               variables = c("dmspols_harmon",
                             "globcover_urban",
                             "globcover_cropland"),
               ROUND_NUM_URBAN = 4)

## Kebele - Full
cat("\\hline \n")
N <- data_kebele_full %>% filter(year %in% 1996) %>% nrow() %>% prettyNum(big.mark=",",scientific=FALSE)
cat("\\multicolumn{4}{l}{{\\bf Kebele}} & ")
cat(paste0("\\multicolumn{3}{r}{N Units: ", N, "} \\\\ \n"))
make_sum_stats(data_kebele_full, 
               variables = c("dmspols_harmon", 
                             "globcover_urban_sum",
                             "globcover_cropland_sum"))

## Kebele - Full
cat("\\hline \n")
N <- data_kebele_near_rd %>% filter(year %in% 1996) %>% nrow() %>% prettyNum(big.mark=",",scientific=FALSE)
cat("\\multicolumn{4}{l}{{\\bf Kebele - Near Improved Road}} & ")
cat(paste0("\\multicolumn{3}{r}{N Units: ", N, "} \\\\ \n"))
make_sum_stats(data_kebele_near_rd, 
               variables = c("dmspols_harmon", 
                             "globcover_urban_sum",
                             "globcover_cropland_sum"))


cat("\\hline \n")
cat("\\end{tabular} \n")

sink()


