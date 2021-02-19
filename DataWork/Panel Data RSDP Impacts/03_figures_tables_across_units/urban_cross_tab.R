# OLS

# Load / Prep Data -------------------------------------------------------------
data <- readRDS(file.path(panel_rsdp_imp_data_file_path, "dmspols_grid_ethiopia",
                          "merged_datasets", "panel_data_clean.Rds"))

data$globcover_urban <- as.numeric(data$globcover_urban > 0)

for(year in c(1996, 2012)){
  for(thresh in c(2, 6)){
    
    dataYY <- data[data$year %in% year,]
    df <- table(dataYY$globcover_urban, dataYY[[paste0("dmspols_", thresh)]]) %>% as.data.frame.matrix()
    
    sink(file.path(paper_tables, paste0("urban_cmatrix_dmsp",thresh,"_",year,".tex")))
    
    cat("\\begin{tabular}{c | cc | c} \n")
    cat(paste("           & NTL$>",thresh,"$         & NTL$>",thresh,"$         &                             \\\\  \n"))
    cat("                 & 0               & 1               &                             \\\\  \n")
    cat("\\hline  \n")
    cat(paste("GC-Urban ~0 & ", df$`0`[1], " & ", df$`1`[1], " & ", df$`0`[1] + df$`1`[1], " \\\\  \n"))
    cat(paste("GC-Urban ~1 & ", df$`0`[2], " & ", df$`1`[2], " & ", df$`0`[2] + df$`1`[2], " \\\\  \n"))
    cat("\\hline  \n")
    cat(paste(" & ", df$`0`[1] + df$`0`[2], " & ", df$`1`[1] + df$`1`[2], " & ", " \\\\  \n"))
    cat("\\end{tabular}  \n")
    
    sink()
    
  }
}








