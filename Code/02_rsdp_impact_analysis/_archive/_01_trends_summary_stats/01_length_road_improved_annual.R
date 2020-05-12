# Impact of Expressway Expansion
# Ethiopia IE

# Setup ------------------------------------------------------------------------
if(Sys.info()[["user"]] == "r521633") project_file_path <- "/home/wb521633/IEs/Ethiopia IE"
if(Sys.info()[["user"]] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE"
if(Sys.info()[["user"]] == "robmarty") project_file_path <- "~/Dropbox/World Bank/IEs/Ethiopia IE"

tables_file_path <- file.path(project_file_path, "Results", "Tables")
figures_file_path <- file.path(project_file_path, "Results", "Figures")
rawdata_file_path <- file.path(project_file_path, "Data", "RawData")

library(lfe)
library(reshape)
library(dplyr)
library(ggplot2)
library(data.table)
library(coefplot)
library(stringr)
library(doBy)
library(stargazer)
library(rgdal)

DIST_THRESH <- 2 #km to be considered near a road

# Load Roads -------------------------------------------------------------------
setwd(file.path(rawdata_file_path, "RoadNetworkPanelDataV3_1996_2016_Revised"))
roads_2016 <- readOGR(dsn=".", layer=paste0("All_Network_","2016")) %>% spTransform(CRS(UTM_ETH))

for(year in c(1997,1999,2001,2003,2005,2007,2009,2011,2013,2015)){
  roads_2016[[paste0("Speed",year)]] <- roads_2016[[paste0("Speed",year-1)]]
  roads_2016[[paste0("Speed",year)]][roads_2016$Complete_G == year] <- roads_2016[[paste0("Speed",year+1)]][roads_2016$Complete_G == year]
}

# Improvement Each Year --------------------------------------------------------
for(year in 1997:2016){
  roads_2016[["improved_"]]
}

improvements_df <- lapply(1997:2016, function(year){
  
  roads_2016_i <- roads_2016[roads_2016[[paste0("Speed",year)]] > roads_2016[[paste0("Speed", year-1 )]],]
  
  speeds <- c(0, 10,15,20,25,30,35,45,50,70,120)
  speed_lengths_df <- lapply(speeds, function(speed){
    
    out <- data.frame(speed = speed,
                      length_year_before = roads_2016_i[roads_2016_i[[paste0("Speed", year-1 )]] %in% speed,] %>% gLength(),
                      length_year_current = roads_2016_i[roads_2016_i[[paste0("Speed", year )]] %in% speed,] %>% gLength())
    
    return(out)
  }) %>% bind_rows
  
  speed_lengths_df$year <- year
  
  return(speed_lengths_df)
}) %>% bind_rows

improvements_year_df <- subset(improvements_df, select=c(speed,length_year_current,year))
improvements_year_df <- improvements_year_df %>% spread(speed, length_year_current)

improvements_yearbefore_df <- subset(improvements_df, select=c(speed,length_year_before,year))
improvements_yearbefore_df <- improvements_yearbefore_df %>% spread(speed, length_year_before)

for(var in names(improvements_yearbefore_df)){
  if(var != "year") improvements_yearbefore_df[[var]] <- round(improvements_yearbefore_df[[var]] / 1000)
}

for(var in names(improvements_year_df)){
  if(var != "year") improvements_year_df[[var]] <- round(improvements_year_df[[var]] / 1000)
}

# Make Tables ------------------------------------------------------------------
sink(file.path(tables_file_path, "length_road_improved_from.tex"))
cat("\\begin{tabular}{lccccccccccc} ")
cat("\\hline ")
cat("Year & 0 & 10 & 15 & 20 & 25 & 30 & 35 & 45 & 50 & 70 & 120 \\\\ ")
cat("\\hline ")
for(i in 1:nrow(improvements_yearbefore_df)){
  row_latex <- improvements_yearbefore_df[i,] %>% as.character %>% paste(collapse = " & ") %>% paste(" \\\\ ")
  cat(row_latex)
}
cat("\\hline ")
sums <- colSums(improvements_yearbefore_df)[-1]
sums <- c("Total", sums) %>% paste(collapse = " & ") %>% paste(" \\\\ ")
cat(sums)
cat("\\hline ")
cat("\\end{tabular} ")
sink()




sink(file.path(tables_file_path, "length_road_improved_to.tex"))
cat("\\begin{tabular}{lccccccccccc} ")
cat("\\hline ")
cat("Year & 0 & 10 & 15 & 20 & 25 & 30 & 35 & 45 & 50 & 70 & 120 \\\\ ")
cat("\\hline ")
for(i in 1:nrow(improvements_year_df)){
  row_latex <- improvements_year_df[i,] %>% as.character %>% paste(collapse = " & ") %>% paste(" \\\\ ")
  cat(row_latex)
}
cat("\\hline ")
sums <- colSums(improvements_year_df)[-1]
sums <- c("Total", sums) %>% paste(collapse = " & ") %>% paste(" \\\\ ")
cat(sums)
cat("\\hline ")
cat("\\end{tabular} ")
sink()
