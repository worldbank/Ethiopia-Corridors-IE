# Phase Road States

ROUND_NUM <- 0 # number of decimal places to round to

# Load Data --------------------------------------------------------------------
roads <- readRDS(file.path(data_file_path, "RSDP Roads", "FinalData", "RoadNetworkPanelData_1996_2016.Rds"))

length_df <- roads@data %>% 
  group_by(rsdp_phase, speed_phasestart) %>%
  dplyr::summarise(LINKLENGTH = sum(LINKLENGTH)) %>%
  filter(rsdp_phase %in% 1:4) %>%
  pivot_wider(names_from = rsdp_phase, values_from = LINKLENGTH) %>%
  arrange(speed_phasestart) %>%
  mutate(speed_phasestart = replace(speed_phasestart, speed_phasestart==0, "No Existing Road")) 
names(length_df) <- c("baseline_speed", "phase1", "phase2", "phase3", "phase4")

totallength_df <- colSums(length_df[,2:5], na.rm=T) %>%
  t %>%
  as.data.frame() %>%
  mutate(baseline_speed = "Total")

length_df <- bind_rows(length_df, totallength_df)

for(var in names(length_df)) length_df[[var]][is.na(length_df[[var]])] <- 0

# Generate LaTeX Table ---------------------------------------------------------
length_df$latex <- paste(length_df$baseline_speed, " & ",
                         length_df$phase1 %>% round(ROUND_NUM) %>% prettyNum(big.mark=",",scientific=FALSE), " & ",
                         length_df$phase2 %>% round(ROUND_NUM) %>% prettyNum(big.mark=",",scientific=FALSE), " & ",
                         length_df$phase3 %>% round(ROUND_NUM) %>% prettyNum(big.mark=",",scientific=FALSE), " & ",
                         length_df$phase4 %>% round(ROUND_NUM) %>% prettyNum(big.mark=",",scientific=FALSE), " \\\\ ")

sink(file.path(paper_tables, "phase_improved_length_byspeed.tex"))
cat("\\begin{tabular}{c | cccc} \n")
cat("\\hline \n")
cat("Speed at & \\multicolumn{4}{c}{Phase} \\\\ \n")
cat("Phase Start & I & II & III & IV \\\\ \n")
cat("\\hline ")
for(i in 1:nrow(length_df)){
  
  if(i %in% nrow(length_df)) cat("\\hline \n")
  cat(length_df$latex[i], " \n")
  
}
cat("\\hline \n")
cat("\\end{tabular} \n")
sink()

