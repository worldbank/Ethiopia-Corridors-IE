# Stock of Roads Improved by Phase

# @Rob: could you add the % of total stock improved by provinces for each phase? 
# For ex, 552.52 km of road were improved in Afar in the first phase of RSDP (1997-2001), 
# and this represent n % of the stock of roads at the beginning of the period considered. 
# We need n for every province at the beginning of the periods

# Phases:
  # I: 1997 - 2002
  # II: 2002 - 2007
  # III: 2007 - 2010
  # IV: 2010 - 2015

# Load Data --------------------------------------------------------------------
phase_results_all <- read.csv(file.path(finaldata_file_path, "rsdp_summary_data", "rsdp_improvement_by_phase.csv"))
phase_results_all$Phase <- phase_results_all$phase

improved_length <- ggplot() +
  geom_col(data=phase_results_all, aes(x=province, y=roads_improved_length_km/1000)) +
  facet_wrap(~Phase, ncol = 2, labeller = label_both) +
  coord_flip() +
  theme_minimal() +
  labs(x="",
       y="",
       title="Length of Road Improved (1,000 Km)") + 
  theme(axis.text.y = element_text(size=8))
ggsave(improved_length, filename = file.path(figures_file_path, "rsdp_summary_trends", "rsdp_improvement_province_length.png"), width=5, height=5)

improved_proportion <- ggplot() +
  geom_col(data=phase_results_all, aes(x=province, y=roads_improved_prop)) +
  facet_wrap(~Phase, ncol = 2, labeller = label_both) +
  coord_flip() +
  theme_minimal() +
  labs(x="",
       y="",
       title="Proportion of Road Network Improved")
ggsave(improved_proportion, filename = file.path(figures_file_path, "rsdp_summary_trends", "rsdp_improvement_province_proportion.png"), width=5, height=5)



