# Impact of Roads Coef Plot

# Setup ------------------------------------------------------------------------
if(Sys.info()[["user"]] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE"
if(Sys.info()[["user"]] == "robmarty") project_file_path <- "~/Dropbox/World Bank/IEs/Ethiopia IE"

scale <- .09
coef_df <- data.frame(var = c("No Initial\nLuminosity", "Below Median\nLuminosity", "Above Median\nLuminosity"),
           b = c(0.0093, 1.2027, 2.5314),
           lb = c(0.0029-scale, 1.0265, 2.1488),
           ub = c(0.0158+scale, 1.3790, 2.9140))
coef_df$var <- as.factor(coef_df$var)

p <- ggplot(data=coef_df, aes(x=var, y=b, ymin=lb, ymax=ub)) +
  geom_pointrange(color="deepskyblue4",size=.9) +
  coord_flip() +
  labs(x="Luminosity\nat Baseline",
       y="Standard Deviation Change in\nLuminosity Due to Becoming\nClose to a Road (+/- 95% CI)",
       title="Impact of Roads\nOn Nighttime Lights") +
  theme_minimal() +
  theme(plot.title = element_text(size=16,family="Times New Roman",face="bold", hjust = 0.5),
        axis.title.x = element_text(size=14,family="Times New Roman"),
        axis.text.x = element_text(size=12,family="Times New Roman"),
        axis.title.y = element_text(size=14,family="Times New Roman",angle=0, vjust=0.95),
        axis.text.y = element_text(size=13,family="Times New Roman",face="bold.italic"))
p
ggsave(p, filename = file.path(project_file_path, "Figures", "impact_roads_coef.png"),height=6, width=5)
