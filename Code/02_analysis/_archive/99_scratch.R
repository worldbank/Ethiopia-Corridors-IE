
# Load Data --------------------------------------------------------------------
#### ADM
eth_adm3 <- getData('GADM', country='ETH', level=3)

woreda <- ggplot() +
  geom_polygon(data=eth_adm3, aes(x=long, y=lat, group=group), fill=NA, color="black") +
  labs(title="Woreda") +
  theme_void() + 
  theme(plot.title = element_text(hjust=0.5, face="bold")) +
  coord_quickmap()
ggsave(woreda, filename=file.path(figures_file_path, "woreda.png"), height=5, width=5)
