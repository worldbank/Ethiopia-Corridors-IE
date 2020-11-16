# Road Improvement: Sankey Graph

#### Parameters
COLOR_0 <- "darkslategray1"
COLOR_10 <- "dodgerblue1"
COLOR_20 <- "darkorange"
COLOR_30 <- "firebrick1"
COLOR_45 <- "darkolivegreen2"
COLOR_50 <- "forestgreen"
COLOR_70 <- "darkorchid1"
COLOR_120 <- "darkorchid4"
PLOT_BACKGROUND_COLOR <- "white"
TEXT_COLOR = "black"

# Load Data --------------------------------------------------------------------
roads <- readRDS(file.path(data_file_path, "RSDP Roads", "FinalData", "RoadNetworkPanelData_1996_2016.Rds"))

# Aggregate Data ---------------------------------------------------------------

## Simplify Speeds
speed_vars <- paste0(c("Speed"),1996:2016)
for(var in speed_vars){
  roads[[var]][roads[[var]] == 15] <- 10
  roads[[var]][roads[[var]] == 25] <- 20
  roads[[var]][roads[[var]] == 35] <- 30
}

transitions_df <- roads@data %>%
  group_by(Speed1996, Speed2006, Speed2016) %>%
  summarise(LINKLENGTH = sum(LINKLENGTH))

# Prep for Figure --------------------------------------------------------------
transitions_df$N <- transitions_df$LINKLENGTH
transitions_df$Speed1996 <- paste0(transitions_df$Speed1996, "_", 1:nrow(transitions_df), "_1") 
transitions_df$Speed2006 <- paste0(transitions_df$Speed2006, "_", 1:nrow(transitions_df), "_2") 
transitions_df$Speed2016 <- paste0(transitions_df$Speed2016, "_", 1:nrow(transitions_df), "_3") 

# Make Riverplot Object --------------------------------------------------------
#### Edges
edges1 <- subset(transitions_df, select=c(Speed1996, Speed2006, Speed2016, N))
names(edges1) <- c(paste0("N",1:3), "Value")

#### Nodes
nodes1 <- bind_rows(
  unique(edges1$N1) %>% 
    as.data.frame %>%
    dplyr::rename(ID=".") %>%
    mutate(x=1),
  
  unique(edges1$N2) %>% 
    as.data.frame %>%
    dplyr::rename(ID=".") %>%
    mutate(x=2),
  
  unique(edges1$N3) %>% 
    as.data.frame %>%
    dplyr::rename(ID=".") %>%
    mutate(x=3)
)

nodes1$y <- as.numeric(gsub("_.*","",nodes1$ID))

#### Style
styles = lapply(1:nrow(nodes1), function(n) {
  nodes1_i <- nodes1[n,]
  if(grepl("0_", nodes1_i$ID)) color = COLOR_0
  if(grepl("10_", nodes1_i$ID)) color = COLOR_10
  #if(grepl("15_", nodes1_i$ID)) color = COLOR_15
  if(grepl("20_", nodes1_i$ID)) color = COLOR_20
  #if(grepl("25_", nodes1_i$ID)) color = COLOR_25
  if(grepl("30_", nodes1_i$ID)) color = COLOR_30
  #if(grepl("35_", nodes1_i$ID)) color = COLOR_35
  if(grepl("45_", nodes1_i$ID)) color = COLOR_45
  if(grepl("50_", nodes1_i$ID)) color = COLOR_50
  if(grepl("70_", nodes1_i$ID)) color = COLOR_70
  if(grepl("120_", nodes1_i$ID)) color = COLOR_120
  
  #if(grepl("60_", nodes1_i$ID)) color = COLOR_60
  #if(grepl("70_", nodes1_i$ID)) color = COLOR_70
  
  if(grepl("_1\\b", nodes1_i$ID)){
    out <- list(col = color, lty = 0, srt="0", textcol=TEXT_COLOR,nodestyle="point",textpos=2)
  }
  
  if(grepl("_2\\b", nodes1_i$ID)){
    out <- list(col = color, lty = 0, srt="0", textcol="white",nodestyle="point",textpos=NULL)
  }
  
  if(grepl("_3", nodes1_i$ID)){
    out <- list(col = color, lty = 0, srt="0", textcol=TEXT_COLOR,nodestyle="point",textpos=4)
  }
  
  return(out)
})
names(styles) = nodes1$ID

#### Labels
labels <- c(gsub("_.*","",nodes1$ID))
labels[labels == "0"] <- "No Existing\nRoad"
labels[labels == "10"] <- "10/15"
labels[labels == "20"] <- "20/25"
labels[labels == "30"] <- "30/35"

labels[nodes1$x %in% 2] <- ""
labels[2:6] <- ""

#### Make Object
edges1a <- edges1[,c(1,2,4)]
edges1b <- edges1[,c(2,3,4)] %>% dplyr::rename(N1=N2) %>% dplyr::rename(N2=N3)
edges1_2 <- bind_rows(edges1a, edges1b)

#### Adjust Nodes
nodes1$y[nodes1$ID %in% "120_6_3"] <- nodes1$y[nodes1$ID %in% "120_6_3"] - 50
nodes1$y[grepl("70_",nodes1$ID) & grepl("_3|_2",nodes1$ID)] <- nodes1$y[grepl("70_",nodes1$ID) & grepl("_3|_2",nodes1$ID)] - 10

nodes1$x[nodes1$x == 2] <- 1.9

nodes1 <- nodes1 %>% as.data.frame()
edges1_2 <- edges1_2 %>% as.data.frame()
riverplot_obj <- makeRiver(nodes1, edges1_2,
                           node_labels=labels,
                           node_styles=styles)

png(file.path(data_file_path, "RSDP Roads", "Outputs", "figures", "sankey_speed_3years.png"),width = 4*480, height = 4*480,res=300, bg=PLOT_BACKGROUND_COLOR)
riverplot(riverplot_obj, nsteps=100, fix.pdf=T, plot_area=0.9, xscale=.8)
text(.025,-.05,"1996",font=2,col=TEXT_COLOR)
text(.38,-.05,"2006",font=2,col=TEXT_COLOR)
text(.77,-.05,"2016",font=2,col=TEXT_COLOR)
text(-.12,-.12, "Note: Width of lines corresponds to road segment length. Figure shows length of road 
network according to speed limits in 1996, 2006 and 2016.",
     font=1,cex=.7,adj=0)
title("Road Network Improvements\nIncrease in Speed Limit", col.main =TEXT_COLOR, line = -1.2,adj=0,col="white")
dev.off()













