# Impact of Expressway Expansion
# Ethiopia IE

#for(i in 1:10) gc()
# Setup ------------------------------------------------------------------------
if(Sys.info()[["user"]] == "r521633") project_file_path <- "/home/wb521633/IEs/Ethiopia IE"
if(Sys.info()[["user"]] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE"
if(Sys.info()[["user"]] == "robmarty") project_file_path <- "~/Dropbox/World Bank/IEs/Ethiopia IE"

library(lfe)
library(stargazer)
library(dplyr)
library(rgdal)
library(raster)
library(alluvial)

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
roads_2016 <- readRDS(file.path(project_file_path, "Data", "FinalData", "roads", "RoadNetworkPanelData_1996_2016.Rds"))

speed_vars <- paste0(c("Speed"),1996:2016)

for(var in speed_vars){
  roads_2016[[var]][roads_2016[[var]] == 15] <- 10
  roads_2016[[var]][roads_2016[[var]] == 25] <- 20
  roads_2016[[var]][roads_2016[[var]] == 35] <- 30
}

transitions_df <- summaryBy(LINKLENGTH ~ 
                              Speed1996 + 
                              Speed1997 + 
                              Speed1998 + 
                              Speed1999 + 
                              Speed2000 + 
                              Speed2001 + 
                              Speed2002 + 
                              Speed2003 + 
                              Speed2004 + 
                              Speed2005 + 
                              Speed2006 + 
                              Speed2007 + 
                              Speed2008 + 
                              Speed2009 + 
                              Speed2010 + 
                              Speed2011 + 
                              Speed2012 + 
                              Speed2013 + 
                              Speed2014 + 
                              Speed2015 + 
                              Speed2016, data=roads_2016@data, keep.names = T, FUN=sum)
#transitions_df$improvement <- transitions_df$Speed2016 > transitions_df$Speed1996
#transitions_df$same <- transitions_df$Speed2016 == transitions_df$Speed1996

# Figure -----------------------------------------------------------------------
transitions_df$N <- transitions_df$LINKLENGTH
transitions_df$Speed1996 <- paste0(transitions_df$Speed1996, "_", 1:nrow(transitions_df), "_1") 
transitions_df$Speed1997 <- paste0(transitions_df$Speed1997, "_", 1:nrow(transitions_df), "_2") 
transitions_df$Speed1998 <- paste0(transitions_df$Speed1998, "_", 1:nrow(transitions_df), "_3") 
transitions_df$Speed1999 <- paste0(transitions_df$Speed1999, "_", 1:nrow(transitions_df), "_4") 
transitions_df$Speed2000 <- paste0(transitions_df$Speed2000, "_", 1:nrow(transitions_df), "_5") 
transitions_df$Speed2001 <- paste0(transitions_df$Speed2001, "_", 1:nrow(transitions_df), "_6") 
transitions_df$Speed2002 <- paste0(transitions_df$Speed2002, "_", 1:nrow(transitions_df), "_7") 
transitions_df$Speed2003 <- paste0(transitions_df$Speed2003, "_", 1:nrow(transitions_df), "_8") 
transitions_df$Speed2004 <- paste0(transitions_df$Speed2004, "_", 1:nrow(transitions_df), "_9") 
transitions_df$Speed2005 <- paste0(transitions_df$Speed2005, "_", 1:nrow(transitions_df), "_10") 
transitions_df$Speed2006 <- paste0(transitions_df$Speed2006, "_", 1:nrow(transitions_df), "_11") 
transitions_df$Speed2007 <- paste0(transitions_df$Speed2007, "_", 1:nrow(transitions_df), "_12") 
transitions_df$Speed2008 <- paste0(transitions_df$Speed2008, "_", 1:nrow(transitions_df), "_13") 
transitions_df$Speed2009 <- paste0(transitions_df$Speed2009, "_", 1:nrow(transitions_df), "_14") 
transitions_df$Speed2010 <- paste0(transitions_df$Speed2010, "_", 1:nrow(transitions_df), "_15") 
transitions_df$Speed2011 <- paste0(transitions_df$Speed2011, "_", 1:nrow(transitions_df), "_16") 
transitions_df$Speed2012 <- paste0(transitions_df$Speed2012, "_", 1:nrow(transitions_df), "_17") 
transitions_df$Speed2013 <- paste0(transitions_df$Speed2013, "_", 1:nrow(transitions_df), "_18") 
transitions_df$Speed2014 <- paste0(transitions_df$Speed2014, "_", 1:nrow(transitions_df), "_19") 
transitions_df$Speed2015 <- paste0(transitions_df$Speed2015, "_", 1:nrow(transitions_df), "_20") 
transitions_df$Speed2016 <- paste0(transitions_df$Speed2016, "_", 1:nrow(transitions_df), "_21") 

# Make Riverplot Object --------------------------------------------------------
#### Edges
edges1 <- subset(transitions_df, select=c(Speed1996, Speed1997, Speed1998, Speed1999,
                                          Speed2000, Speed2001, Speed2002, Speed2003,
                                          Speed2004, Speed2005, Speed2006, Speed2007,
                                          Speed2008, Speed2009, Speed2010, Speed2011, 
                                          Speed2012, Speed2013, Speed2014, Speed2015, Speed2016, N))
names(edges1) <- c(paste0("N",1:21), "Value")
edges1$N1

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
    mutate(x=3),
  
  unique(edges1$N4) %>% 
    as.data.frame %>%
    dplyr::rename(ID=".") %>%
    mutate(x=4),
  
  unique(edges1$N5) %>% 
    as.data.frame %>%
    dplyr::rename(ID=".") %>%
    mutate(x=5),
  
  unique(edges1$N6) %>% 
    as.data.frame %>%
    dplyr::rename(ID=".") %>%
    mutate(x=6),
  
  unique(edges1$N7) %>% 
    as.data.frame %>%
    dplyr::rename(ID=".") %>%
    mutate(x=7),
  
  unique(edges1$N8) %>% 
    as.data.frame %>%
    dplyr::rename(ID=".") %>%
    mutate(x=8),
  
  unique(edges1$N9) %>% 
    as.data.frame %>%
    dplyr::rename(ID=".") %>%
    mutate(x=9),
  
  unique(edges1$N10) %>% 
    as.data.frame %>%
    dplyr::rename(ID=".") %>%
    mutate(x=10),
  
  unique(edges1$N11) %>% 
    as.data.frame %>%
    dplyr::rename(ID=".") %>%
    mutate(x=11),
  
  unique(edges1$N12) %>% 
    as.data.frame %>%
    dplyr::rename(ID=".") %>%
    mutate(x=12),
  
  unique(edges1$N13) %>% 
    as.data.frame %>%
    dplyr::rename(ID=".") %>%
    mutate(x=13),
  
  unique(edges1$N14) %>% 
    as.data.frame %>%
    dplyr::rename(ID=".") %>%
    mutate(x=14),
  
  unique(edges1$N15) %>% 
    as.data.frame %>%
    dplyr::rename(ID=".") %>%
    mutate(x=15),
  
  unique(edges1$N16) %>% 
    as.data.frame %>%
    dplyr::rename(ID=".") %>%
    mutate(x=16),
  
  unique(edges1$N17) %>% 
    as.data.frame %>%
    dplyr::rename(ID=".") %>%
    mutate(x=17),
  
  unique(edges1$N18) %>% 
    as.data.frame %>%
    dplyr::rename(ID=".") %>%
    mutate(x=18),
  
  unique(edges1$N19) %>% 
    as.data.frame %>%
    dplyr::rename(ID=".") %>%
    mutate(x=19),
  
  unique(edges1$N20) %>% 
    as.data.frame %>%
    dplyr::rename(ID=".") %>%
    mutate(x=20),
  
  unique(edges1$N21) %>% 
    as.data.frame %>%
    dplyr::rename(ID=".") %>%
    mutate(x=21)
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
  
  if(!grepl("_1\\b|_21\\b", nodes1_i$ID)){
    out <- list(col = color, lty = 0, srt="0", textcol=TEXT_COLOR,nodestyle="point",textpos=NULL)
  }
  
  if(grepl("_21\\b", nodes1_i$ID)){
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

labels[nodes1$x %in% 2:20] <- ""


#### Make Object
edges1a <- edges1[,c(1,2,22)]
edges1b <- edges1[,c(2,3,22)] %>% dplyr::rename(N1=N2) %>% dplyr::rename(N2=N3)
edges1c <- edges1[,c(3,4,22)] %>% dplyr::rename(N1=N3) %>% dplyr::rename(N2=N4)
edges1d <- edges1[,c(4,5,22)] %>% dplyr::rename(N1=N4) %>% dplyr::rename(N2=N5)
edges1e <- edges1[,c(5,6,22)] %>% dplyr::rename(N1=N5) %>% dplyr::rename(N2=N6)
edges1f <- edges1[,c(6,7,22)] %>% dplyr::rename(N1=N6) %>% dplyr::rename(N2=N7)
edges1g <- edges1[,c(7,8,22)] %>% dplyr::rename(N1=N7) %>% dplyr::rename(N2=N8)
edges1h <- edges1[,c(8,9,22)] %>% dplyr::rename(N1=N8) %>% dplyr::rename(N2=N9)
edges1i <- edges1[,c(9,10,22)] %>% dplyr::rename(N1=N9) %>% dplyr::rename(N2=N10)
edges1j <- edges1[,c(10,11,22)] %>% dplyr::rename(N1=N10) %>% dplyr::rename(N2=N11)
edges1k <- edges1[,c(11,12,22)] %>% dplyr::rename(N1=N11) %>% dplyr::rename(N2=N12)
edges1l <- edges1[,c(12,13,22)] %>% dplyr::rename(N1=N12) %>% dplyr::rename(N2=N13)
edges1m <- edges1[,c(13,14,22)] %>% dplyr::rename(N1=N13) %>% dplyr::rename(N2=N14)
edges1n <- edges1[,c(14,15,22)] %>% dplyr::rename(N1=N14) %>% dplyr::rename(N2=N15)
edges1o <- edges1[,c(15,16,22)] %>% dplyr::rename(N1=N15) %>% dplyr::rename(N2=N16)
edges1p <- edges1[,c(16,17,22)] %>% dplyr::rename(N1=N16) %>% dplyr::rename(N2=N17)
edges1q <- edges1[,c(17,18,22)] %>% dplyr::rename(N1=N17) %>% dplyr::rename(N2=N18)
edges1r <- edges1[,c(18,19,22)] %>% dplyr::rename(N1=N18) %>% dplyr::rename(N2=N19)
edges1s <- edges1[,c(19,20,22)] %>% dplyr::rename(N1=N19) %>% dplyr::rename(N2=N20)
edges1t <- edges1[,c(20,21,22)] %>% dplyr::rename(N1=N20) %>% dplyr::rename(N2=N21)
edges1_2 <- bind_rows(edges1a, edges1b, edges1c, edges1d, edges1e, edges1f, edges1g, 
                      edges1h, edges1i, edges1j, edges1k, edges1l, edges1m, edges1n, 
                      edges1o, edges1p, edges1q, edges1r, edges1s, edges1t)

#### Adjust Nodes
nodes1$y[grepl(paste0("70_",1:21,collapse="|"), nodes1$ID)] <- nodes1$y[grepl(paste0("70_",1:21,collapse="|"), nodes1$ID)] - 10
nodes1$y[grepl(paste0("120_",1:21,collapse="|"), nodes1$ID)] <- nodes1$y[grepl(paste0("120_",1:21,collapse="|"), nodes1$ID)] - 50

nodes1$y[grepl(paste0("30_",1:21,collapse="|"), nodes1$ID)] <- nodes1$y[grepl(paste0("30_",1:21,collapse="|"), nodes1$ID)] + 6
nodes1$y[grepl(paste0("10_",1:21,collapse="|"), nodes1$ID)] <- nodes1$y[grepl(paste0("10_",1:21,collapse="|"), nodes1$ID)] - 2

riverplot_obj <- makeRiver(nodes1, edges1_2,
                           node_labels=labels,
                           node_styles=styles)

riverplot(riverplot_obj)

png(file.path(project_file_path,"Results","Figures", "road_speed_transition_alluvial_all.png"),width = 4*480, height = 4*480,res=300, bg=PLOT_BACKGROUND_COLOR)
riverplot(riverplot_obj, nsteps=100, fix.pdf=T, plot_area=0.9, xscale=.9)
text(.025,-.05,"1996",font=2,col=TEXT_COLOR)
text(.56,-.05,"2016",font=2,col=TEXT_COLOR)
text(-.12,-.11, "Note: Width of lines corresponds to road segment length.",
     font=1,cex=.7,adj=0)
dev.off()













