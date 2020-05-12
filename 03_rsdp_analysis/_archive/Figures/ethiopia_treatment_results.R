# Figure Illustrating Treatment Effect on NTL

# Setup ------------------------------------------------------------------------
if(Sys.info()[["user"]] == "WB521633") project_file_path <- "C:/Users/wb521633/Dropbox/World Bank/IEs/Ethiopia IE"
if(Sys.info()[["user"]] == "robmarty") project_file_path <- "~/Dropbox/World Bank/IEs/Ethiopia IE"

library(parallel)
library(pbmcapply)
library(gtools)
library(doBy)

# Load Data --------------------------------------------------------------------
load(file.path(project_file_path, "Data", "IntermediateData", "Data Outputs for Creating 300m Grid", "points.300m.distrd_all.Rda"))
load(file.path(project_file_path, "Data", "IntermediateData", "Data Outputs for Creating 300m Grid", "points.300m.dmspols.Rda"))
load(file.path(project_file_path, "Data", "IntermediateData", "Data Outputs for Creating 300m Grid", "points.300m.globcover.Rda"))

# Time Since Road Built --------------------------------------------------------
points.300m.near_road <- subset(points.300m.distrd_all, select=-c(id))
for(var in names(points.300m.near_road)){
  points.300m.near_road[[var]] <- as.numeric(points.300m.near_road[[var]] < 20)
}
sums <- rowSums(points.300m.near_road)

sums <- as.data.frame(sums)
sums$id <- 1:nrow(sums)

points.300m.near_road <- points.300m.near_road[!(sums$sums %in% c(0,11)),]
points.300m.dmspols <- points.300m.dmspols[!(sums$sums %in% c(0,11)),]
points.300m.globcover <- points.300m.globcover[!(sums$sums %in% c(0,11)),]
sums <- sums[!(sums$sums %in% c(0,11)),]

sums_to_df <- function(sum){
  if(sum == 0) return(as.data.frame(t(rep(NA,11))))
  if(sum == 1) return(as.data.frame(t(seq(from=-20,to=0,by=2))))
  if(sum == 2) return(as.data.frame(t(seq(from=-18,to=2,by=2))))
  if(sum == 3) return(as.data.frame(t(seq(from=-16,to=4,by=2))))
  if(sum == 4) return(as.data.frame(t(seq(from=-14,to=6,by=2))))
  if(sum == 5) return(as.data.frame(t(seq(from=-12,to=8,by=2))))
  if(sum == 6) return(as.data.frame(t(seq(from=-10,to=10,by=2))))
  if(sum == 7) return(as.data.frame(t(seq(from=-8,to=12,by=2))))
  if(sum == 8) return(as.data.frame(t(seq(from=-6,to=14,by=2))))
  if(sum == 9) return(as.data.frame(t(seq(from=-4,to=16,by=2))))
  if(sum == 10) return(as.data.frame(t(seq(from=-2,to=18,by=2))))
  if(sum == 11) return(as.data.frame(t(seq(from=0,to=20,by=2))))
}

points.300m.time.since.road <- pbmclapply(sums$sums, sums_to_df, mc.cores=3) %>% bind_rows
names(points.300m.time.since.road) <- paste0("time_since_road",seq(from=1996,to=2016,by=2))

# Change in NTL Since 1996 -----------------------------------------------------
points.300m.dmspols$dmspols.1996.zhang.change <- points.300m.dmspols$dmspols.1996.zhang - points.300m.dmspols$dmspols.1996.zhang
points.300m.dmspols$dmspols.1998.zhang.change <- points.300m.dmspols$dmspols.1998.zhang - points.300m.dmspols$dmspols.1996.zhang
points.300m.dmspols$dmspols.2000.zhang.change <- points.300m.dmspols$dmspols.2000.zhang - points.300m.dmspols$dmspols.1996.zhang
points.300m.dmspols$dmspols.2002.zhang.change <- points.300m.dmspols$dmspols.2002.zhang - points.300m.dmspols$dmspols.1996.zhang
points.300m.dmspols$dmspols.2004.zhang.change <- points.300m.dmspols$dmspols.2004.zhang - points.300m.dmspols$dmspols.1996.zhang
points.300m.dmspols$dmspols.2006.zhang.change <- points.300m.dmspols$dmspols.2006.zhang - points.300m.dmspols$dmspols.1996.zhang
points.300m.dmspols$dmspols.2008.zhang.change <- points.300m.dmspols$dmspols.2008.zhang - points.300m.dmspols$dmspols.1996.zhang
points.300m.dmspols$dmspols.2010.zhang.change <- points.300m.dmspols$dmspols.2010.zhang - points.300m.dmspols$dmspols.1996.zhang
points.300m.dmspols$dmspols.2012.zhang.change <- points.300m.dmspols$dmspols.2012.zhang - points.300m.dmspols$dmspols.1996.zhang

# Data to Long -----------------------------------------------------------------
data_long <- rbind(
cbind(points.300m.dmspols$id, sums$sums, points.300m.time.since.road$time_since_road1996, points.300m.dmspols$dmspols.1996.zhang, points.300m.dmspols$dmspols.1996.zhang.change, points.300m.dmspols$dmspols.1996.zhang, points.300m.globcover$globcover.1996),
cbind(points.300m.dmspols$id, sums$sums, points.300m.time.since.road$time_since_road1998, points.300m.dmspols$dmspols.1998.zhang, points.300m.dmspols$dmspols.1998.zhang.change, points.300m.dmspols$dmspols.1996.zhang, points.300m.globcover$globcover.1998),
cbind(points.300m.dmspols$id, sums$sums, points.300m.time.since.road$time_since_road2000, points.300m.dmspols$dmspols.2000.zhang, points.300m.dmspols$dmspols.2000.zhang.change, points.300m.dmspols$dmspols.1996.zhang, points.300m.globcover$globcover.2000),
cbind(points.300m.dmspols$id, sums$sums, points.300m.time.since.road$time_since_road2002, points.300m.dmspols$dmspols.2002.zhang, points.300m.dmspols$dmspols.2002.zhang.change, points.300m.dmspols$dmspols.1996.zhang, points.300m.globcover$globcover.2002),
cbind(points.300m.dmspols$id, sums$sums, points.300m.time.since.road$time_since_road2004, points.300m.dmspols$dmspols.2004.zhang, points.300m.dmspols$dmspols.2004.zhang.change, points.300m.dmspols$dmspols.1996.zhang, points.300m.globcover$globcover.2004),
cbind(points.300m.dmspols$id, sums$sums, points.300m.time.since.road$time_since_road2006, points.300m.dmspols$dmspols.2006.zhang, points.300m.dmspols$dmspols.2006.zhang.change, points.300m.dmspols$dmspols.1996.zhang, points.300m.globcover$globcover.2006),
cbind(points.300m.dmspols$id, sums$sums, points.300m.time.since.road$time_since_road2008, points.300m.dmspols$dmspols.2008.zhang, points.300m.dmspols$dmspols.2008.zhang.change, points.300m.dmspols$dmspols.1996.zhang, points.300m.globcover$globcover.2008),
cbind(points.300m.dmspols$id, sums$sums, points.300m.time.since.road$time_since_road2010, points.300m.dmspols$dmspols.2010.zhang, points.300m.dmspols$dmspols.2010.zhang.change, points.300m.dmspols$dmspols.1996.zhang, points.300m.globcover$globcover.2010),
cbind(points.300m.dmspols$id, sums$sums, points.300m.time.since.road$time_since_road2012, points.300m.dmspols$dmspols.2012.zhang, points.300m.dmspols$dmspols.2012.zhang.change, points.300m.dmspols$dmspols.1996.zhang, points.300m.globcover$globcover.2012)
) %>% as.data.frame
names(data_long) <- c("id","sums","time_since_road","dmspols","dmspols_change", "dmspols_initial","globcover")

# Quartiles by Initial NTL -----------------------------------------------------
max(data_long$dmspols_initial)
data_long$dmspols_initial[data_long$dmspols_initial != 0] %>% summary

data_long$dmsp_group <- NA
data_long$dmsp_group[data_long$dmspols_initial == 0] <- 0
data_long$dmsp_group[data_long$dmspols_initial > 0 & data_long$dmspols_initial <= 472] <- 1
data_long$dmsp_group[data_long$dmspols_initial >= 582] <- 2

#### Globcover - Urban
data_long$gc_urban <- as.numeric(data_long$globcover == 190)

# Collapse and Make Plot -------------------------------------------------------
data_long_sub <- data_long[data_long$sums %in% c(4,5,6),]
#data_long_sub <- data_long
data_long_sum <- summaryBy(gc_urban + dmspols + dmspols_change ~ time_since_road + dmsp_group, data=data_long_sub, keep.names=T, FUN=mean)

ggplot() + 
  geom_line(data=data_long_sum[data_long_sum$dmsp_group == 0,], aes(x=time_since_road, y=gc_urban))






data_long


data_long$dmspols


data_long$quantile[data_long$dmspols_initial != 0] <- Hmisc::cut2(data_long$dmspols_initial[data_long$dmspols_initial != 0], g=4)


data_long$quartile <- with(data_long, cut(data_long$dmspols_initial, 
                               breaks=quantile(data_long$dmspols_initial, probs=seq(0,1, by=0.25), na.rm=TRUE), 
                               include.lowest=TRUE))

a <- quantile(data_long$dmspols_quartile, probs=seq(0,1, by=0.25), na.rm=TRUE)

a <- quantile(as.character(as.numeric(data_long$dmspols_quartile)), probs=c(0.25, .5, .75))

library(Hmisc)
Hmisc::cut2(value, g=4)




data_long$dmspols

df <- cbind(points.300m.distrd_all,points.300m.dmspols)
head(df)
df$distance_distrd_all_1996 %>% summary
