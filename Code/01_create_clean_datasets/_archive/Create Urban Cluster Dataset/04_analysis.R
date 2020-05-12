# Identify Clusters of Lights

library(Rfast)
library(lfe)

# Load Data --------------------------------------------------------------------
df_all <- readRDS(file.path(finaldata_file_path, "urban_cluster_dataset", "urban_cluster_data.Rds"))

# Years Since Improved Variables -----------------------------------------------
df_all$near_improved_all <- as.numeric(apply(df_all[,paste0("distance_road_speed_",c(20,25,30,35,45,50,70,120),"_improved")], 1, FUN = min, na.rm=T) <= 0*1000)
df_all$year_improved_all <- df_all$near_improved_all * df_all$year
df_all$year_improved_all[df_all$year_improved_all == 0] <- NA

df_all$near_improved_50above <- as.numeric(apply(df_all[,paste0("distance_road_speed_",c(50,70,120),"_improved")], 1,FUN = min, na.rm=T) <= 0*1000)
df_all$year_improved_50above <- df_all$near_improved_50above * df_all$year
df_all$year_improved_50above[df_all$year_improved_50above == 0] <- NA

df_all$near_improved_below50 <- as.numeric(apply(df_all[,paste0("distance_road_speed_",c(20,25,30,35,45),"_improved")], 1,FUN = min, na.rm=T) <= 0*1000)
df_all$year_improved_below50 <- df_all$near_improved_below50 * df_all$year
df_all$year_improved_below50[df_all$near_improved_below50 == 0] <- NA

df_all <- df_all %>%
  dplyr::group_by(cluster_id) %>%
  dplyr::mutate(year_improved_all = min(year_improved_all, na.rm=T)) %>%
  dplyr::mutate(year_improved_50above = min(year_improved_50above, na.rm=T)) %>%
  dplyr::mutate(year_improved_below50 = min(year_improved_below50, na.rm=T)) %>%
  ungroup()

df_all$year_improved_all[df_all$year_improved_all == Inf] <- NA
df_all$year_improved_50above[df_all$year_improved_50above == Inf] <- NA
df_all$year_improved_below50[df_all$year_improved_below50 == Inf] <- NA

df_all$years_since_improved_all <- df_all$year - df_all$year_improved_all
df_all$years_since_improved_50above <- df_all$year - df_all$year_improved_50above
df_all$years_since_improved_below50 <- df_all$year - df_all$year_improved_below50

# Create Additional Variables --------------------------------------------------
df_all <- df_all %>%
  dplyr::group_by(cluster_id) %>%
  dplyr::mutate(dmspols_1997 = dmspols[year == 1997]) %>%
  ungroup()

# Constant Sample --------------------------------------------------------------
year_improved_begin <- 2002
year_improved_end <- 2008
years_since_improved_begin <- 1997 - year_improved_begin
years_since_improved_end <- 2013 - year_improved_end

df_all_improved_all <- df_all[!is.na(df_all$year_improved_all),]
df_all_improved_all <- df_all_improved_all[df_all_improved_all$year_improved_all >= year_improved_begin & df_all_improved_all$year_improved_all <= year_improved_end,]
df_all_improved_all <- df_all_improved_all[df_all_improved_all$years_since_improved_all >= years_since_improved_begin & df_all_improved_all$years_since_improved_all <= years_since_improved_end,]

df_all_improved_50above <- df_all[!is.na(df_all$year_improved_50above),]
df_all_improved_50above <- df_all_improved_50above[df_all_improved_50above$year_improved_50above >= year_improved_begin & df_all_improved_50above$year_improved_50above <= year_improved_end,]
df_all_improved_50above <- df_all_improved_50above[df_all_improved_50above$years_since_improved_50above >= years_since_improved_begin & df_all_improved_50above$years_since_improved_50above <= years_since_improved_end,]

df_all_improved_below50 <- df_all[!is.na(df_all$year_improved_below50),]
df_all_improved_below50 <- df_all_improved_below50[df_all_improved_below50$year_improved_below50 >= year_improved_begin & df_all_improved_below50$year_improved_below50 <= year_improved_end,]
df_all_improved_below50 <- df_all_improved_below50[df_all_improved_below50$years_since_improved_below50 >= years_since_improved_begin & df_all_improved_below50$years_since_improved_below50 <= years_since_improved_end,]

# Constant Sample --------------------------------------------------------------
lm_confint_tidy <- function(lm){
  lm_confint <- confint(lm) %>% 
    as.data.frame
  names(lm_confint) <- c("p025", "p975")
  lm_confint$b <- (lm_confint$p025 + lm_confint$p975)/2
  lm_confint$variable <- row.names(lm_confint)
  
  lm_confint <- lm_confint[!grepl("cluster_id)|year)|Intercept)", lm_confint$variable),]
  lm_confint$years_since_improved <- gsub(".*)", "", lm_confint$variable) %>% as.numeric
  
  return(lm_confint)
}

lm_anyroad <- lm(dmspols ~ factor(years_since_improved_all) + factor(cluster_id), data=df_all_improved_all) %>% 
  lm_confint_tidy %>%
  mutate(subset = "anyroad")

lm_below50 <- lm(dmspols ~ factor(years_since_improved_below50) + factor(cluster_id), data=df_all_improved_below50) %>% 
  lm_confint_tidy %>%
  mutate(subset = "below50")

lm_50above <- lm(dmspols ~ factor(years_since_improved_50above) + factor(cluster_id), data=df_all_improved_50above) %>% 
  lm_confint_tidy %>%
  mutate(subset = "50above")

lm_all <- bind_rows(lm_anyroad, lm_50above, lm_below50)

ggplot(data=lm_all, aes(x=years_since_improved, y=b, ymin=p025, ymax=p975, group=subset, color=subset)) + 
  geom_line() +
  geom_point() +
  geom_linerange()




