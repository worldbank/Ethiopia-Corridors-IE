library(foreign)
library(readr)
library(tidyverse)

# John Computer
file.path <- "~/Dropbox/research/2017/ethroads/Ethiopia IE/"

# Rob Personal Computer
file.path <- "~/Dropbox/Ethiopia IE/"

#system.time(
df <- read_csv(paste(file.path, "Data/etre/Research Data/AAE Traffic Research Data/2017 AAE Traffic Research Data.csv",sep=""))
df2 <- readxl::read_excel(paste(file.path, "Data/etre/Research Data/AAE Crush Research Data/2017 AAE Crush  Research Data .xlsx",sep=""))
#)

# # $8.3M/year
# sum(df$FeeFare) / 27

df2$Fatality %>% as.numeric %>% sum(na.rm = T)
df2$`Serious Injury` %>% as.numeric %>% sum(na.rm = T)
df2$`Light Injury` %>% as.numeric %>% sum(na.rm = T)
nrow(df2)

# ACCIDENT LOCATION HISTOGRAM
df2 %>%
  select(`Time of Accident`, `Accident Date`, `Accident Location`, Direction) %>%
  distinct %>%
  rename(al = `Accident Location`, d = Direction) %>%
  subset(grepl("^[0-9]+[+][0-9]+$", al) & grepl("(Addis|Adama)", d)) %>%
  separate(al, c("km", "m"), "[+]") %>%
  mutate_at(c("km", "m"), as.numeric) %>%
  mutate(loc = km + (m / 1000), d = ifelse(grepl("Addis", d), "Addis", "Adama")) %>%
  ggplot(aes(x = loc, fill = d)) +
  geom_histogram(binwidth = 1, position = "dodge")

# ACCIDENT TIME OF DAY HISTOGRAM
# NOTE : I'M MAKING A POTENTIALLY RECKLESS ASSUMPTION ABOUT WHAT DAY/NIGHT MEANS...
df2 %>%
  select(`Time of Accident`, `Accident Date`, `Accident Location`, Direction) %>% distinct %>%
  mutate(ta = `Time of Accident`) %>%
  mutate(ta = ta %>% tolower %>% gsub(pattern = "[^a-z0-9]", replacement = "") %>%
           gsub(pattern = "(time|tme)", replacement = "") %>%
           gsub(pattern = "(nitght|evening)", replacement = "night") %>%
           gsub(pattern = "morning", replacement = "day") %>%
           gsub(pattern = "(\\d)(\\D)", replacement = "\\1_\\2")) %>%
  filter(grepl("_", ta)) %>%
  separate(ta, c("time", "ampm"), "[_]") %>%
  mutate(time = ifelse(nchar(time) == 2, paste0(time, "00"), ifelse(nchar(time) == 3, paste0(0, time), time))) %>%
  mutate(time = ifelse(substr(time, 1, 2) == "12", paste0("00", substr(time, 3, 4)), time)) %>%
  mutate(hour = as.numeric(substr(time, 1, 2)), minute = as.numeric(substr(time, 3, 4))) %>%
  mutate(hour = ifelse(hour >= 6 & ampm == "night" | hour <= 5 & ampm == "day", hour + 12, hour)) %>%
  mutate(t = hour + minute / 60) %>%
  ggplot(aes(x = t)) + geom_histogram(binwidth = 0.5)

#system.time(
df %>%
  mutate(hour = lubridate::floor_date(ENT_OccurTime, unit = "hour")) %>%
  count(hour) %>%
  filter(hour >= strptime("2017-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S")) %>%
  ggplot(aes(x = hour, y = n)) +
  geom_point()
#)

df %>%
  mutate(day = lubridate::floor_date(ENT_OccurTime, unit = "day")) %>%
  count(day) %>%
  filter(day >= strptime("2017-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S")) %>%
  ggplot(aes(x = day, y = n)) +
  geom_point()
