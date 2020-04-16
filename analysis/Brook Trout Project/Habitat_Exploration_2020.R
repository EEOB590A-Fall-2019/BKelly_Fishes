#Habitat summaries

library(tidyverse)
library(skimr)

#load data
brown <- read_csv("Data/Thesis/Tidy/BRT_RMark.csv", col_names = T)
brook <- read_csv("Data/Thesis/Tidy/BKT_Occu_File.csv", col_names = T)
brook.df <- as.data.frame(brook) %>%
  rename(BrBnk = pctBrBnk)
names(brook.df)

bkt <- brook.df %>%
  separate(col = newID, into = c("HUC8","Site"), sep = "_", remove = T) %>%
  select(3:23) %>%
  group_by(HUC8) %>%
  summarise(mean_for = mean(HAiFLS_for), mean_alt = mean(HAiFLS_alt),
            sd_for = sd(HAiFLS_for), sd_alt = sd(HAiFLS_alt),
            mean_slope = mean(AvgSlope), min_slope = min(AvgSlope),
            max_slope = max(AvgSlope), sd_slope = sd(AvgSlope), mean_temp = mean(avgT),
            sd_temp = sd(avgT), mean_area = mean(Area_km2),
            sd_area = sd(Area_km2), min_area = min(Area_km2), max_area = max(Area_km2))
