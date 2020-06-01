#Habitat summaries

library(tidyverse)
library(skimr)

#load data
brown <- read_csv("Data/Thesis/Tidy/BRT_RMark.csv", col_names = T)
brook <- read_csv("Data/Thesis/Tidy/BKT_Occu_File.csv", col_names = T)
brook.df <- as.data.frame(brook) %>%
  rename(BrBnk = pctBrBnk) %>%
  select(-pctex21,-MAXT,-RNGT,-EFac_Cat,-ch,-freq)
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


#summary stats for local habitat variables
names(brown)
brt <- brown %>%
  select(newID, avdep, mFlow, pctrun)

local <- left_join(brook.df, brt, by = "newID") %>%
  select(newID, avgT, pctpool, BrBnk, pctrun, pctrock, pctShade, avdep, mFlow)
names(local)

dscrptv <- local %>%
  separate(col = newID, into = c("HUC8","Site"), sep = "_", remove = T) %>%
  group_by(HUC8) %>%
  summarise(mDP = mean(avdep), sdDP = sd(avdep), minDP = min(avdep), maxDP = max(avdep),
            mFL = mean(mFlow), sdFL = sd(mFlow), minFL = min(mFlow), maxFL = max(mFlow),
            mPL = mean(pctpool), sdPL = sd(pctpool), minPL = min(pctpool), maxPL = max(pctpool),
            mRK = mean(pctrock), sdRK = sd(pctrock), minRK = min(pctrock), maxRK = max(pctrock),
            mSH = mean(pctShade), sdSH = sd(pctShade), minSH = min(pctShade), maxSH = max(pctShade))
dscrptv




