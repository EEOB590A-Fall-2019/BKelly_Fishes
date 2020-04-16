## Summarize catchment-scale model predictions of 
## occupancy probability for Brook trout and Brown Trout

library(tidyverse)
library(skimr)

#load data
bk <- read.csv("Data/Thesis/Spatial/bkt_aoi_psi.csv", header=T)
br <- read.csv("Data/Thesis/Spatial/brt_aoi_psi.csv", header=T)

#inspect data
names(bk)
names(br)

#subset data
bk2 <- bk %>%
  select(HUC8, estimate, se, lcl, ucl)

br2 <- br %>%
  select(HUC8, estimate, se, lcl, ucl)

#summarize data - brook trout
skim(bk2)

bk3 <- bk2 %>%
  group_by(HUC8) %>%
  summarise(mean_psi = mean(estimate), min_psi = min(estimate), max_psi = max(estimate), sd_psi = sd(estimate),
            mean_lcl = mean(lcl), min_lcl = min(lcl), max_lcl = max(lcl),
            mean_ucl = mean(ucl), min_ucl = min(ucl), max_ucl = max(ucl),
            n())

#summarize data - brown trout
skim(bk2)

bk3 <- bk2 %>%
  group_by(HUC8) %>%
  summarise(mean_psi = mean(estimate), min_psi = min(estimate), max_psi = max(estimate), sd_psi = sd(estimate),
            mean_lcl = mean(lcl), min_lcl = min(lcl), max_lcl = max(lcl),
            mean_ucl = mean(ucl), min_ucl = min(ucl), max_ucl = max(ucl),
            n())
