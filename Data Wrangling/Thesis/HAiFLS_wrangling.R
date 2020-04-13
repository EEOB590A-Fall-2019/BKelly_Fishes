## Catchment-Scale data from "Study Area Analysis" 
### To be used for occupancy predictions later
#### main goal for this script will be to organize the DF for further analysis

library(tidyverse)
library(skimr)



# load data
pts <- read.csv("Data/Thesis/Spatial/Pts_for_Preds_2020.csv", header=T)

#inspect data
skim(pts)

names(pts)

pt2 <- pts %>%
  select(OBJECTID, POINT_X, POINT_Y, RASTERVALU, CatAr_km2, 15:24, 26:30) %>%
  rename(No_cells = RASTERVALU, Area_km2 = CatAr_km2)

#remove double rows
pt2[2829:2921,9] <- 0
pt2[3606:3630,13] <- 0
pt2[4365:4664,20] <- 0

names(pt2)

pt3 <- pt2 %>%
  mutate(HAiFLS = rowSums(.[6:20], na.rm = T)) %>%
  select(OBJECTID, POINT_X, POINT_Y, No_cells, Area_km2, HAiFLS) %>%
  rename(HAiFLS_for = HAiFLS)

skim(pt3)

write.csv(pt3, "Data/Thesis/Tidy/gis_points.csv", row.names = F)
