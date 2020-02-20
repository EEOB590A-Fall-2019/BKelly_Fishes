########################################

#Need Map of study area and FIBI scores

#######################################

library(tidyverse)
library(skimr)

#load FIBI data
ibi <- read.csv("Data/Thesis/Tidy/FIBI_tidy2.csv", header=T)
skim(ibi)

#load site attributes data with X,Y coords of sites
locs <- read.csv("Data/Thesis/Spatial/sites_with_HUC12.csv", header = T)
skim(locs)

#trim locs to just unique site identifier and x,y coords (UTM in this case)
names(locs)
loc2 <- locs %>%
  select(newID, Xcoord, Ycoord, HU_12_NAME)
  
#Left join loc2 TO ibi data
final <- left_join(ibi, loc2, by="newID")

#write tidy csv for map making
write.csv(final, "Data/Thesis/Tidy/FIBI_locs.csv", row.names = F)


