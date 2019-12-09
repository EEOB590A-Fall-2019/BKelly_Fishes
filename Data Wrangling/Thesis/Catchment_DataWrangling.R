############################################################
############################################################
## Create Catchment Scale Environmental Covariate Dataset ##
############################################################
############################################################

library(tidyverse)
library(skimr)
library(forcats)

#read data
catch <- read_csv("Data/Thesis/Raw/All Years/Catchment_Attributes.csv", col_names = T)

#Examine structure, correct any data types, look for NA's
names(catch)
#site number to character
catch$Site <- as.character(catch$Site)

#remove unwanted variables, rename variables, calculate density (per watershed area) instead of counts
catch2 <- catch %>%
  select(HUC8, Site, POLY_AREA, Avg_Percen, Count_1, Count_2)%>%
  rename(Area_km2 = POLY_AREA, AvgSlope = Avg_Percen, EnvFac = Count_1, CrossCat = Count_2)%>%
  mutate(EFac_Cat = (EnvFac/Area_km2), Cross_Cat = (CrossCat/Area_km2))%>%
  select(-EnvFac, -CrossCat)

#rename site numbers to include characters
catch2[9,2] <- '57b'
catch2[13,2] <- '14b'
catch2[27,2] <- '78b'
catch2[58,2] <- '118b'
catch2[68,2] <- '32b'
catch2[134,2] <- '28b'
catch2[106,2] <- "97b"


#create "newID" 
#delete columns we will not use for analysis (YEL_33 and UPI_29 because of fishless status)
names(catch2)
catch3 <- catch2 %>%
  unite(newID, c(HUC8, Site), sep = "_", remove = T)%>%
  filter(newID != "YEL_33" & newID != "UPI_29")

catch3[129,1] = "LMAQ_48"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##              Making a DF ready for occupancy analysis               ##
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
brook <- read_csv("Data/Thesis/Tidy/BKT_occDF_RMARK.csv", col_names = T)
names(brook)
brook$freq <- 1
brook <- brook[,c(1,74,2:73)]
names(brook)

brook2 <- brook %>%
  rename(effort1 = t1_eff, effort2 = t2_eff, effort3 = t3_eff, pctpool = pctslow)%>%
  select(1:3, effort1, effort2, effort3, pctex21, pctpool, 
         pctrock, pctBrBnk, pctShade, BRT_100m, HAiFLS_alt, HAiFLS_for)

#Join to new DF
occupancy_input <- full_join(brook2, catch3, by="newID")

write.csv(occupancy_input, "Data/Thesis/Tidy/BKT_Occu_File.csv", row.names = F)




