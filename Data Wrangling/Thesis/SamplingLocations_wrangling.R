#################################################

# Sampling Locations of Streams for CW Inventory

################################################

#We have csv's from ArcMap of the sampled locations in 2018 and 2019
#Want to join them, and have common column titles and subject material
#resulting csv should have HUC8, Site, Xcoord, ycoord

#load libraries
library(tidyverse)
library(forcats)
library(skimr)

getwd()
slocs18 <- read_csv("Data/Thesis/Raw/All Years/Sample_Locs_2018utm.csv", col_names = T)
slocs19 <- read_csv("Data/Thesis/Raw/All Years/Sample_Locs_2019utm.csv", col_names = T)

#trim slocs18 to the columns of interest
names(slocs18)
slocs18 <- slocs18 %>%
  select(Watershed_, SiteNumber, POINT_X, POINT_Y) %>%
  rename(HUC8 = Watershed_, Site = SiteNumber, Xcoord = POINT_X, Ycoord = POINT_Y)
slocs18

#change HUC8 to a factor, then change levels from integers to UPI, YEL, LMAQ
slocs18$HUC8 <- as.factor(slocs18$HUC8)
slocs18 <- slocs18 %>%
  mutate(HUC8 = fct_recode(HUC8, "UPI" = "1", "YEL" = "2", "LMAQ" = "3"))

year18 <- rep(2018, 88)
slocs18 <- slocs18 %>%
  mutate(YrSmpld = year18)%>%
  select(-YearSampld)
#############################################################################################
# 2019
##########


names(slocs19)
slocs19 <- slocs19 %>%
  select(HUC8, Site, POINT_X, POINT_Y, YearCol) %>%
  rename(Xcoord = POINT_X, Ycoord = POINT_Y, YrSmpld = YearCol)
#change from 78b and 118b to 7878 and 118118
slocs19[17,2] <- 7878
slocs19[21,2] <- 118118
slocs19$Site <- as.numeric(slocs19$Site)
#############################################################################################

#############################################################################################

#join data sets and then write tidy csv
#--------------------------------------

#full join
SLOCS <- full_join(slocs18, slocs19, by = NULL)
skim(SLOCS)

#write tidy csv
write.csv(SLOCS, "C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos/Data/Thesis/Tidy/Sampled_LocsUTM_tidy.csv", row.names = F)
