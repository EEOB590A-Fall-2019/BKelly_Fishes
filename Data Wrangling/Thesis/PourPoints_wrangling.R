#############################################################################################
# GIS Data Cleaning: 2018 and 2019 outlet pour points for stream catchement area calculation
#############################################################################################
library(tidyverse)
library(skimr)
library(forcats)


#load in 2018 and 2019 csv's exported from Arcmap
getwd()

points18 <- read_csv("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos/Data/Thesis/Raw/All Years/Pourpoints18.csv", col_names = T)
points19 <- read_csv("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos/Data/Thesis/Raw/All Years/Pourpoints19.csv", col_names = T)

#Explore
str(points18)
names(points18)
names(points19)

#--2018--#
#remove objectid and SGCN columns
points18 <- points18 %>%
  select(-OBJECTID_12, -Sum_SGCN) %>%
  rename(HUC8 = Sum_Waters, Site = Sum_SiteNu)
#make HUC8 and Site factors
points18 <- points18 %>%
  mutate_at(vars(HUC8, Site), factor) 
#Change levels of HUC8 to "UPI", "YEL", and "LMAQ"
class(points18$HUC8)
levels(points18$HUC8)  
points18 <- points18 %>%
  mutate(HUC8 = fct_recode(HUC8, "UPI" = "1", "YEL" = "2", "LMAQ" = "3"))

#Change levels of Site from the 1414 format to "14b"
levels(points18$Site)
points18 <- points18 %>%
  mutate(Site = fct_recode(Site, "14b" = "1414", "28b" = "2828", "32b" = "3232", "57b" = "5757"))
levels(points18$Site)

#write tidy 2018 csv
write.csv(points18,"C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos/Data/Thesis/Tidy/TidyOutlets_2018.csv", row.names = F)


#--2019--#
names(points19)

#we have two "zeros" in the data set for Site Number due to them not being read in with the "b" attached
#let's fix that
#first - UPI_78b
points19[17,3] <- 7878
#second - YEL_118b
points19[21,3] <- 118118

#change HUC8 and Site to factors so we can have "78b" and "118b"
points19 <- points19 %>%
  mutate_at(vars(HUC8, Site), factor)
class(points19$Site)
points19 <- points19 %>%
  mutate(Site = fct_recode(Site, "78b" = "7878", "118b" = "118118"))
levels(points19$Site)

#remove objectid, loggernum, ycoord, xcoord, description
points19 <- points19 %>%
  select(HUC8, Site, POINT_X, POINT_Y)

#write tidy 2019 csv
write.csv(points19,"C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos/Data/Thesis/Tidy/TidyOutlets_2019.csv", row.names = F)


###########################
# Join the two data sets
###########################

Outlets <- full_join(points18, points19)
str(Outlets)

#write tidy csv
write.csv(Outlets,"C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos/Data/Thesis/Tidy/Outlets_tidy.csv", row.names = F)
