##-----------------------------------##
##    Fish data Exploration 2019     ##
##___________________________________##
## we have fish data for all sites 
## sampled in 2019, in wadable 
## streams. We want to summarize the 
## data for use in reports and 
## manuscripts. 
##-----------------------------------##

#libraries
library(tidyverse)
library(skimr)
library(ggplot2)

#load data
getwd()
fish <- read_csv("Data/Thesis/Tidy/tidyfish1.csv", col_names = T)

#skim data
skim(fish) #only missing values are for the 88 sites where we did not account for SLS vs MTS. (2018 data regardless)

#trim to 2019 data
f2 <- fish %>%
  filter(uid > 88) #53 sites with at least 1 fish. UPI_58 and YEL_98 were fishless. so 55 sampled in report, only 53 to summarize

#Let's list species by number of occurrences, and total abundance 

##occurrences
presence <- f2 %>%
  select_at(vars(-contains("_ab")))

occ.sums <- colSums(presence[,4:39])  

presence[53,] <- c(142, "ALL", 9999, occ.sums)

occ.summed <- presence %>%
  filter(uid == 142)

occ.wide <- occ.summed %>%
  select(-c(uid,HUC8,site))%>%
  pivot_longer(cols = 1:35,names_to = "Species", values_to = "Occurrence")%>%
occ.wide$Occurrence <- as.numeric(occ.wide$Occurrence)

##abundances
abundance <- f2%>%
  select_at(vars(contains("_ab")))

ab.sums <- colSums(abundance[,1:37])

abundance[54,] <- ab.sums

ab.wide <- abundance %>%
  filter(uid == 54)%>%
  pivot_longer(cols = 1:35,names_to = "Species",values_to = "Count")



