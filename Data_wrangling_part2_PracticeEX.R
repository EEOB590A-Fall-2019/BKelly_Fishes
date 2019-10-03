# EEOB590A
# Data_wrangling part 2 practice exercise
# practice tidying and wrangling 
library("tidyverse")
library("lubridate")
library("readxl")
library("forcats")

#from the tidy folder, read in the partialtidy file for pollination from last week's assignment
poll_long_pt <- read.csv("data/tidy/poll_long_partialtidy.csv")
head(poll_long_pt)

###########################################################
#####Part 1: finish tidying & wrangling dataframe #########

#1) Broad changes to the database
summary(poll_long_pt)
?str
str(poll_long_pt)
#1a) Change the class of each variable as appropriate (i.e. make things into factors or numeric)
# use lubridate to tell R the format of hte date
poll_long_pt$date.traps.out <- ymd(poll_long_pt$date.traps.out)
poll_long_pt$date.traps.coll <- ymd(poll_long_pt$date.traps.coll)
class(poll_long_pt$date.traps.out)
class(poll_long_pt$date.traps.coll)
#use select to choose columns
poll_LPT <- poll_long_pt %>%
  select(2:11)
names(poll_LPT)
  
#2) Fix the errors below within cells 
##2a) Fix the levels of site so that they have consistent names, all in lowercase
#Change levels of a variable. There are a lot of ways to do this. Here is the forcats approach
levels(poll_LPT$site)
poll_LPT <- poll_LPT %>%
  mutate(site = fct_recode(site, "anao" = "Anao", "forbia" = "ForbiA", "forbigrid" = "ForbiGrid",
                           "ladta" = "LADTA", "ladtg" = "LADTG", "mtrl" = "MTRL",
                           "nblas" = "Nblas", "racetrack" = "Racetrack", "riti" = "Riti",
                           "sblas" = "Sblas"))
levels(poll_LPT$site)

##2b) What format are the dates in? Do they look okay? 
#addressed earlier in part 1a, changed class from factor to date using lubridate
class(poll_LPT$date.traps.out)
class(poll_LPT$date.traps.coll)

##2c) Do you see any other errors that should be cleaned up? 
summ <- summary(poll_LPT)
summ
#not that I can see, uniqueID will be slightly different now due to lowercased site names

#3) Create a new column for the duration of time traps were out

#4) Arrange data by the number of insects

#5) Print tidied, wrangled database

#####################################################
####Part 3: start subsetting & summarizing ##########

#6) Make a new dataframe with just the data from Guam at the racetrack site and name accordingly. 

#7) Make a new dataframe with just the uniqueID, island, site, transect, insectorder, numinsects, and duration columns. 

#8) With the full database (not the new ones you created in the two previous steps), summarize data, to get: 
#8a) a table with the total number of insects at each site
#8b) a table that shows the mean number of insects per island
#8c) a table that shows the min and max number of insects per transect

#9a) Figure out which insect order is found across the greatest number of sites

#9b) For that insect order, calculate the mean and sd by site. 