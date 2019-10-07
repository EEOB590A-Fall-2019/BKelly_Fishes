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
head(poll_LPT)

poll_duration <- poll_LPT %>%
  mutate(duration = poll_LPT$date.traps.coll - poll_LPT$date.traps.out) 

names(poll_duration)
head(poll_duration)
#4) Arrange data by the number of insects
poll_duration %>%
  arrange(numinsects)
#5) Print tidied, wrangled database
getwd()
?write.csv
write.csv(poll_duration, "/Users/brettkelly/Documents/MyFiles/Iowa State/EEOB590B/BKelly_Fishes/
          wrangled_pollination.csv", row.names = T)

#####################################################
####Part 3: start subsetting & summarizing ##########

#6) Make a new dataframe with just the data from Guam at the racetrack site and name accordingly. 
levels(poll_duration$island)
levels(poll_duration$site)
Guam_racetrack <- filter(poll_duration, island == "Guam", site == "racetrack")
head(Guam_racetrack)

#7) Make a new dataframe with just the uniqueID, island, site, transect, insectorder, numinsects, and duration columns. 
names(poll_duration)
poll_new <- poll_duration %>%
  select(uniqueID, island, site, transect, insectorder, numinsects, duration)
head(poll_new)

#8) With the full database (not the new ones you created in the two previous steps), summarize data, to get: 
#8a) a table with the total number of insects at each site
Insects <- poll_duration %>%
  group_by(site) %>%
  summarise(SumInsects = sum(numinsects, na.rm = T))
Insects
#8b) a table that shows the mean number of insects per island "per bowl"
names(poll_duration)
InsectAvg <- poll_duration %>%
  group_by(island) %>%
  summarise(AvgInsects = mean(numinsects, na.rm = T))
InsectAvg
#8c) a table that shows the min and max number of insects per transect
min_max <- poll_duration %>%
  group_by(transect) %>%
  summarise(Maximum = max(numinsects, na.rm = T), Minimum = min(numinsects, na.rm = T))
head(min_max)
#9a) Figure out which insect order is found across the greatest number of sites
no_hope <- poll_duration %>%
  mutate(Presence = ifelse(numinsects > 0,"1","0")) 
no_hope$Presence <- as.integer(no_hope$Presence)
class(no_hope$Presence)
summary(no_hope$Presence)

little_hope <- no_hope %>%
  group_by(insectorder) %>%
  summarise(orders = sum(Presence, na.rm = T))
little_hope %>%
  arrange(orders) #Diptera present at most number of sites (n = 223 sites)


#9b) For that insect order, calculate the mean and sd by site. 
dip <- poll_duration %>%
  filter(insectorder == "Diptera")
dipt <- dip %>%
  group_by(site) %>%
  summarise(avg = mean(numinsects, na.rm = T), stdev = sd(numinsects, na.rm = T))
head(dipt)

