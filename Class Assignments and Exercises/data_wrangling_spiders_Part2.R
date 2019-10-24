#EEB698: Data wrangling Part 2

#Use this script to:
  #1) Fix cells within columns
  #3) Subset data (filter, select)
  #4) Summarize data (summarise)
  #5) Group data (group_by)
  #6) Create new columns (mutate)
  #7) Arrange data by the levels of a particular column (arrange)
  #8) Combine datasets (Join)
  #9) Iterate over groups (for loops, purrr)
  #10) Print tidy, wrangled database

source("lib/R_functions/ipak_fx.R") #load ipak function
ipak(c("tidyverse", "lubridate", "readxl", "magrittr"))

##### Step 1) Load tidy data, explore ########
transplant <- read_csv("data/tidy/transplant_tidy.csv")
str(transplant)
summary(transplant)


##### Step 1: Fix cells within columns (e.g. naming, capitalization) #####

summary(transplant) #need to fix capitalization, spelling, whitespace (maybe)

#1.1: change levels within a column so they are lower case.
levels(transplant$island)
transplant$island <- as.factor(tolower(transplant$island))
transplant$site <- as.factor(tolower(transplant$site))
levels(transplant$island)

#1.2: Change levels of a variable. There are a lot of ways to do this. Here are two.

#preferred approach
transplant$island[transplant$island == "gaum"] <- "guam"

#alternative approach
levels(transplant$island) <- gsub("siapan", "saipan", levels(transplant$island))

#1.3: Get rid of ghost levels
levels(transplant$island) # shows you what levels R thinks are part of island
transplant$island <- droplevels(transplant$island) # or
transplant$island <- factor(transplant$island)
levels(transplant$island)

#1.4: Re-order levels within a variable.
transplant$island <- factor(transplant$island, levels = c("saipan", "guam"))
levels(transplant$island)

#1.5: delete a certain # of characters from the values within a vector
#this is saying "keep the 1st-4th elements (start at 1, stop at 4)".
levels(transplant$site)
transplant$site<-as.factor(substr(transplant$site, 1, 4))
levels(transplant$site)

#1.6: Remove trailing whitespace
transplant$site <- as.factor(trimws(transplant$site))

#1.7: Center continuous predictors and make new column for this variable, may help with convergence
transplant$websize_c <- as.numeric(scale(transplant$websize))

###############################3
# 5.3: Deal with dates
#Change date format to standard yyyymmdd format
#helpful site: https://www.r-bloggers.com/date-formats-in-r/
class(transplant$startdate)

#Tell R startdate is a real date in a specific format
transplant$startdate<-as.Date(transplant$startdate, "%d-%b-%Y") #this is a base function

#repeat for end date, using lubridate instead of as.Date
transplant$enddate <- dmy(as.character(transplant$enddate))

#now, can do math on your dates!
transplant$duration <- transplant$enddate - transplant$startdate
transplant$duration <- as.numeric(transplant$duration)
summary(transplant$duration)


##### Step 3) Subset data (filter, select)
#use filter to extract rows according to some category
transplant_guam <- filter(transplant, island == "guam", site == "anao")

transplant_guam <- transplant %>%
  filter(island == "guam")

#use select to choose columns
select(transplant, island, site, websize, duration)

transplant %>%
  select(island, site, websize, duration)

##### Step 4) Summarize data (summarise, count)
#use summarize to compute a table using whatever summary function you want (e.g. mean, length, max, min, sd, var, sum, n_distinct, n, median)
summarise(transplant, avg=mean(websize), numsites= (length(unique(site))))

transplant %>%
  summarise(avg=mean(websize), numsites = length(unique(site)))

#use count to count the number of rows in each group
count(transplant, site)

transplant %>%
  count(site)

##### Step 5) Group data (group_by)
#use group_by to split a dataframe into different groups, then do something to each group

transplant %>%
  group_by(island) %>%
  summarize (avg = mean(websize))

trans_summ <- transplant %>%
  group_by(island, site, netting) %>%
  summarize (avgweb = mean(websize),
            avgduration = mean(duration))

##### Step 6) Create new columns (mutate)
mutate(transplant, webarea = ((websize/2)/100)^2*pi) #assume circle, divide in half to get radius, divide by 100 to get from cm to m, calculate area
transplant$webarea <- ((transplant$websize/2)/100)^2*pi

transplant %>%
  mutate(webarea = ((websize/2)/100)^2*pi) %>%
  head()



##### Step 7) Arrange data by the levels of a particular column (arrange)
#default goes from low to high
arrange(transplant, websize)

transplant %>%
  arrange(websize)

#use desc inside the arrange fx to go from high to low
transplant %>%
  arrange(desc(websize)) %>%
  head()



##### Step 9) Iterate over groups (for loops, purrr)
#We will get to this later.

##### Step 10) Print tidy, wrangled database

write.csv(trans_summ, "data/tidy/transplant_summary.csv", row.names=F)

