# 26 September 2019 ####

#data wrangling part 1, practice script
#we will be working with a real insect pan traps dataset that I've amended slightly in order to practice the skills from Tuesday. 

#1) load libraries - you will need tidyverse and readxl
library(tidyverse)
library(readxl)
#2) Read in data
?readxl
polli <- read_xlsx("/Users/brettkelly/Course-Materials/3. Data wrangling/Data_wrangling_day1_pollination.xlsx")
polli
#3) rename columns. Leave insect families with capital letters, but make all other columns lowercase. Remove any spaces. Change "location" to "site". Change "tract" to "transect". 
head(polli)
names(polli)
polli <- polli %>% 
  rename(island = Island, other = Other, 
       site = Location, transect = Tract, partial = Partial, 
       topcolor_bowlcolor = "Top color - Bowl color")
names(polli)
#4) Add missing data. Note that the people who entered the data did not drag down the island or location column to fill every row. 
?fill

polli_fill <- polli %>%
  fill(island,site)
polli_fill
#5) Separate "Top color - Bowl color" into two different columns, with the first letter for the top color and the second letter for the bowl color. We do not need to save the original column. 
?separate
polli_sep <- polli_fill %>%
  separate(col = topcolor_bowlcolor, into = c("topcolor", "bowlcolor"), sep = "-")
names(polli_sep)
#6) Use the complete function to see if we have data for all 3 transects at each location. Do not overwrite the poll dataframe when you do this. 
?complete
PC2 <- polli_fill %>%
  complete(transect, site)

#which transects appear to be missing, and why? 
?is.na()
is.na(PC2$island)
# three transects for various Ladtg/LADTG seem to be missing. The Island name did not fill when previously using the fill function.
# additionally, there are no data entries for those transects, i.e. there are "NAs" for all insect counts. 

#7) Unite island, site, transect into a single column with no spaces or punctuation between each part. Call this column uniqueID. We need to keep the original columns too. 
?unite
polli_unite <- polli_sep %>%
  unite(uniqueID, c(island, site, transect), sep = "", remove = F)
polli_unite$uniqueID
  

#8) Now, make this "wide" dataset into a "long" dataset, with one column for the insect orders, and one column for number of insects. 
?gather
polli_long <- polli_unite %>%
  gather(key = "order", value = "count", 7:19)
polli_long

#9) And just to test it out, make your "long" dataset into a "wide" one and see if anything is different. 
polli_wide <- polli_long %>%
  spread(key = order, value = count)

#are you getting an error? Can you figure out why? 
# Yes, "Error: Each row of output must be identified by a unique combination of keys.
# Keys are shared for 78 rows:"
# I think this is because the values for insect order appear multiple times, and so it tries to make them
# all new columns, by the same name, hence the error. 

#10) Now, join the "InsectData" with the "CollectionDates" tab on the excel worksheet.
# You'll need to read it in, and then play around with the various types of 'mutating joins' (i.e. inner_join, left_join, right_join, full_join),
# to see what each one does to the final dataframe. 

# read in data
collect <- read_excel("/Users/brettkelly/Course-Materials/3. Data wrangling/Data_wrangling_day1_pollination.xlsx", sheet = 2)
# inspect 
colnames(collect) #two columns with names containing spaces

#change column names to remove spaces and reduce words
collectr <- collect %>%
  rename(trap_out = "date traps out", trap_in = "date traps coll")
colnames(collectr)
# inspect
collectr

#Left Join:
leftjoin_coll <- polli_unite %>%
  left_join(collectr, by = c("island", "site"))
head(leftjoin_coll)
dim(leftjoin_coll)
#Appears to work how we expected/wanted! Now each row, will give indication of when the trap was set and removed. 

#Right Join: join matching values from x to y. Return all rows of y, all columns from x and y, but only those from x that match. As above, if multiple matches, all combinations are returned.
rightjoin_coll <- polli_unite %>%
  right_join(collectr, by = c("island", "site"))
dim(rightjoin_coll) #lost obs!

#Inner Join: Join data. Retain only rows from x and y that match, and all columns from both. If multiple matches between x and y, then all combination of matches are returned.
innerjoin_coll <- polli_unite %>%
  inner_join(collectr, by = c("island", "site"))
dim(innerjoin_coll) #lost obs!

#Full Join: Join data. retain all values, all rows from both x and y
?full_join
fulljoin_coll <- polli_unite %>%
  full_join(collectr, polli_unite, by = c("island", "site"))
dim(fulljoin_coll)

# Worked, did not lose any obs. 












