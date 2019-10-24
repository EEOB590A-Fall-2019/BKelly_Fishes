#EEBOB590A: Data Wrangling part 1
#Use this script to:
  #1) Load data into R
  #2) Explore data
  #Side Note) learn how to pipe
  #3) fix column names (rename)
  #4) add/remove/split/combine columns (unite,separate)
  #5) reshape (spread, gather)
  #6) change class of columns
  #7) fill in missing data
  #8) join tables
  #9) print tidy database

#load libraries the classic way
library(tidyverse) #loads ggplot2, tibble, tidyr, readr, purrr, dplyr, stringr, forcats
library(readxl) #hadley wickham's package for reading in excel files. not in Tidyverse.

##### Step 1 - Load data.  ############
#There are multiple ways to do this, which impacts your imported dataframe. The key things that vary are how R interprets your blank values & NA's, and how R assigns a class to each column.

#option 1: if a csv or table, use read.csv or read.table.
transplant <- read.csv("data/raw/transplant_raw.csv", na.strings = c("", "NA", "na", " "))

#option 1b:
transplant_csvreadr <- read_csv("data/raw/transplant_raw.csv", na = c("", "NA", "na", " "))

# #option 2: if excel, use readxl(). (note- rest of code works better with read.csv version)
transplant_excel <- read_excel("data/raw/transplant_raw.xlsx", sheet = 1, col_names = T, col_types = NULL) #col_types = null is default, and readxl guesses what each column is. by default, readxl converts blank cells to missing data. use na="yourcode" if you have a code you use for missing values.

##### Step 2: Explore data ##########

#compare dataframes from above to see what happens using read.csv, read_csv, and readxl
str(transplant) # str shows the structure of your dataset
str(transplant_csvreadr) #most variables are read in as characters
str(transplant_excel) # most variables are character class. dates are read in intelligently, though, although it added the current year because year was missing.

summary(transplant) #gives summary information, which can help you identify if any columns are in the wrong class or if there are data entry errors or a lot of NA's etc.
names(transplant) #show column names

##### Side note: Introduction to piping #########
#Each function returns an object, so calls can be chained together in a single statement, without needing variables to store the intermediate results.
# it takes the output of one statement and makes it the input of the next statement. When describing it, you can think of it as a "THEN".

# %>%  is piping function

# the code chunk above will translate to something like "you take the transplant data, then you subset the guam data and then you calculate the meanwebsize".

#Here are four reasons why you should be using pipes in R:

# You'll structure the sequence of your data operations from left to right, as apposed to from inside and out;
# You'll avoid nested function calls;
# You'll minimize the need for local variables and function definitions; And
# You'll make it easy to add steps anywhere in the sequence of operations.

##### Step 3: Standardize/clean inconsistencies in column names, class etc. ##########

# 3.1: Rename column headings
#Use the rename function in the dplyr package
#new name is on left, old name is on right.
transplant <- transplant %>% 
  rename(island = Island, 
         site = Site, 
         web = Web.., 
         native = Native, 
         netting = Netting, 
         startdate = Start.Date, 
         enddate = End.Date, 
         totaldays = Total.Days, 
         spidpres = SpidPres, 
         webpres = WebPres, 
         WebSize = WebSize.cm.)

colnames(transplant) #note that I left one with uppercase letters; I will return to that later

# 3.2: Fix upper case/lower case issues in column names
#change column names from upper to lowercase

transplant <- transplant %>%
  rename_all(tolower)

#To see column names, use colnames or names
colnames(transplant)
names(transplant)

##### Step 4: Add, remove, split, combine columns ############

#4.1: Add a column

#4.1.1: Add column directly
transplant$year <- 2013 #this experiment happened in 2013. Added this in a new column

#4.2: Remove a column
#use "Select" - to select columns from a dataframe to include or drop
transplant <- transplant %>%
  select(-totaldays)

colnames(transplant)

#4.3: Separate one column into two columns
transplant2 <- transplant %>%
  separate(col=web, into=c("web_a", "web_b"), sep="'", remove = F) 
#remove=F tells R to leave the original column

colnames(transplant)

#4.4: Combine two columns into one
transplant$startdate <- as.character(transplant$startdate)

transplant <- transplant %>%
  unite(startdate, c(startdate, year), sep="-", remove=F)

transplant$enddate <- as.character(transplant$enddate)

transplant <- transplant %>%
  unite(enddate, c(enddate, year), sep="-")

##### Step 5: Reshape database (i.e. if wide, change to long; if long, change to wide)  ########################

#5.1: Change data from long to wide format using tidyr::spread()
transplantwide <- transplant %>%
  spread(key = webpres, value = websize) #the info in webpres will be the new column names, and the columns will be populated by the values in websize

#5.2: Change data from wide to long using tidyr::gather()
# Gather moves column names into a key column, gathering the column values into a single value column.
# The arguments to gather(key, value, ..., factor_key):
# key: Name of new key column (make a new name for the column that will contain the names of the column headings in the wide format)
# value: Name of new value column (make a new name for this column that will contain the values currently populating the rows in the wide format)
# ...: Names or column numbers for source columns that contain values (e.g. 10:12)
# factor_key=T: Treat the new key column as a factor (instead of character vector)

transplant_long <- transplantwide %>%
  gather(key = "webpres", value = "websize", no, yes) #this makes a new column called "webpres" populated by "no" or "yes", and then another new column called "websize" populated by the values within the former "no" and "yes" columns. 
#note that we have now added a row with "NA" for every observation - this is undesirable for analysis, but serves to show how to go from wide to long

##### Step 6: Change class of columns ############################
# 6.1: Change class of a single column
transplant$island <- as.factor(transplant$island)

# 6.2: Change class of multiple columns at a time

transplant <- transplant %>%
  mutate_at(vars(island, site, web, native, netting, spidpres), 
                   factor)

######## Step 7: Fill in missing values ##############
#complete function gives you all combinations of the columns you choose. E.g. if you want to make sure you sampled every island on each of every year, and add rows with NA's for data for any island:year combos you missed, use this function. 

transplant_comp <- transplant %>%
  complete(island, site)  #note - this is meaningless, just to show how it works

#The Fill function fills in information from the most recent nonmissing value. This is useful when people enter data and fail to fill in repetitive data (e.g. the first 30 rows are from Site A, but they only write Site once on row 1 and assume it follows for the next 29 rows). 

transplant_comp <- transplant_comp %>%
  fill(web) 

##### Step 8) Combine datasets (Join) ####################
#you have two tables, table x (considered "left" table) and table y (considered "right" table)
preycap <- read_csv("data/tidy/preycap_tidy.csv")

#Left Join: join matching values from y to x. Return all values of x, and all columns from x and y, but only those from y that match. If multiple matches between x and y, then all combinations are returned.
leftjoin_transprey <- transplant %>%
  left_join(preycap, by = c("island", "site"))
#Use by = c("col1", "col2", ...) to specify one or more common columns to match on.

#Right Join: join matching values from x to y. Return all rows of y, all columns from x and y, but only those from x that match. As above, if multiple matches, all combinations are returned.
rightjoin_transprey <- transplant %>%
  right_join(preycap, by = c("island" = "island", "site" = "site"))
#Use a named vector, by = c("col1" = "col2"), to match on columns that have different names in each table. le _join(x, y, by = c("C" = "D"))

#Inner Join: Join data. Retain only rows from x and y that match, and all columns from both. If multiple matches between x and y, then all combination of matches are returned.
innerjoin_transprey <- transplant %>%
  inner_join(preycap, by = c("island", "site"))

#Full Join: Join data. retain all values, all rows from both x and y
fulljoin_transprey <- transplant %>%
  full_join(transplant, preycap, (by = c("island", "site")))

#let's figure out why we have 5502 obs and 18 variables for each of these join types
levels(as.factor(preycap$island))
levels(transplant$island)
levels(as.factor(preycap$site))
levels(transplant$site)

fulljoin_transprey %>%
  group_by(island, site) %>%
  summarize(numrows = n())

preycap %>%
  group_by(island, site) %>%
  summarize(numrows = n())

transplant %>%
  group_by(island, site) %>%
  summarize(numrows = n())

#since there are lots of matches for island and site, you get a row for every combination. i.e. for anao, get 13 rows in transplant each duplicated for 112 rows from preycap, for a total of 1456 rows.


##### Step 9: write csv file with tidy dataset into tidy folder #########

#create tidy database for analysis
write.csv(transplant, "data/tidy/transplant_tidy.csv", row.names=F)
