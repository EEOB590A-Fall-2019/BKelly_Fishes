#######################################################################################################

# We have temperature data for each stream sampled across 2018 and 2019
# Most streams sampled in 2018 have data for 2019 as well, although we will eventually only use 
# stream temp data for the summer in which fish were collected

# HOBO loggers were set to record every hour 
# comments and notes columns in spreadsheets detail retrieval notes in hopes to discern weird values

######################################################################################################


library(tidyverse)
library(ggplot2)
library(forcats)
library(readxl)
library(lubridate)
library(skimr)

#load in summary data for 2018 - Temp Loggers
getwd()
logger18 <- read_xlsx("C:/Users/bbkelly/Documents/Brook Trout_Brett/Thesis/data/Data 2018/Temperature Monitors/TempLogLocations_2018 (newest).xlsx")

#6 sites sampled in 2018 are missing data for 2018 summer due to time limitations of installations or logger losses (7% or 6/86 samples)

#explore data set - arrange to go through and check for missing obs
logger18 <- logger18 %>%
  arrange(HUC8, Site)
#-----------------------------------------------------------------------------------------------------------------------------------------------
#LMAQ - 7/7 - no missing obs

#UPI -------------------------------------------------------------------------------------------------------------------------------------------
# - Site_84 has two obs: two loggers were accidentally installed, however, one was lost regardless
#   > we will subset the replicate that was lost eventually and save as new

# - Site_154_PineSpringsWMA needs to be added in:
  # > 2018 data was very far down close to confluence with Canoe, and "buried" when retrieved
  # > 2019 data from the same place we shocked IS available, but can we use 2019 data for a site shocked in 2018? 
# Add in a row to represent UPI_154_PineSpringsWMA
names(logger18)
logger18[nrow(logger18)+1,] = list("UPI", "154", "9345", "43.38149", "-91.77626", "4", "30", "2018", "4", 
                                   "1", "2019", "3185", "UPI_154_PineSpringsWMA", "Site 5 location buried", "New logger listed is further upstream: 43.38153, -91.79426 ")

#found missing logger number for UPI 96 == 3211 (written down as such in one of the green "Rite in the Rain" notebooks)
logger18[54,3] = 3211

# UPI is now up to date with no missing obs
#_______________________________________________________________________________________________________________________________________________

#Yellow River

# Site 33 is missing, but it was also fishless, and has no 2018 data. We have 2019 data, in case we want to show DNR that it may be a good reintroduction site
# > we will not add it to this data set here

#Site 36 - West Branch of Bear Creek - Freilinger Property - Missing
# we will add this in now
logger18[nrow(logger18)+1,] = list("YEL", "36", "NA", "43.149513", "-91.39765", "NA", "NA", "NA", "5", 
                                   "31", "2019", "4619", "YEL_36_Freilinger", "Not currently sure if a logger was installed for 2018", "2019 logger was dry and dislodged upon removal")

#Site 41 - owned by melvin meyer - is missing due to sharing data with Sam Franzen @ the NRCS. Need to add that in. 
logger18[nrow(logger18)+1,] = list("YEL", "41", "3188", "43.16297", "-91.64883", "5", "21", "2018", "4", 
                                   "1", "2019", "3188", "YEL_41_Meyer", "None provided", "NA")

#Site 73 - Dale Green / Lon Koenig - is missing due to sharing data with Sam Franzen @ the NRCS. Need to add that in.
logger18[nrow(logger18)+1,] = list("YEL", "73", "4485", "43.13261", "-91.63630", "4", "30", "2018", "4", 
                                   "1", "2019", "4485", "YEL_73_KoenigGreen", "None provided", "NA")

# **Logger info for Site YEL_125 is included in this data set, because that is when the original logger was installed,
# **BUT fish data for YEL_125 was collected in 2019. Use 2019 data instead. subset this obs out later and save as new.

#add in retrieval notes and comments to YEL_129
logger18[63,15] = "New landowner for 2019, lost 2019 logger to streambank destruction"

######################################
# Last looks at summary 2018 Temp Data
######################################

#skim it
skim(logger18)

#looks like Logger18$LoggerNumber_2018 only has 86 unique values despite there being 89 obs. Let's investigate
summary(logger18$LoggerNumber_2018)
str(logger18)

#first off, it is currently a character class variable, needs to be numeric
logger18$LoggerNumber_2018 <- as.numeric(logger18$LoggerNumber_2018)
class(logger18$LoggerNumber_2018)

#re-do summary search
summary(logger18$LoggerNumber_2018)
list18 <- logger18$LoggerNumber_2018
n_unique(list18)

#problem appears to be solved, there are 89 obs, with 4 missing or "NA's" for LoggerNum_18, 
#after changing class to numeric there are 85 obs and 85 unique logger numbers! :)
#the sites with NA's for LoggerNumber_2018 either did not receive a logger in 2018 or it was lost

#would be nice to tally the number of sites with unusable data. Even though some sites may not have an "NA", 
#for LoggerNumber_2018, I know that some sites lost their logger AND we know which one it was. 

#Sites without usable data for 2018:
#-----------------------------------
#From LMAQ: 17, 48
#From UPI: 41 
#From YEL: 36*, 56, 80, 120
#Total: 7/87 or 8% of total obs  (88th site is YEL_33, no 2018 data, however it was fishless and not included here)
#YEL_36* : may actually have data but cannot find record of LoggerNumber_2018

#-----------------------------------------------------------------------------------------------------------------------------------------------
#_______________________________________________________________________________________________________________________________________________

#load in summary data for 2019 - Temp Loggers

#_______________________________________________________________________________________________________________________________________________
#-----------------------------------------------------------------------------------------------------------------------------------------------

logger19 <- read_xlsx("C:/Users/bbkelly/Documents/Brook Trout_Brett/Thesis/data/Data 2019/Temperature/Logger_Locs_2019_updated.xlsx", sheet = 2)

#try to arrange in ascending order of Site grouped by watershed
str(logger19)

#change HUC8 to factor 
logger19$HUC8 <- as.factor(logger19$HUC8)
class(logger19$HUC8)

logger19 <- logger19 %>%
  arrange(HUC8, Site)

#check for missing values, we only have 48 obs, but we sampled more streams than that. 

#Missing obs 1 = LMAQ_009 
#need to add row to the data
names(logger19)
logger19[nrow(logger19)+1,] = list("LMAQ", "9", "4598", "42.8753", "-91.1530", "LMAQ_009_Wilker", "7", "17", "2019", 
                                   "9", "10", "2019", "normal")

#arrange again
logger19 <- logger19 %>%
  arrange(HUC8, Site)
#continue exploring for missing data
skim(logger19)

#currently all 49 obs have unique logger numbers, that is Good!
#looks like UPI is missing obs, let's take a look at those next

#missing value #2: UPI_70 - Clear Creek Trib
#add row to data set
logger19[nrow(logger19)+1,] = list("UPI", "70", "3223", "43.477786", "-91.414734", "UPI_70_ClearCreek", "7", "16", "2019", 
                                   "9", "20", "2019", "normal")
#arrange again
logger19 <- logger19 %>%
  arrange(HUC8, Site)
#UPI now has no missing obs, on to the Yellow River HUC8
#right now YEL has 33 obs, I believe we need three more obs to hit our 53 sampled and usable sites

#Missing obs 3: YEL_125 - Berger/Sommer, Village Headwaters
#add row to data set
logger19[nrow(logger19)+1,] = list("YEL", "125", "8158", "43.287544", "-91.458803", "YEL_125_BergerSommer", "3", "30", "2019", 
                                   "9", "20", "2019", "on bank")

#Missing obs 4: YEL_153 Tieskoetter/Uhlenhake
#add row to data set
logger19[nrow(logger19)+1,] = list("YEL", "153", "9680", "43.146936", "-91.703603", "YEL_153_TieskoetterUhlenhake", "4", "1", "2019", 
                                   "10", "15", "2019", "none provided")
#-----------------------------------------------------------------------------
#let's make sure we have all unique values for logger numbers
str(logger19)
#make loggernum_2019 a numeric class variable
logger19$loggernum_2019 <- as.numeric(logger19$loggernum_2019)
class(logger19$loggernum_2019)
#count n_unique of loggernum_2019
n_unique(logger19$loggernum_2019)
#52 obs and 52 unique values, BUT how many sites have LOST data for 2019? Let's find out

#Sites without usable data for 2019:
#-----------------------------------
#From LMAQ: none
#From UPI:  none
#From YEL: none
#Total: 0/52 or 0% of obs appear to be missing from 2019. Nice!  

#-----------------------------------------------------------------------------
##All Missing Obs should be accounted for in the 2019 temp logger summary data
#_____________________________________________________________________________



##################################################################################################################
# Let's write tidy csv's of the current data, before we move forward and begin to subset, join data sets, etc. 

######
#2018#
######
#write tidy csv
getwd()
?write.csv
write.csv(logger18, "Data/Thesis/Tidy/tidy_temps18.csv", row.names = F)

######
#2019#
######
write.csv(logger19, "Data/Thesis/Tidy/tidy_temps19.csv", row.names = F)


#-------------------------------------------------------------------------------------------------------------------------------------
##############################################################################

## Make a DF with the HUC8, Site, Logger Number, Date, Temperature 

## one for each year of the field season - 2018 and 2019

##############################################################################

# EXAMPLE CODE FROM HALDRE RODGERS **NOT RUN**
##############################################################################
# FIRST: read in all data files
setwd(rawind.wd) #point R to the raw folder

listtxt <- dir(pattern = "*.csv") #pull out all files that include .csv
veg <- NULL
for (f in listtxt) {
  ldff <- map_df(f, ~ read_csv(f, col_names = FALSE, skip = 3))
  ldff$file <- unlist(strsplit(f,split=".",fixed=T))[1]
  veg <- rbind(veg, ldff)
}

#--------------------------
#Second: Pull out merged csv
setwd(raw.wd) #re-set wd to the raw folder, not the individual folder

write.csv(veg, "Finegayan Hobo_lightandtemp_merged.csv", row.names=F) #will put it in raw
###########################################################################################

###########################################################################################
# Now Try with my data 
# 2019 First - because smaller data set
#-----------------------------------------

#have to setwd() to the folder with raw data files
setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/Thesis/data/Data 2019/Temperature/2019 Logger Data/2019 temp data csv/2019 sampled sites")

#make a list of the file names in the raw data folder
list_logger19 <- dir(path = "C:/Users/bbkelly/Documents/Brook Trout_Brett/Thesis/data/Data 2019/Temperature/2019 Logger Data/2019 temp data csv/2019 sampled sites",
                     pattern = "*.csv" )

#create a blank df for later
trial <- NULL

#create the for loop that takes every csv, reads in the data, skips the first two rows, throws away the first column, and adds a column with the file name
for (f in list_logger19) {
  mid <- map_df(f, ~ read_csv(f, col_names = c("worthless", "Date_Time", "Temp_F"), col_types = "ncn", skip = 2))
  mid <- mid %>%
    select(-worthless)
  mid$SN19 <- unlist(strsplit(f, split = "_", fixed = T))[1]
  trial <- rbind(trial, mid)}

#rename df something logical now that we know it WORKED! :)
Temps2019 <- trial

####################################
####################################
# now do the same for the 2018 data
#-----------------------------------

#have to setwd() to the folder with raw data files
setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/Thesis/data/Data 2018/Temperature Monitors/2018 temp data csv")

#make a list of the file names in the raw data folder, pull out all of the csv's
list_logger18 <- dir(pattern = "*.csv" )

#create a blank df for later
df18 <- NULL

#create the for loop that takes every csv, reads in the data, skips the first two rows, throws away the first column, and adds a column with the file name
for (f in list_logger18) {
  md <- map_df(f, ~ read_csv(f, col_names = c("worthless", "Date_Time", "Temp_F"), col_types = "ncn", skip = 5))
  md <- md %>%
    select(-worthless)
  md$SN18 <- unlist(strsplit(f, split = ".", fixed = T))[1]
  df18 <- rbind(df18, md)}

#rename df something logical now that we know it WORKED! :)
Temps2018 <- df18



######################################################################################################
#Let's skim our new data, and then if how we want, write a csv and back up online as a backup for now!
######################################################################################################

skim(Temps2018) #only three missing obs of $Temp_F out of 501028

skim(Temps2019) #no missing obs!

LMAQ_28b <- Temps2018 %>%
  filter(SN18 == 4613)
hist(LMAQ_28b$Temp_F)
summary(LMAQ_28b$Temp_F)
#Later we will need to check to make sure Temperature is at constant units across sites, Celcius for my work

#For now let's write csv's of both and upload them to google drive as back-ups!

#setwd() to tidy folder
setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos")

write.csv(Temps2018, "Data/Thesis/Tidy/TemperatureData_2018Sites.csv", row.names=F) 

write.csv(Temps2019, "Data/Thesis/Tidy/TemperatureData_2019Sites.csv", row.names=F) 




#########################################################################################################################################################################

# Further data wrangling with the now raw temperature data

#Things to do:
# > make sure all temps are in degrees C
# > look for any outliers or red flags
# > deal with the DateTime column, need to make two columns, one with date and one with time - FINISHED 10/23/2019
# > find first date ALL loggers are in, and last summer date and trim temps into new df
# > join df with HUC8 and Site information - FINISHED 10/23/2019
# > create new df with summary variables for summer (c(maximum daily mean (MEANT), maximum daily maximum (MAXT), maximum daily temp range (RNGT)) -- Wehrly et al. 2007,
# > other potential temp variables: mean daily mean, mean, sd, number of days where daily mean exceeds 21 C, or proportion of obs above 21 C
# > plot maxes vs date
#_________________________________________________________________________________________________________________________________________________________________________

############################
############################
#     2018 Data
############################
############################

#-------------------------------------------------------------------------------------------------
#Let's start by joining two dfs to create a new df with the HUC8 and Site associated with each obs
#-------------------------------------------------------------------------------------------------

#**Here I will use an existing DF in my environment, in the future will need to read in the "tidy_temps18/19.csv's!**
names(logger18)

#new df with just: HUC8, Site, and LoggerNumber_2018 (rename to "SN18" for serial number 2018)
log18 <- logger18 %>%
  select(HUC8, Site, LoggerNumber_2018) %>%
  rename(SN18 = LoggerNumber_2018)
head(log18)

summary(log18) #4 expected NA's in SN18 - either sites had no logger or that info was lost with the logger

##########################################################################################
#join so that "Temps2018" has HUC8 and Site associated with the existing obs
#end result should have 5 variables (as opposed to 3 before)
###########################################################################################

#----------------------------
#before we join to need to get rid of two logger's obs from Temps2018
#3187_nohome 
#4616_nohome
lst.3187 <- list(Temps2018$SN18[Temps2018$SN18 == "3187_nohome"]) #2110 obs
lst.4616 <- list(Temps2018$SN18[Temps2018$SN18 == "4616_nohome"]) #2112 obs
501028 - (2110+2112) #= 496806 obs once those two loggers are removed
tmps2018 <- Temps2018 %>%
  filter(SN18 != "3187_nohome", SN18 != "4616_nohome") #Worked!
#----------------------------
#need class to match so we can join
class(Temps2018$SN18) #class = char
class(log18$SN18) #class = numeric

#Change both to factor()
tmps2018$SN18 <- as.character(tmps2018$SN18)
class(tmps2018$SN18)
log18$SN18 <- as.character(log18$SN18)
class(log18$SN18)

#levels(log18$SN18)
#levels(tmps2018$SN18)
#----------------------------

tmp18_joined <- left_join(tmps2018, log18, by = "SN18") #new joined df with all obs and 5 vars


#make SN18 a factor... again.So we can make sure we have the correct number of levels of SN18 
#tmp18_joined$SN18 <- as.factor(tmp18_joined$SN18)
#class(tmp18_joined$SN18)
#levels(tmp18_joined$SN18)

#explore new dataset
str(tmp18_joined)
skim(tmp18_joined)

#change Site and HUC8 to factor
tmp18_joined$Site <- as.factor(tmp18_joined$Site)
levels(tmp18_joined$Site)
tmp18_joined$HUC8 <- as.factor(tmp18_joined$HUC8) 
levels(tmp18_joined$HUC8)

#check for unique levels 
n_unique(tmp18_joined$SN18)
n_unique(tmp18_joined$Site)

#3 missing obs for logger number 20351163
missing <- tmp18_joined %>%
  filter(SN18 == '20351163')
mean(missing$Temp_F)

#-------------------------------------------------
#81 unique values, here is what is missing: 

#category 1: Logger Lost
# > Yel_56_MoonValley
# > Yel_80_Dousman
# > Yel_120_Goettler
# > UPI_41_Easler
# > UPI_84_Humpal *repeat*
# > UPI_108_Tietz *logger broke on install - quit logging on date of sample*

#category 2: Logger not Installed in 2018
# > Yel_33_Barr *fishless*
# > Yel_36_Freilinger *???*
# > LMAQ_17_USFWS *sampled in Sept.*
# > LMAQ_48_Koopman *sampled in Sept.*
#--------------------------------------------------

# Moving on! 

#We need to deal with the Date_Time column

#first, we need to split the column into two: Date, and Time
names(tmp18_joined)
datetime <- tmp18_joined %>%
  separate(col=Date_Time, into=c("Date", "Time"), sep=" ", remove = F)
#remove = TRUE means the og column will be dropped
#we have some complications with loggers recording time in a different format - some include AM some PM, some on military time
head(datetime)

#but, in total, the separate worked, now lets deal with the classes

#Combine two columns into one
datetime <- datetime %>%
  unite(DateTime, c(Date, Time), sep=" ", remove=F)

mdy_hms(datetime$DateTime) #convert the character to an actual date-time
class(datetime$DateTime)
#now lets drop the old variable
tmp18_joined <- datetime %>%
  select(-Date_Time)
str(tmp18_joined)
#need to change class of DateTime and Date with lubridate
mdy_hms(tmp18_joined$DateTime)
mdy(tmp18_joined$Date)
class(tmp18_joined$Date)

#let's see if it worked, try and plot a tiny portion of data set
#x axis we want as DateTime
#y axis we want temperature
g <- head(tmp18_joined)
ggplot(g, aes(DateTime, Temp_F))+
  geom_point()
#it worked! cool :)

########################################################################

# Time to now make sure all of the temperatures are in Celcius! 

########################################################################
t <- tmp18_joined %>%
  group_by(SN18) %>%
  summarise(avg = mean(Temp_F, na.rm = T), Min = min(Temp_F, na.rm = T), Max = max(Temp_F, na.rm = T))
t
hist(t$avg) #wow, data distribution is actually normal
#logger 4613 appears to be in Celcius already

SN4613 <- tmp18_joined %>%
  filter(SN18 == '4613') %>%
  mutate(Temp_F2 = Temp_F*(9/5)+32)
max(SN4613$Temp_F)
max(SN4613$Temp_F2)

tmp18_joined[384690:390935,4] <- SN4613$Temp_F2
ppp <- tmp18_joined[384690:390935,4]
str(ppp)
summary(ppp)
#now ALL obs are in F
#ultimately not what we want - but much easier to mutate an entire column at one time :)

#------------------------------------------------------------------------------------------
#reorder column order
names(tmp18_joined)
tmp18_joined <- tmp18_joined[,c(6,7,5,1,2,3,4)]
summary(tmp18_joined$Temp_F)
hist(tmp18_joined$Temp_F)
#------------------------------------------------------------------------------------------

#Add "Temp_C" column for degrees Celcius
tmp18C <- tmp18_joined %>%
  mutate(Temp_C = (Temp_F-32)*5/9)
summary(tmp18C$Temp_C)

#Okay, we are now ready to trim the data to a date range from the first day all loggers are in - to the last day of summer
# Beginning Date 8/17/2018
# Ending Date-Time = 9/22/2018

tmp18C$Date <- mdy(tmp18C$Date)

tmp18trim <- tmp18C %>%
  filter(Date > "2018-08-16", Date < "2018-09-23")
#worked!

skim(tmp18trim)
n_unique(tmp18trim$SN18)

plot(tmp18trim$Date, tmp18trim$Temp_C)

#test if further trimmed data is significantly different
temp18trimm <- tmp18C %>%
  filter(Date > "2018-08-16", Date < "2018-09-12")

#t.test
t.test(tmp18trim$Temp_C, temp18trimm$Temp_C)


################################################################
################################################################
## Quality Control of warm temps - combined with retrieval notes
################################################################

hot <- tmp18trim[which(tmp18trim$Temp_C > 26),]

#YEL_004 -- found floating
YEL_004 <- tmp18trim[which(tmp18trim$SN18=="3201"),]
YEL_004_all <- tmp18C[which(tmp18C$SN18=="3201"),]
str(YEL_004_all)
line4 <- mean(YEL_004$Temp_C)

ggplot(YEL_004, aes(x=Date, y=Temp_C))+
  geom_line() #subset data to not include anything past 09/03/2018
ggplot(YEL_004_all, aes(x=Date, y=Temp_C))+
  geom_line()+
  scale_x_date(limits = as.Date(c("2018-06-14","2018-09-22")))+
  geom_hline(yintercept = line4, color="blue", linetype="dashed", size=1) ## 


oop <- tmp18trim[-c(11977:12432),]
71040-70584

#YEL_017 -- Dry
YEL_017 <- tmp18trim[which(tmp18trim$SN18=="3208"),]
YEL_017_all <- tmp18C[which(tmp18C$SN18=="3208"),]
str(YEL_017_all)

ggplot(YEL_017, aes(x=Date, y=Temp_C))+
  geom_line() #
ggplot(YEL_017_all, aes(x=Date, y=Temp_C))+
  geom_line()+
  scale_x_date(limits = as.Date(c("2018-05-20","2018-09-22"))) ## observed values are legit

#YEL_065 -- Dry & buried!
YEL_065 <- tmp18trim[which(tmp18trim$SN18=="3216"),]
YEL_065_all <- tmp18C[which(tmp18C$SN18=="3216"),]
str(YEL_065_all)
line <- mean(YEL_065$Temp_C)

ggplot(YEL_065, aes(x=Date, y=Temp_C))+
  geom_line() #
ggplot(YEL_065_all, aes(x=Date, y=Temp_C))+
  geom_line()+
  scale_x_date(limits = as.Date(c("2018-07-06","2018-09-22")))+
  geom_hline(yintercept = line, color="blue", linetype="dashed", size=1) ## observed values are legit

#YEL_113 -- partially submerged
YEL_113 <- tmp18trim[which(tmp18trim$SN18=="3218"),]
YEL_113_all <- tmp18C[which(tmp18C$SN18=="3218"),]
line113 <- mean(YEL_113$Temp_C)

ggplot(YEL_113, aes(x=Date, y=Temp_C))+
  geom_line() #
ggplot(YEL_113_all, aes(x=Date, y=Temp_C))+
  geom_line()+
  scale_x_date(limits = as.Date(c("2018-05-20","2018-09-22")))+
  scale_y_continuous(limits = c(0,35))+
  geom_hline(yintercept = line113, color="blue", linetype="dashed", size=1) ## observed values are legit

#UPI_57b -- floating
UPI_57b <- tmp18trim[which(tmp18trim$SN18=="3222"),]
UPI_57b_all <- tmp18C[which(tmp18C$SN18=="3222"),]
line57b <- mean(UPI_57b$Temp_C)

ggplot(UPI_57b, aes(x=Date, y=Temp_C))+
  geom_line() #
ggplot(UPI_57b_all, aes(x=Date, y=Temp_C))+
  geom_line()+
  scale_x_date(limits = as.Date(c("2018-06-12","2018-09-22")))+
  scale_y_continuous(limits = c(0,35))+
  geom_hline(yintercept = line57b, color="blue", linetype="dashed", size=1) 

## August 25th through 27th are suspect, but could potentially be possible from large rain event. 
## the river gauge on the UPI at Bluffton (downstream of site) spiked significantly on Aug. 28th
## in discharge and height -- from ~200 cfs to 1200 csf and 3.5 ft to 5.25 ft. Overall, 
## the small date range and times that the temps exceed 30 degrees C are minimal. 
## leave the values due to uncertainty.

#UPI_26 -- floating
UPI_26 <- tmp18trim[which(tmp18trim$SN18=="4606"),]
UPI_26_all <- tmp18C[which(tmp18C$SN18=="4606"),]
line26 <- mean(UPI_26$Temp_C)

ggplot(UPI_26, aes(x=Date, y=Temp_C))+
  geom_line()+
  geom_hline(yintercept = line26, color="blue", linetype="dashed", size=1) #
ggplot(UPI_26_all, aes(x=Date, y=Temp_C))+
  geom_line()+
  scale_x_date(limits = as.Date(c("2018-05-21","2018-09-22")))+
  scale_y_continuous(limits = c(0,35))+
  geom_hline(yintercept = line26, color="blue", linetype="dashed", size=1)

## high variability and temps begin in late july and extend till late August but follow diel-ish patters. 
## typically warmest ~ 2:40 pm. Leave the values due to uncertainty.

#YEL_82 -- buried
YEL_82 <- tmp18trim[which(tmp18trim$SN18=="4618"),]
YEL_82_all <- tmp18C[which(tmp18C$SN18=="4618"),]
line82 <- mean(YEL_82$Temp_C)

ggplot(YEL_82, aes(x=Date, y=Temp_C))+
  geom_line()+
  geom_hline(yintercept = line82, color="blue", linetype="dashed", size=1) #
ggplot(YEL_82_all, aes(x=Date, y=Temp_C))+
  geom_line()+
  scale_x_date(limits = as.Date(c("2018-07-18","2018-09-22")))+
  scale_y_continuous(limits = c(0,35))+
  geom_hline(yintercept = line82, color="blue", linetype="dashed", size=1)

## remove dates 08/17-08/19 due to very abnormal temp values -- coincides with removal by owner*

oop2 <- oop[-c(57265:57336),]
70584-70512

tmp18trim <- oop2
#################################################################
# Make summary dataset for occupancy covariates
# Variables of interest:
# 1.) avgT = 38d summer mean
# 2.) sd = std dev of 38d summer temps
# 3.) pctex21 = proportion of obs that exceed 21 degrees C
# 4.) MEANT = maximum daily mean
# 5.) MAXT = maximum daily maximum
# 6.) RNGT = maximum daily temp range
################################################################

#new df with mean, sd, and max
tmpvars18 <- tmp18trim %>%
  group_by(SN18) %>%
  summarise(avgT = mean(Temp_C, na.rm = T), sdT = sd(Temp_C, na.rm = T), MAXT = max(Temp_C, na.rm = T))

#new df with MEANT and RNGT
tmp18trim$Date <- ymd(tmp18trim$Date)
tmpvars18_2 <- tmp18trim %>%
  group_by(SN18, Date) %>%
  summarise(dailymean = mean(Temp_C), dailyrng = (max(Temp_C))-(min(Temp_C))) %>%
  group_by(SN18) %>%
  summarise(MEANT = max(dailymean), RNGT = max(dailyrng))

#mutate new variable pctex21 to be the (num obs > 21)/(num obs)*100
tmpvars18_3 <- tmp18trim %>%
  group_by(SN18) %>%
  summarise(obsex21 = sum(Temp_C > 21), obs = length(Temp_C)) %>%
  mutate(pctex21 = (obsex21)/(obs)*100)

#join all variables together
tmpvars18 <- left_join(tmpvars18, tmpvars18_2, by = "SN18")
tmpvars18 <- left_join(tmpvars18, tmpvars18_3, by = "SN18")
summary(tmpvars18)

#add HUC8 and Site info to variable df

#extract HUC8 and Site from elsewhere
loggerinfo18 <- logger18 %>%
  select(HUC8, Site, LoggerNumber_2018) %>%
  rename(SN18 = LoggerNumber_2018)
str(loggerinfo18)
loggerinfo18$SN18 <- as.character(loggerinfo18$SN18)

tmpvars18 <- left_join(tmpvars18, loggerinfo18, by = "SN18")

tmpvars18_tidy <- tmpvars18 %>%
  select(-obs, -obsex21)

#rearrange columns to be more logical
names(tmpvars18_tidy)
tmpvars18_tidy <- tmpvars18_tidy[,c(8,9,1:7)]  

#one last check to make sure the data set is correct
skim(tmpvars18_tidy)

#Let's write some csv's for 2018 at various stages of what we have done here:

# First: merged raw data without the two "nohome" sites
getwd()
setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos")
write.csv(tmps2018, "Data/Thesis/Tidy/TempData_2018Sites_exclude_nohome.csv", row.names = F)

# 2nd: data set with Date and Time separated and Celcius
write.csv(tmp18C, "Data/Thesis/Tidy/TempData18_DateTimeCelcius.csv", row.names = F)

# 3rd: temps trimmed to common dates 
write.csv(tmp18trim, "Data/Thesis/Tidy/TempData_2018_Trimmed_AugSept.csv", row.names = F)

# 4th and final: tidy temp covariates
write.csv(tmpvars18_tidy, "Data/Thesis/Tidy/TempVars_2018_tidy.csv", row.names = F)



###################################
#          2019 Data              #
###################################

#-------------------------------------------------------------------------------------------------
#Let's start by joining two dfs to create a new df with the HUC8 and Site associated with each obs
#-------------------------------------------------------------------------------------------------
##########################################################################################
#join so that "Temps2019" has HUC8 and Site associated with the existing obs
#end result should have 5 variables (as opposed to 3 before)
###########################################################################################

#**Here I will use an existing DF in my environment, in the future will need to read in the "tidy_loggers/19.csv's!**
names(logger19)

#new df with just: HUC8, Site, and loggernum_2019 (rename to "SN19" for serial number 2019)
log19 <- logger19 %>%
  select(HUC8, Site, loggernum_2019) %>%
  rename(SN19 = loggernum_2019)
head(log19)

summary(log19) #no missing values, but let's make SN19 a factor
#log19$SN19 <- as.factor(log19$SN19)
#levels(log19$SN19)
#check distinct values, should be 52
#n_distinct(log19$SN19) #52 

#remove 4620 (UPI_58_Korsness)
#remove 4590 (Jack Knight on Penny Springs)
tmps2019 <- Temps2019 %>%
  filter(SN19 != "4590", SN19 != "4620")
str(tmps2019)
#n_distinct(Temps2019$SN19) #appear to be 2 ghost levels for the levels we removed
#tmps2019$SN19 <- factor(tmps2019$SN19)
#levels(tmps2019$SN19) #fixed now

#join
class(tmps2019$SN19)
class(log19$SN19)
log19$SN19 <- as.character(log19$SN19)
#n_distinct(tmps2019$SN19)

tmp19_join <- left_join(tmps2019, log19, by = "SN19") 

#explore new dataset
summary(tmp19_join)
skim(tmp19_join)

#check for unique levels 
#n_unique(tmp19_join$SN19) #52

#################################################################
#We need to deal with the Date_Time column

#first, we need to split the column into two: Date, and Time
names(tmp19_join)
tmp19_join <- tmp19_join %>%
  separate(col=Date_Time, into=c("Date", "Time"), sep=" ", remove = F)
#remove = TRUE means the og column will be dropped

#now lets deal with the classes
#Combine two columns back into one now that the "AM/PM" has been dropped
tmp19_join <- tmp19_join %>%
  unite(DateTime, c(Date, Time), sep=" ", remove=F)
tmp19_join$DateTime <- mdy_hms(tmp19_join$DateTime) #convert the character to an actual date-time
class(tmp19_join$DateTime)
skim(tmp19_join) #worked

#now lets drop the old variable
tmp19_join <- tmp19_join %>%
  select(-Date_Time)
head(tmp19_join)

#need to change class of Date with lubridate
tmp19_join$Date <- mdy(tmp19_join$Date)
class(tmp19_join$Date)

#let's see if it worked, try and plot a tiny portion of data set
#x axis we want as Date
#y axis we want temperature
g <- head(tmp19_join)
ggplot(g, aes(Date, Temp_F))+
  geom_point()
ggplot(g, aes(DateTime, Temp_F))+
  geom_point()

########################################################################

# Time to now make sure all of the temperatures are in Celcius! 

########################################################################
t <- tmp19_join %>%
  group_by(SN19) %>%
  summarise(avg = mean(Temp_F, na.rm = T), Min = min(Temp_F, na.rm = T), Max = max(Temp_F, na.rm = T))
hist(t$avg) #data heavy right skewed, due to 1-2 loggers recording in C

#logger 8158 appears to be in Celcius 
SN8158 <- tmp19_join %>%
  filter(SN19 == '8158') %>%
  mutate(Temp_F = Temp_F*(9/5)+32)

#replace obs in C with new vector of obs in F
tmp19_join[111976:115117,] <- SN8158

hist(tmp19_join$Temp_F)
summary(tmp19_join$Temp_F)
cold <- tmp19_join[which(tmp19_join$Temp_F<30),] #legit
#now ALL obs are in F
#ultimately not what we want - but much easier to mutate an entire column at one time :)

#------------------------------------------------------------------------------------------
#reorder column order
names(tmp19_join)
tmp19_join <- tmp19_join[,c(6,7,5,1,3,2,4)]

#Add "Temp_C" column for degrees Celcius
tmp19C <- tmp19_join %>%
  mutate(Temp_C = (Temp_F-32)*5/9)
summary(tmp19C$Temp_C)

getwd()
write.csv(tmp19C, "Data/Thesis/Tidy/TempData2019_DateTimeCelcius.csv", row.names = F)
#------------------------------------------------------------------------------------------

#Okay, we are now ready to trim the data to a date range from the first day all loggers are in - to the last day of summer
# Beginning Date: 08/01/2019
# Ending Date: 09/11/2019 --- first day that a logger was removed

class(tmp19C$Date)

tmp19trim <- tmp19C %>%
  filter(Date > "2019-07-31", Date < "2019-09-10")
tmp19trim

hot_2019 <- tmp19trim[which(tmp19trim$Temp_C>26),] 

# after accounting for ghost levels, two logger's worth of obs are completely gone
#Missing: 8158
#due to the logger breaking apparently on 7/24/2019

#average temp 
mean(tmp19trim$Temp_C) #15.648
summary(tmp19trim)
skim(tmp19trim)

#trim to equal time ranges from 2018
temp19trimm <- tmp19C %>%
  filter(Date > "2019-08-16", Date < "2019-09-12")
temp19trimm

#average temp for 2nd trim
mean(temp19trimm$Temp_C) #15.178

t.test(tmp19trim$Temp_C,temp19trimm$Temp_C)


write.csv(tmp19trim, "Data/Thesis/Tidy/TempData2019_Trim_AugSept_no8158.csv", row.names=F)

#################################################################
# Make summary dataset for occupancy covariates
# Variables of interest:
# 1.) avgT = 38d summer mean
# 2.) sd = std dev of 38d summer temps
# 3.) pctex21 = proportion of obs that exceed 21 degrees C
# 4.) MEANT = maximum daily mean
# 5.) MAXT = maximum daily maximum
# 6.) RNGT = maximum daily temp range
################################################################

#new df with mean, sd, and max
tmpvars19_1 <- tmp19trim %>%
  group_by(SN19) %>%
  summarise(avgT = mean(Temp_C, na.rm = T), sdT = sd(Temp_C, na.rm = T), MAXT = max(Temp_C, na.rm = T))

#new df with MEANT and RNGT
class(tmp19trim$Date)
tmpvars19_2 <- tmp19trim %>%
  group_by(SN19, Date) %>%
  summarise(dailymean = mean(Temp_C), dailyrng = (max(Temp_C))-(min(Temp_C))) %>%
  group_by(SN19) %>%
  summarise(MEANT = max(dailymean), RNGT = max(dailyrng))

#mutate new variable pctex21 to be the (num obs > 21)/(num obs)*100
tmpvars18_3 <- tmp19trim %>%
  group_by(SN19) %>%
  summarise(obsex21 = sum(Temp_C > 21), obs = length(Temp_C)) %>%
  mutate(pctex21 = (obsex21)/(obs)*100)

#join all variables together
tmpvars19 <- left_join(tmpvars19_1, tmpvars19_2, by = "SN19")
tmpvars19 <- left_join(tmpvars19, tmpvars18_3, by = "SN19")
summary(tmpvars19)

#add HUC8 and Site info to variable df

#extract HUC8 and Site from elsewhere
loggerinfo19 <- logger19 %>%
  select(HUC8, Site, loggernum_2019) %>%
  rename(SN19 = loggernum_2019)
str(loggerinfo19)
loggerinfo19$SN19 <- as.character(loggerinfo19$SN19)

#join the HUC8 and Site info
tmpvars19 <- left_join(tmpvars19, loggerinfo19, by = "SN19")

#drop unwanted columns
tmpvars19_tidy <- tmpvars19 %>%
  select(-obs, -obsex21)

#rearrange columns to be more logical
names(tmpvars19_tidy)
tmpvars19_tidy <- tmpvars19_tidy[,c(8,9,1:7)]  

#one last check to make sure the data set is correct
skim(tmpvars19_tidy)

# finally: tidy temp covariates for 2019
getwd()
write.csv(tmpvars19_tidy, "Data/Thesis/Tidy/TempVars_2019_tidy.csv", row.names = F)

####################################################################################
####################################################################################

#join temp covariate data sets
tvars <- full_join(tmpvars18_tidy, tmpvars19_tidy)
#explore, did it work? should have 131 obs of 10 vars
skim(tvars)

#drop the SN18 and SN19 columns because for raw data they will be uninformative
tvars <- tvars %>%
  select(-SN18, -SN19)

#need common column between the two
#we will combine HUC8 and site
tvars2 <- tvars %>%
  unite(newID, c(HUC8, Site), sep = "_", remove=F)
class(tvars2$newID)

#---------------------------------------------------#
#create same character variable in the Site_List
#Site_List <- Site_List %>%
#  unite(HUCsite, c(HUC8, site), sep = "", remove=F)
#class(Site_List$HUCsite)

#use inner join to get uid info within the tvars df
#trial2 <- inner_join(trial, Site_List, by = "HUCsite")
#names(trial2)
#tvars <- trial2 %>%
#  select(-HUC8.y, -site) %>%
#  select(uid, 1:9) %>%
#  rename(HUC8 = HUC8.x)
#---------------------------------------------------#

#write new csv's
getwd()
#1) final temp variables for all sites 
write.csv(tvars, "Data/Thesis/Tidy/TmpVars_all.csv", row.names = F)

#-----
#2) just the site list could be helpful? 
#-----
#write.csv(Site_List, "C:/Users/bbkelly/Documents/Brook Trout_Brett/Thesis/data/Tidy/SmplLocsInfo.csv", row.names = F)




























