# More temperature Exploration
library(tidyverse)
library(skimr)
library(lubridate)

t2018 <- read.csv("Data/Thesis/Tidy/TempData18_DateTimeCelcius.csv", header=T) #raw temp data
list18 <- read.csv("Data/Thesis/Tidy/tidy_temps18.csv", header = T)
names(t2018)
names(list18)

list18_v2 <- list18 %>%
  select(HUC8, Site, SN18 = LoggerNumber_2018, MonthIn, DayIn, YearIn) %>%
  unite(newID, c(HUC8,Site), sep = "_", remove = F) %>%
  unite(YrMnDy, c(YearIn,MonthIn,DayIn), sep = "-", remove = F) #trim data

names(list18_v2) #check column headers
head(list18_v2) #inspect 

list18_v3 <- list18_v2 %>%
  filter(SN18 != 'NA') #remove NAs


#Okay, we are now ready to trim the data to a date range from the first day all loggers are in - to the last day of summer
# Beginning Date 8/17/2018
# Ending Date-Time = 9/22/2018

t2018$Date <- ymd(t2018$Date)

tmp18trim <- t2018 %>%
  filter(Date > "2018-08-16", Date < "2018-09-23")
#worked!

skim(tmp18trim)
n_unique(tmp18trim$SN18)

plot(tmp18trim$Date, tmp18trim$Temp_C)

#test if further trimmed data is significantly different
temp18trimm <- t2018 %>%
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
YEL_004_all <- t2018[which(t2018$SN18=="3201"),]
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
YEL_017_all <- t2018[which(t2018$SN18=="3208"),]
str(YEL_017_all)

ggplot(YEL_017, aes(x=Date, y=Temp_C))+
  geom_line() #
ggplot(YEL_017_all, aes(x=Date, y=Temp_C))+
  geom_line()+
  scale_x_date(limits = as.Date(c("2018-05-20","2018-09-22"))) ## observed values are legit

#YEL_065 -- Dry & buried!
YEL_065 <- tmp18trim[which(tmp18trim$SN18=="3216"),]
YEL_065_all <- t2018[which(t2018$SN18=="3216"),]
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
YEL_113_all <- t2018[which(t2018$SN18=="3218"),]
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
UPI_57b_all <- t2018[which(t2018$SN18=="3222"),]
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
UPI_26_all <- t2018[which(t2018$SN18=="4606"),]
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