##########################################################
# Explore FIBI with watershed and year information added
##########################################################

library(tidyverse)
library(skimr)

dat <- read.csv("Data/Thesis/Tidy/FIBI_tidy2.csv", header = T)
names(dat)

#change to factor
dat$Year <- as.factor(dat$Year)
dat$HUC8 <- as.factor(dat$HUC8)
dat$HUC_10 <- as.factor(dat$HUC_10)
dat$HUC_12 <- as.factor(dat$HUC_12)

#check levels
levels(dat$HUC_10)
levels(dat$HUC_12)

#distinct
n_distinct(dat$HUC_10) #11 
n_distinct(dat$HUC_12) #35

#plot data across watersheds and year

#year
ggplot(data=dat,aes(x=Year,y=IBIScore)) +
  geom_boxplot()

ggplot(data=dat,aes(x=HUC8,y=IBIScore)) +
  geom_boxplot()

#Summarize
table <- dat %>%
  group_by(Year, HUC8) %>%
  summarise(mean(IBIScore), n()) ##appears that regardless of year or sample size, sites in the YEL watershed have much lower scores

