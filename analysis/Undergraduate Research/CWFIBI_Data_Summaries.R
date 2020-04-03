library(tidyverse)
library(skimr)

#load data - you need to make sure to set your working directory! 
mydat <- read.csv("Data/Thesis/Tidy/FIBI_tidy2.csv", header = T)

### Change some of the variables to factors
mydat$Rating <- factor(mydat$Rating, levels = c("Very Poor", "Poor", "Fair", "Good", "Excellent"))
mydat$Year <- factor(mydat$Year, levels = c("2018", "2019"))
mydat$HUC8 <- as.factor(mydat$HUC8)

#subset data - only choose variables we want to summarize
names(mydat)
newdat <- mydat %>%
  select(HUC8, Year,IBIScore, Rating, MEANT, pctrun, pctrock, pctBrBnk, 
         HAiFLS_dev, HAiFLS_for)

#summary of dataset
summary(newdat) #basic summaries
skim(newdat) #more in depth summary - includes sample sizes, standard deviations, etc. 
              #when reading the output, p0 is the same thing as min and p100 is max



