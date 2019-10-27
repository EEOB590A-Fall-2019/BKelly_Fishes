# Data Simulation - Encounter History for Brook Trout
# String of 0's and 1's across sites and occassions
# Each row is a site
# column value for "encounter history" will be in 000, 001, 010, etc. format
#######################################################################################

library(tidyverse)
library(skimr)
library(ggplot2)

# Description:
#--------------
# > Watersheds (3)
# > Sites (aimed for 60+60+30 = 150 total sites)
# > Was hoping to encounter Brook Trout at about 30-40% of sites (45-60 / 150 total)
# > Response Variable: Naive Occurrence Rate -- encounter history
# > Expected Modeled Response Variable: Occupancy Probability
# > Predictors: environmental conditions (instream physiochemical parameters)
#     > Examples: water temp, substrate comp, bank stability, etc. 

#------------------------
#simulate response first
#------------------------
# need to simulate presence (1) or absence (0) across three occassions at 150 sites
# we did not state any a priori expectations of occurrence prob differing by Watershed
#since we have no way of determining site occupancy probability beforehand, BUT know what we expected, we will split the difference, 
#and put prob of success at 35% (BASED ON NO KNOWLEDGE OF ACTUAL OBSERVED DATA POST HOC)
occ1 <- rbinom(n=150, size=1, .35) #simulate fist sampling occasion
occ2 <- rbinom(n=150, size=1, .35) #simulate second sampling occasion
occ3 <- rbinom(n=150, size=1, .35) #simulate third sampling occasion
occurrence <- data.frame(occ1, occ2, occ3) #combine simulated occasions into new DF

#now we need a 4th column that is the encounter history (combine all three columns into: 000, 001, etc. format)
occurrence <- occurrence %>%
  unite(EncHist, occ1, occ2, occ3, sep = "", remove = F) %>%
  select(occ1, occ2, occ3, EncHist)

#---------------------
#simulate predictors
#---------------------
#Categorical
watershed <- rep(c("UPI", "YEL", "LMAQ"), c(60, 60, 30))
site <- sample(1:460, 150, replace = T)
#Continuous (water temp, avwidth, avdep, pctrock, barebnk, canopyopen)
tempC <- runif(150, min = 9, max = 23)
avwid <- runif(150, min = 1, max = 14)
avdep <- runif(150, min = .12, max = .7)
pctrock <- sample(1:100, 150, replace = T)
pctbarebnk <- sample(1:100, 150, replace = T)
pctcanopyopen <- sample(1:100, 150, replace = T)

#------------------------------
#combine all into a dataframe 
#------------------------------
simoccupancy <- data.frame(watershed, site, occ1, occ2, occ3, tempC, avwid, avdep, pctrock, pctbarebnk, pctcanopyopen)
#need column for site-specific presence or absence, right now all we have is occasion specific AND, 
#need column for encounter history
simoccupancy <- simoccupancy %>%
  mutate(occtotal = occ1 + occ2 + occ3)%>%
  mutate(presence = ifelse(occtotal > 0, 1, 0))%>%
  unite(EncHist, occ1, occ2, occ3, sep = "", remove = F)
#rearrange to logical order and remove occtotal column
names(simoccupancy)
simoccupancy <- simoccupancy %>%
  select(watershed, site, occ1, occ2, occ3, presence, EncHist, tempC, avwid, avdep, pctrock, pctbarebnk, pctcanopyopen)

#Write tidy simulated CSV
getwd()
write.csv(simoccupancy,"C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos/Data/Thesis/Tidy/DataSim_BKToccupancy.csv")

#look at response
class(simoccupancy$presence)
simoccupancy$presence <- as.factor(simoccupancy$presence)
ggplot(simoccupancy, aes(presence, tempC, fill=watershed))+
  geom_boxplot()

#Run a model with dataset - need logistic regression
install.packages("corrplot")
library(corrplot)
correlations <- cor(simoccupancy[,8:13])
corrplot(correlations, method="circle")

mod1 <- glm(presence ~ tempC + pctrock + pctbarebnk + pctcanopyopen, data = simoccupancy, family = binomial)
summary(mod1)

















