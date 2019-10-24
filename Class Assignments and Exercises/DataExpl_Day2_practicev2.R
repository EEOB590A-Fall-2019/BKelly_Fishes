#Data Exploration and visualization practice exercise
#EEOB590A

#Research Question: 
#does survival of seedlings depend on distance from nearest conspecific adult, and does that effect vary by species or canopy openness? 

library(ggplot2)
library(skimr)
library(tidyverse)
library(DataExplorer)
library(lubridate)

install.packages("DataExplorer") #I said "no" dont use compilation
######### Get Dataset Prepared for Exploration ##################

#1) start with a tidy dataset. Load data/tidy/fencesurv_tidy.csv from the tidy folder
getwd()
fence <- read_csv("/Users/brettkelly/Course-Materials/data/tidy/fencesurv_tidy.csv")


#############################################
#data dictionary
# "species"- six plant species     
# "disp" - disperser present on island - yes/no          
# "island" - island (guam, saipan, tinian, rota)     
# "site"    - 5 sites on Guam, 3 each on Rota, Tinian, Saipan         
# "fence"   - fence name (based on forest plot grid names)       
# "numalive"  - number seedlings alive in fence 
# "date"       - date fence checked     
# "observer"   - person collecting data      
# "dataentry"   - person entering data     
# "dateenter"    - date data entered    
# "uniqueidsppfence" - unique id for each spp:fence combo
# "canopydate"    - date canopy cover data taken 
# "north"          - canopy measurement 1  
# "east"           - canopy measurement 2     
# "south"            - canopy measurement 3  
# "west"             - canopy measurement 4   
# "avgcover"        -average canopy measurement (% cover)    
# "avgopen"          -average canopy measurement (% open)   
# "doubleplant"     - was this fence double planted? 
# "plantdt"          - planting data
# "dist"             - near or far from conspecific? 
# "soil"             - soil type within the fence
# "numseedplant"    - number of seedlings planted
# "DDsurvival_notes"  - notes
# "bird"             - bird presence or absence on the island
# "age"             - age of seedlings (since planting)
# "centavgopen"      - centered average open
# "adultdens_wdisp"  - adult tree density on islands with disperser for that spp
# "adultdens_wodisp" - adult tree density on islands without disperser for that spp
# "seedsize"       - seed size 
# "numtrees"        - number of conspecific trees in the plot 
# "area"            - area of the plot
# "dens_100m"       - calculated density per 100 m
# "regdens"         - density across all plots
# "regdenswd"       - density just from plots with dispersers for that species
# 
#############################################

#2) check structure to make sure everything is in correct class 
str(fence)

fence$species <- as.factor(fence$species)
class(fence$species)

#Change class of multiple columns at a time

fence <- fence %>%
  mutate_at(vars(island, site, dist), 
            factor)
class(fence$site)
class(fence$centavgopen) #numeric, which is correct for continuous vars :)

#3) Subset to the dataset you will use for the analysis
#we will use the whole dataset for now, may subset & re-run later. 
fenced <- fence %>%
  select(island, site, species, dist, centavgopen, propalive)
colnames(fenced)
#a) Make a new column for propalive by dividing numalive/numseedplant 
fence <- fence %>%
  mutate(propalive = (numalive)/(numseedplant))
colnames(fence)

#4) Decide which variables are your response variables and which are your predictors
# Response: cbind(numalive, numseedplant) or propalive
# Continuous predictors: distance, centavgopen
# Categorical predictors: species
# Random effects: island (n=4 usually), site (n=3/island)

############ Start Data Exploration ##########
#1) try the skim() functions from the skimr package and the create_report() function from DataExplorer package. Note anything that stands out to you from those. 
skim(fenced)
create_report(fenced)

## variable "centavgopen" has 40 missing obs, why I am not sure

########## INDIVIDUAL VARIABLES #####################
#2) Start with your continuous variables. 
# a) With your continuous response and predictor variables, use ggplot and geom_histogram or dotchart() to look for outliers. 
names(fenced)
## Continuous Response: "propalive"
## Continuous Predictor: "centavgopen"
ggplot(fenced, aes(centavgopen, color = island)) +
  geom_histogram() 
dotchart(fenced$centavgopen) #appear to be outliers - one obs around 40 and one around 60

ggplot(fenced, aes(propalive, color = island)) +
  geom_histogram()
dotchart(fenced$propalive) #does not appear to be any outliers, however the majority of obs are 1's, followed by 0's, so it is a bookend-end distribution

# b) With your continuous response variable, look for zero-inflation (count data only). Are there more than 25% zero's in the response? 
prop_zeros <- fenced %>%
  filter(propalive == 0) #created df with only obs where propalive = 0
90/522*100
#17% (n=90) of total obs (n=522) are zeros, less than the "danger zone" of ~25% of obs, but still somewhat high

# c) With your continuous response variable, look for independence. 
# Are there patterns in the data that are unrelated to the fixed or random effects identified above?  Consider patterns over time, for example. 
fenced$date <- fence$date
class(fenced$date)
fenced$date <- mdy(fenced$date)
class(fenced$date)

ggplot(fenced, aes(date, propalive)) +
  geom_point()

ggplot(fenced, aes(date, centavgopen)) +
  geom_point()

ggplot(fenced, aes(fence$bird, propalive)) + 
  geom_boxplot()

test <- aov(propalive ~ fence$bird, data = fenced)
summary(test)
#may be a confoudning affect of bird presence, 
#on average the proportion alive is significantly higher in the absence of birds

# 3) Now, explore your categorical predictors
# a) assess whether you have adequate sample size. How many observations per level of each of your categorical predictors? Are there any that have fewer than 15 observations?  
names(fenced)
#categorical variables = island, site, species, distance
with(fenced, table(island, species)) #missing combinations: island = rota and sp = neisosperma, 
# and island = tinian and sp = psychotria
with(fenced, table(island, dist)) #adequate obs of each combination
with(fenced, table(site, dist)) #adequte obs of each combination
with(fenced, table(species, dist)) #adequate obs of each combination
with(fenced, ftable(island, species, site))
with(fenced, table(site, species)) #many missing combinations of species and site, also, no species was planted/counted more than 8 times -->
# (cont.) on any island, may not be an issue

########## RELATIONSHIPS BETWEEN VARIABLES #######################
# 4) Explore relationships between your predictor variables
# a) look for correlation/covariation between each of your predictors (fixed & random)
class(fenced$centavgopen)
class(fenced$propalive)
?cor
summary(fenced)
cor.test(fenced$centavgopen, y = fenced$propalive, na.rm = T)
#If 2 continuous predictors, use ggplot, geom_point to plot against each other, or use pairs()
ggplot(fenced, aes(centavgopen, propalive, color = island)) +
  geom_point() #no visible trends between islands, variables dont seem to be correlated

#If 1 continuous and 1 categorical predictor, use ggplot with geom_boxplot() 
#For two categorical predictors, use summarize or table (ftable for more than 2 categories)
names(fenced)
ggplot(fenced, aes(species, propalive, color = island)) +
  geom_boxplot()
ggplot(fenced, aes(dist, propalive, color = island)) +
  geom_boxplot()
# b) Interactions: need to make sure you have adequate data for any 2-way or 3-way interactions in your model. 
## We are interested in a species * distance * centavgopen interaction. Do we have adequate sampling across this interaction? 

# 5) Look at relationships of Y vs X’s to see if variances are similar for each X value, identify the type of relationship (linear, log, etc.)
#plot each predictor and random effect against the response


###############################################################
#Summary of data exploration - summarize your general results here. 

####### 1: Individual variables ########
#a) Continuous variables (propalive, canopy)

#### Outliers (response & predictors)

#### Zero-inflation (response)

#### Independence (response)


#b) Categorical predictors and Random effects (island, soil, species)


####### 2: Multiple  variables ########
#a: Predictor vs predictor

#### Collinearity: No strong collinearities. Heterogeneity, though. 

#### Interactions - do we have enough data? 

#b: Predictor vs response: 
#### Linearity & homogeneity- relationship of Y vs X's. 

