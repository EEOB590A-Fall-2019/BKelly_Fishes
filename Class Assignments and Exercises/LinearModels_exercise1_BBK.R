# Linear Model practice exercise
#Brett Kelly - 11/7/2019
#We are going to work with a dataset on plant traits. We will test whether leaf thickness differs between tree species and island. Today, we will only do data exploration and model building/selection. We will assess model fit and interpret model results next week. 

#Helpful scripts to have open while you work on this include: DataExplorationDay2_practice_answers.R, DataExplorationDay2.R, and LinearModels.R (from class Tuesday)

#Response: thickness (leaf thickness)
#Predictors: species, island
#Random effects: none for right now, but could add some later

#Load libraries (you'll need the tidyverse)
library(tidyverse)
library(skimr)
library(ggplot2)
#install.packages("ggResidpanel")
library(ggResidpanel)

#Load dataset (tidyREUtraits.csv) and name it "traits". 
getwd()
traits <- read_csv("Data/EEOB590/tidy/tidyREUtraits.csv", col_names = T) %>%
  rename(uid = X1)
### Part 1: explore the dataset  #######

#1. Look at structure of dataset. 
skim(traits)
#1927 obs of 13 variables
#many missing values (not consistent across variables)


#------------------------------------------------------------------------------------#
#2. Subset to rows that have a value for leaf thickness. How many rows did you lose? 
traits <- traits %>%
  filter(!is.na(thickness))
summary(traits$thickness) # now down to 1151 obs, lost 776 rows!

lost = 1927 - 1151
lost

#Also we will subset to the species that were collected across all three islands. I'll give you the code for this below. 
traits <- traits %>%
  filter(species == "aglaia"| species == "aidia" | species == "guamia" | species == "cynometra" | species == "neisosperma" | species == "ochrosia" | species == "premna")  
lost2 = 1151 - 730
lost2 #421 more rows lost


#------------------------------------#
## Explore single variables ##
#3. Start with continuous variables - of which we only have the response (thickness)
# a) Check for outliers
ggplot(data = traits, aes(species, thickness, fill = island))+
  geom_boxplot()
ggplot(data = traits, aes(thickness))+
  geom_histogram()
summary(traits$thickness)
sd(traits$thickness) #0.072

IQR.thick = .2290 - .1380

outliers <- traits %>%
  mutate(lower = if_else(.1380 - (1.5*IQR.thick) > thickness, "outlier", "within range"))%>%
  mutate(upper = if_else(.2290 + (1.5*IQR.thick) < thickness, "outlier", "within range"))%>%
  select(uid, lower, upper)%>%
  filter(lower == "outlier" | upper == "outlier") #21 outliers identified whose thickness falls > Q3+1.5*IQR

# b) Check for zero-inflation (not relevant bc it's a measurement not a count)
# if needed you could filter to values of zero or count the number of times the variable of interest == 0

# c) Check for independence in the response (is each row independent?) or might there be some patterns we are not including. 
ggplot(data = traits, aes(species, thickness, fill = island))+
  geom_boxplot() 
ggplot(data = traits, aes(uid, thickness, color = island))+
  geom_point()
#would like to look across time, but variable for time not given here
#would also love to look at a map of the sites, to see how they are spatially arranged
#Later we can check the residuals of our model using 
  # >resid_panel()


#-------------------------------#
#4. Now categorical predictors. Do you have an adequate sample size? How many measurements per level of island and per level of species? 
island.count <- traits%>%
  group_by(island)%>%
  tally()
species.count <- traits%>%
  group_by(species)%>%
  tally()
#sample size is uneven across island and species, but seems adequate

#-----------------------------------------#
## Explore relationships between variables
#5) Check for correlations between predictors, or for categorical predictors, check to see if the sampling for each species is spread across each island.
#This is also useful for seeing whether you have adequate samples to run an island * species interaction.
#Try using group_by() and count(), and then graphing it using geom_bar() and facet_grid(). 

#Tables
with(traits, table(island, species))
combined.count <- traits%>%
  group_by(island, species)%>%
  tally()
#looks okay, guam and rota did not have many cynometra comparatively (7 and 8 respectively opposed to 56 on saipan!)

#Graphical Displays
ggplot(data = traits, aes(species, thickness, fill = island))+
  geom_boxplot()

#needs work, but gets the message across
ggplot(combined.count, aes(n))+
  geom_bar()+facet_grid(rows = vars(island))+
  aes(fill = as.factor(species))

#---------------------------------------------------------------------------------------------------------------------------------------------
#6) Look at relationships of Y vs X’s to see if variances are similar for each X value, identify the type of relationship (linear, log, etc.)
#plot each predictor and random effect against the response
#continuous response with two categorical explanatory variables, that have been best displayed earlier in code IMO

### Summary of data exploration ### 
#what did you find? 
#multiple outliers of thickness on the higher end
#some combinations are weak for including interaction factor, but overall the data seem adequate across levels of measurements and predictors
#appears to be relationship with thickness and the two hypothesized variables just from graphical display

### Linear model #### 
# Create a linear model to test whether leaf thickness varies by island, and whether that depends on the plant species. 

##-----------------------##
#possible model list
#thickness ~ island*species
#thickness ~ island + species
#thickness ~ island 
#thickness ~ species
#thickness ~ 1
##-----------------------##

#Option 1: Create a full model, remove interaction if not significant, but otherwise do not simplify. 
lmfull <- lm(thickness ~ island*species, data = traits)
summary(lmfull)

#Option 2: Create a full model, remove any non-significant interactions to get final model. 
mod1 <- lm(thickness ~ island*species, ddata = traits)

#Option 3: Create a full model, and all submodels, and compare using Likelihood ratio tests (anova(mod1, mod2)) to choose the best fitting model. 

#Option 4: Create a full model and all submodels and compare AIC values to choose the best fitting model


#Next week, we will assess model fit, and then interpret results. 
