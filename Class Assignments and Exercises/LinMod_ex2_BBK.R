#Linear Models Exercise 2

library(tidyverse)
library(lme4)
library(ggResidpanel)
library(emmeans)

#We will use the same dataset from last week

getwd()
traits <- read_csv("Data/EEOB590/tidy/tidyREUtraits.csv", col_names = T) %>%
  rename(uid = X1)

traits <- traits %>%
  filter(!is.na(thickness))

traits <- traits %>%
  filter(species == "aglaia"| species == "aidia" | species == "guamia" | species == "cynometra" | species == "neisosperma" | species == "ochrosia" | species == "premna")  


#-------------------------------------------------------------------------------
#1) Let's assess model fit for the model that came out on top for all 4 methods
thick1 <- lm(thickness ~ island*species, data = traits)

#Do data follow the assumptions of:
#1) independence? 

#2) normality?

#3) constant variance?

#4) linearity?

#Use ggResidpanel
resid_panel(thick1)
resid_xpanel(thick1) #to plot residuals against predictor variables
#no obvious patterns, slightly unequal variance on saipan (higher) and premna (higher)

###### Fitted and residuals by hand #####
#extract residuals
E1 <- resid(thick1, type = "pearson")

#plot fitted vs residuals
F1 <- fitted(thick1, type = "response")

par(mfrow = c(2,2), mar = c(5,5,2,2))
plot(x = F1, 
     y = E1, 
     xlab = "Fitted values",
     ylab = "Pearson residuals", 
     cex.lab = 1.5)
abline(h = 0, lty = 2)


#-------------------------------------------------------------------------------
#2) Now let's interpret the results, using each of the methods from last week: 

#Option 1: Traditional hypothesis testing (simplified model). 
#use emmeans to tell whether there are differences between islands for a given species
#which species differ between islands? 
thick1 <- lm(thickness ~ island*species, data = traits) #final model

#If you have multiple levels of a factor, or interactions between factors,
#will need a post-hoc test to assess differences between levels of the factor or combinations involved in the interaction.
#There are several options, but emmeans is a good one (also see glht in multcomp). 

op1 <- emmeans(thick1, pairwise ~ island*species) # to test whether there are differences between islands and species
op1 #shows p-value;
#compare to
summary(thick1)

#can use emmeans to test for main effects when there is an interaction present. 
#emmeans tutorial is here: https://aosmith.rbind.io/2019/03/25/getting-started-with-emmeans/
main <- emmeans(thick1, pairwise ~ species | island) #to test whether there are differences between species given that you are on Guam, Saipan, or Rota
main



#Option 2: Full model approach. 
#get confidence intervals using emmeans, and determine species
thick1 <- lm(thickness ~ island*species, data = traits) #final model

confint(thick1) #some overlap zero


#Option 3: Likelihood Ratio Test approach
#use emmeans to determine whether there are differences between species across all islands
thick1 <- lm(thickness ~ island*species, data = traits) #final model
#see above code! :)



#Option 4: Create a full model and all submodels and compare AIC values to choose the best fitting model
#just interpret the best fitting model. 
thick1 <- lm(thickness ~ island*species, data = traits) #final model
#island is a significant predictor of leaf thickness, but varies across species and island relationships
ggplot(data = traits, aes(species, thickness, fill = island))+
  geom_boxplot() 
#graphically; neisosperma, ochrosia, and premna appear to have the thickest leaves, and it also depends on island 



