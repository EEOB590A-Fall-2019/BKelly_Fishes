library(ggplot2)
library(ggResidpanel)
library(car)

getwd()
setwd("/Users/brettkelly/Documents/MyFiles/Iowa State/EEOB590B/BKelly_Fishes")

mydat <- read.csv("Data/Thesis/Tidy/FIBI_and_Hab.csv", header = T)
summary(mydat$IBIScore)
mydat[which(mydat$IBIScore == 0),]

ggplot(mydat, aes(x = IBIScore)) + geom_histogram(binwidth = 1)

mod1 <- glm(IBIScore ~ MEANT + pctrun + pctrock + pctShade + boulder + pctBrBnk + HAiFLS_dev + HAiFLS_for, data = mydat, family = "quasipoisson")
resid_panel(mod1)

summary(mod1)
Anova(mod1, type = 3) #model selection pased on p-values or AIC?

mydat$predict <- predict(mod1, type = "response") #is there a way to cap this at 120? 
ggplot(mydat, aes(x = IBIScore, y = predict)) + geom_point()

# replace your x-axis variable with the variable you're interested in: MEANT
ggplot(mydat, aes(x = MEANT, y = predict)) + geom_point() + ylab("predicted value of FIBI score") +
  stat_smooth(method = "lm")

# replace your x-axis variable with the variable you're interested in: pctShade
ggplot(mydat, aes(x = pctShade, y = predict)) + geom_point() + ylab("predicted value of FIBI score") +
  stat_smooth(method = "lm")

# replace your x-axis variable with the variable you're interested in: pctBrBnk
ggplot(mydat, aes(x = pctBrBnk, y = predict)) + geom_point() + ylab("predicted value of FIBI score") +
  stat_smooth(method = "lm")

# replace your x-axis variable with the variable you're interested in: 
ggplot(mydat, aes(x = HAiFLS_for, y = predict)) + geom_point() + ylab("predicted value of FIBI score") +
  stat_smooth(method = "lm")

# replace your x-axis variable with the variable you're interested in: 
ggplot(mydat, aes(x = HAiFLS_dev, y = predict)) + geom_point() + ylab("predicted value of FIBI score") +
  stat_smooth(method = "lm")

Rsquared = 1 - (1410.7/2292.1)


#Notes
# oversidpersed - quasipoisson
#ordinal data - analysis
