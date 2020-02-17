library(lme4)
library(ggplot2)
library(car)
library(MASS)
library(ggResidpanel)

### Massage the dataset
mydat <- read.csv("FIBI_tidy2.csv", header = T)
mydat$Rating <- factor(mydat$Rating, levels = c("Very Poor", "Poor", "Fair", "Good", "Excellent"))
mydat$Year <- factor(mydat$Year, levels = c("2018", "2019"))

mydat$HUC10 <- as.factor(sub('.*(?=.{3}$)', '', mydat$HUC_10, perl=T))
mydat$HUC12 <- as.factor(sub('.*(?=.{2}$)', '', mydat$HUC_12, perl=T))
mydat$watershed_med <- as.factor(paste(mydat$HUC8, mydat$HUC10, sep = "_"))
mydat$watershed_sm <- as.factor(paste(mydat$watershed_med, mydat$HUC12, sep = "_"))

# Take a look at the response variable: qq plots for a normal distribution vs. a poisson distribution
ggplot(mydat, aes(x = IBIScore)) + geom_histogram(binwidth = 1)

fit_pois <- fitdistr(mydat$IBIScore + 1, "Poisson")
par(mfrow = c(1,2))
qqPlot(mydat$IBIScore + 1, "norm")
qqPlot(mydat$IBIScore + 1, "pois", lambda = fit_pois$estimate)
par(mfrow = c(1,1))

### Models: additive, random effect for watershed (at smallest scale)
  # Normal model
mod_norm <- lmer(IBIScore ~ Year + MEANT + pctrun + pctrock + pctShade + pctBrBnk + HAiFLS_dev + HAiFLS_for + (1|watershed_sm), data = mydat)
resid_panel(mod_norm)
summary(mod_norm)
Anova(mod_norm, type = "III")

  # Poisson model
mod_pois <- glmer(IBIScore ~ Year + MEANT + pctrun + pctrock + pctShade + pctBrBnk + HAiFLS_dev + HAiFLS_for + (1|watershed_sm), data = mydat, family = poisson, nAGQ = 10)
resid_panel(mod_pois)
summary(mod_pois)
