library(lme4)
library(ggplot2)
library(car)
library(MASS)
library(ggResidpanel)
library(cowplot)
library(pracma)
library(tidyverse)

### Massage the dataset
mydat <- read.csv("Data/Thesis/Tidy/FIBI_tidy2.csv", header = T)
mydat$Rating <- factor(mydat$Rating, levels = c("Very Poor", "Poor", "Fair", "Good", "Excellent"))
mydat$Year <- factor(mydat$Year, levels = c("2018", "2019"))
mydat$HUC8 <- as.factor(mydat$HUC8)


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


### Models: additive, random effect for watershed (at smallest scale)
# Normal model
mod_norm2 <- lmer(IBIScore ~ HUC8 + MEANT + pctrun + pctrock + pctShade + pctBrBnk + HAiFLS_dev + HAiFLS_for + (1|watershed_sm), data = mydat)
resid_panel(mod_norm2)
ggsave("ResidPanel_FIBImod.png", dpi = 350)
summary(mod_norm2)
Anova(mod_norm2, type = "III")

#significant covariates based on type III Anova:
  # MEANT (-)***
  # pctShade (+) *
  # BrBnk (-) **

#Use predict function to plot significant covariates
mydat$predict <- predict(mod_norm2, type = "response")  

######################################################
#predict while holding other values constant 
#####################################################
huc.mean <- as.factor(rep("YEL", 50)) #HUC8=YEL
huc.UPI <- as.factor(rep("UPI", 50)) #HUC8=UPI
huc.LMAQ <- as.factor(rep("LMAQ", 50)) #HUC8=LMAQ
temp.mean <- rep(mean(mydat$MEANT), 50) #MEANT
run.mean <- rep(mean(mydat$pctrun), 50) #pctrun
rock.mean <- rep(mean(mydat$pctrock), 50) #pctrock
shade.mean <- rep(mean(mydat$pctShade), 50) #pctShade
bare.mean <- rep(mean(mydat$pctBrBnk), 50) #pctBrBnk
dev.mean <- rep(mean(mydat$HAiFLS_dev), 50) #HAiFLS_dev
for.mean <- rep(mean(mydat$HAiFLS_for), 50) #HAiFLS_for


#-----
# Vector of levels of random grouping variable - HUC_12
#-----
basin <- factor(levels(mydat$HUC8))
basin
watersheds <- levels(mydat$watershed_sm)
watersheds

#------------------------------------------
#Dataframes for each predictor of interest
#------------------------------------------

#MEANT
min.temp <- min(mydat$MEANT)
max.temp <- max(mydat$MEANT)
temp.values <- seq(from = min.temp, to = max.temp, length = 50)
temperature <- data.frame(HUC8 = huc.mean, MEANT = temp.values, pctrun = run.mean,
                          pctrock = rock.mean, pctShade = shade.mean, pctBrBnk = bare.mean,
                          HAiFLS_dev = dev.mean, HAiFLS_for = for.mean)
temp.df <- crossing(temperature, watersheds)

#pctShade
min.shade <- min(mydat$pctShade)
max.shade <- max(mydat$pctShade)
shade.values <- seq(from = min.shade, to = max.shade, length = 50)
canopy <- data.frame(HUC8 = huc.mean, MEANT = temp.mean, pctrun = run.mean,
                          pctrock = rock.mean, pctShade = shade.values, pctBrBnk = bare.mean,
                          HAiFLS_dev = dev.mean, HAiFLS_for = for.mean)
canopy.df <- crossing(canopy, watersheds)

#pctBrBnk
min.bare <- min(mydat$pctBrBnk)
max.bare <- max(mydat$pctBrBnk)
bare.values <- seq(from = min.bare, to = max.bare, length = 50)
barebank <- data.frame(HUC8 = huc.mean, MEANT = temp.mean, pctrun = run.mean,
                     pctrock = rock.mean, pctShade = shade.mean, pctBrBnk = bare.values,
                     HAiFLS_dev = dev.mean, HAiFLS_for = for.mean)
bare.df <- crossing(barebank, watersheds)






#observed vs. predicted
ggplot(mydat, aes(x = IBIScore, y = predict)) + geom_point()+
  stat_smooth(method = "lm")+
  theme_classic()+
  labs(x="Observed FIBI Score", y="Predicted FIBI Score")+
  theme(axis.title = element_text(size = 12, face = "bold"))

ggsave("PredictedFIBI.png", dpi = 350)

# replace your x-axis variable with the variable you're interested in: MEANT
a <- ggplot(mydat, aes(x = MEANT, y = predict)) + geom_point()+
  stat_smooth(method = "lm")+
  theme_cowplot()+
  labs(x="Max Daily Mean Stream Temp (°C)***", y="Predicted FIBI Score")+
  theme(axis.title = element_text(size = 12, face = "bold"))
a

# replace your x-axis variable with the variable you're interested in: pctShade
b <- ggplot(mydat, aes(x = pctShade, y = predict)) + geom_point() + 
  stat_smooth(method = "lm")+
  theme_cowplot()+
  labs(x="Percent Canopy Cover*", y="Predicted FIBI Score")+
  theme(axis.title = element_text(size = 12, face = "bold"))

# replace your x-axis variable with the variable you're interested in: pctBrBnk
c <- ggplot(mydat, aes(x = pctBrBnk, y = predict)) + geom_point() + 
  stat_smooth(method = "lm")+
  theme_cowplot()+
  labs(x="Bare Bank Index**", y="Predicted FIBI Score")+
  theme(axis.title = element_text(size = 12, face = "bold"))

#cowplot
plot_grid(a,c,b, align = "h", labels = NULL, nrow = 1)

ggsave("FIBIcovars.png", dpi = 350)















