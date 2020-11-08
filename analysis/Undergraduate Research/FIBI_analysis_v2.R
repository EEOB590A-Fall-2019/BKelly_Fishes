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
mydat$Rating_M <- factor(mydat$Rating_M, levels = c("Very Poor", "Poor", "Fair", "Good", "Excellent"))
mydat$Rating_L <- factor(mydat$Rating_L, levels = c("Very Poor", "Poor", "Fair", "Good", "Excellent"))
mydat$Year <- factor(mydat$Year, levels = c("2018", "2019"))
mydat$HUC8 <- as.factor(mydat$HUC8)


mydat$HUC10 <- as.factor(sub('.*(?=.{3}$)', '', mydat$HUC_10, perl=T))
mydat$HUC12 <- as.factor(sub('.*(?=.{2}$)', '', mydat$HUC_12, perl=T))
mydat$watershed_med <- as.factor(paste(mydat$HUC8, mydat$HUC10, sep = "_"))
mydat$watershed_sm <- as.factor(paste(mydat$watershed_med, mydat$HUC12, sep = "_"))

# Take a look at the response variable - Mundahl & Simon: qq plots for a normal distribution vs. a poisson distribution
ggplot(mydat, aes(x = IBIScore_M)) + geom_histogram(binwidth = 1)

fit_pois <- fitdistr(mydat$IBIScore_M + 1, "Poisson")
par(mfrow = c(1,2))
qqPlot(mydat$IBIScore_M + 1, "norm")
qqPlot(mydat$IBIScore_M + 1, "pois", lambda = fit_pois$estimate)
par(mfrow = c(1,1))

# Take a look at the response variable - Lyons: qq plots for a normal distribution vs. a poisson distribution
ggplot(mydat, aes(x = IBIScore_L)) + geom_histogram(binwidth = 1)

fit_pois2 <- fitdistr(mydat$IBIScore_L + 1, "Poisson")
par(mfrow = c(1,2))
qqPlot(mydat$IBIScore_L + 1, "norm")
qqPlot(mydat$IBIScore_L + 1, "pois", lambda = fit_pois2$estimate)
par(mfrow = c(1,1))



### Model for Mundahl & Simon: additive, random effect for watershed (at smallest scale)
# Normal model with 
mod_norm2 <- lmer(IBIScore_M ~ HUC8 + MEANT + pctrun + pctrock + pctShade + pctBrBnk + HAiFLS_dev + HAiFLS_for + (1|watershed_sm), data = mydat)
resid_panel(mod_norm2)
ggsave("ResidPanel_FIBImod.png", dpi = 350)
summary(mod_norm2)
Anova(mod_norm2, type = "III")
#significant covariates based on type III Anova:
  # MEANT (-)***
  # pctShade (+) *
  # BrBnk (-) **

## Normal model without HUC8
#mod_norm3 <- lmer(IBIScore ~ MEANT + pctrun + pctrock + pctShade + pctBrBnk + HAiFLS_dev + HAiFLS_for + (1|watershed_sm), data = mydat)
#resid_panel(mod_norm3)
#summary(mod_norm3)
#Anova(mod_norm3, type = "III")

#AIC(mod_norm2, mod_norm3)


######################################################
#predict while holding other values constant 
#####################################################
huc.YEL <- as.factor(rep("YEL", 50)) #HUC8=YEL
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
watershed_sm <- levels(mydat$watershed_sm)
watershed_sm

#------------------------------------------
#Dataframes for each predictor of interest
#------------------------------------------

#MEANT
min.temp <- min(mydat$MEANT)
max.temp <- max(mydat$MEANT)
temp.values <- seq(from = min.temp, to = max.temp, length = 50)
temperature <- data.frame(HUC8 = huc.YEL, MEANT = temp.values, pctrun = run.mean,
                          pctrock = rock.mean, pctShade = shade.mean, pctBrBnk = bare.mean,
                          HAiFLS_dev = dev.mean, HAiFLS_for = for.mean)
temp.df <- crossing(temperature, watershed_sm)

#pctShade
min.shade <- min(mydat$pctShade)
max.shade <- max(mydat$pctShade)
shade.values <- seq(from = min.shade, to = max.shade, length = 50)
canopy <- data.frame(HUC8 = huc.YEL, MEANT = temp.mean, pctrun = run.mean,
                          pctrock = rock.mean, pctShade = shade.values, pctBrBnk = bare.mean,
                          HAiFLS_dev = dev.mean, HAiFLS_for = for.mean)
canopy.df <- crossing(canopy, watershed_sm)

#pctBrBnk
min.bare <- min(mydat$pctBrBnk)
max.bare <- max(mydat$pctBrBnk)
bare.values <- seq(from = min.bare, to = max.bare, length = 50)
barebank <- data.frame(HUC8 = huc.YEL, MEANT = temp.mean, pctrun = run.mean,
                     pctrock = rock.mean, pctShade = shade.mean, pctBrBnk = bare.values,
                     HAiFLS_dev = dev.mean, HAiFLS_for = for.mean)
bare.df <- crossing(barebank, watershed_sm)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Make predictions using new function with varying temperature values.

predict.fun.temp <- function(my.lmm) {
  predict(my.lmm, newdata = temperature, re.form = NA)   # This is predict.merMod 
}
temperature$ml.value <- predict.fun.temp(mod_norm2)

# Make predictions in 100 bootstraps of the LMM. Use these to get confidence
# intervals.
lmm.temp.boots <- bootMer(mod_norm2, predict.fun.temp, nsim = 10000)
temp.df.predicted <- cbind(temperature, confint(lmm.temp.boots))
head(temp.df.predicted)
write.csv(temp.df.predicted, "Data/Thesis/Tidy/temp_df_predicted.csv", row.names = F)

#Make ggplot for predicted FIBI as function of temperature 
temperature <- read.csv("Data/Thesis/Tidy/temp_df_predicted.csv", header=T)
names(temperature)
temperature <- temperature %>%
  rename(lcl=X2.5..,ucl=X97.5..)
a <- ggplot(data = temperature, aes(x=MEANT))+
  geom_ribbon(aes(ymin=temperature$lcl, ymax=temperature$ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=ml.value), colour="Black", size=1)+
  labs(x="Max Daily Mean Stream Temp (°C)",
       y="Predicted FIBI Score")+
  theme_bw()+
  theme(axis.title = element_text(face = "bold", size = 14))+
  theme(panel.grid = element_blank())+
  theme(strip.text.x = element_text(size=10,face = "bold"))+
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12))+
  scale_y_continuous(limits = c(0,120),
                     breaks = c(0,10,35,70,105),
                     labels = c("0","10","35","70","105"))+
  scale_x_continuous(limits = c(10,22),
                     breaks = c(10,12,14,16,18,20,22))
a
ggsave("FIBI_vs_Temp.png", dpi = 400)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Make predictions using new function with varying BareBank values.

predict.fun.bare <- function(my.lmm) {
  predict(my.lmm, newdata = barebank, re.form = NA)   # This is predict.merMod 
}
barebank$ml.value <- predict.fun.bare(mod_norm2)
head(barebank)

# Make predictions in 10000 bootstraps of the LMM. Use these to get confidence
# intervals.
lmm.bare.boots <- bootMer(mod_norm2, predict.fun.bare, nsim = 10000)
bare.df.predicted <- cbind(barebank, confint(lmm.bare.boots))
head(bare.df.predicted)
write.csv(bare.df.predicted, "Data/Thesis/Tidy/bare_df_predicted.csv", row.names = F)

#Make ggplot for predicted FIBI as function of barebank 
barebank <- read.csv("Data/Thesis/Tidy/bare_df_predicted.csv", header=T)
names(barebank)
barebank <- barebank %>%
  rename(lcl=X2.5..,ucl=X97.5..)
b <- ggplot(data = barebank, aes(x=pctBrBnk))+
  geom_ribbon(aes(ymin=barebank$lcl, ymax=barebank$ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=ml.value), colour="Black", size=1)+
  labs(x="Bare Bank Index",
       y=NULL)+
  theme_bw()+
  theme(axis.title = element_text(face = "bold",size = 14))+
  theme(panel.grid = element_blank())+
  theme(strip.text.x = element_text(size=10,face = "bold"))+
  scale_y_continuous(limits = c(0,120),
                     breaks = c(0,10,35,70,105),
                     labels = c("0","10","35","70","105"))+
  scale_x_continuous(limits = c(0,2.5),
                     breaks = c(0.0,0.5,1.0,1.5,2.0,2.5))+
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12))
b
ggsave("FIBI_vs_BrBank.png", dpi = 400)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Make predictions using new function with varying canopy cover values.

predict.fun.canopy <- function(my.lmm) {
  predict(my.lmm, newdata = canopy, re.form = NA)   # This is predict.merMod 
}
canopy$ml.value <- predict.fun.canopy(mod_norm2)
head(canopy)

# Make predictions in 10000 bootstraps of the LMM. Use these to get prediction intervals.
lmm.canopy.boots <- bootMer(mod_norm2, predict.fun.canopy, nsim = 10000)
canopy.df.predicted <- cbind(canopy, confint(lmm.canopy.boots))
head(canopy.df.predicted)
write.csv(canopy.df.predicted, "Data/Thesis/Tidy/canopy_df_predicted.csv", row.names = F)

#Make ggplot for predicted FIBI as function of canopy 
canopy <- read.csv("Data/Thesis/Tidy/canopy_df_predicted.csv", header=T)
names(canopy)
canopy <- canopy %>%
  rename(lcl=X2.5..,ucl=X97.5..)
c <- ggplot(data = canopy, aes(x=pctShade))+
  geom_ribbon(aes(ymin=canopy$lcl, ymax=canopy$ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=ml.value), colour="Black", size=1)+
  labs(x="% Canopy Cover",
       y=NULL)+
  theme_bw()+
  theme(axis.title = element_text(face = "bold", size = 14))+
  theme(panel.grid = element_blank())+
  theme(strip.text.x = element_text(size=10,face = "bold"))+
  scale_y_continuous(limits = c(0,120),
                     breaks = c(0,10,35,70,105),
                     labels = c("0","10","35","70","105"))+
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12))
c
ggsave("FIBI_vs_Canopy.png", dpi = 400)
#cowplot
plot_grid(a,b,c, align = "h", labels = NULL, nrow = 1)

ggsave("FIBIcovars.png", dpi = 350)

####################################################
######        Observed VS Predicted          ######
###################################################

#observed vs. predicted
names(mydat)
mydat2 <- mydat %>%
  select(IBIScore, HUC8, MEANT, pctrun, pctrock, 
         pctShade, pctBrBnk, HAiFLS_dev, HAiFLS_for,
         watershed_sm)


# Make predictions using new function
predict.fun <- function(my.lmm) {
  predict(my.lmm, newdata = mydat2, re.form = NA)   # This is predict.merMod 
}
mydat2$estimate <- predict.fun(mod_norm2)
head(mydat2)
mydat2[32,11] <- 0

# Make predictions in 10000 bootstraps of the LMM. Use these to get confidence
# intervals.
lmm.boots <- bootMer(mod_norm2, predict.fun, nsim = 10000)
FIBI.df.predicted <- cbind(mydat2, confint(lmm.boots))
head(FIBI.df.predicted)
write.csv(FIBI.df.predicted, "Data/Thesis/Tidy/FIBI_df_predicted.csv", row.names = F)

# Use 95% confidence interval instead of SEM
ggplot(mydat2, aes(x=estimate, y=IBIScore)) + 
  #geom_errorbar(aes(ymax=FIBI.df.predicted$`2.5 %`, ymin=FIBI.df.predicted$`97.5 %`), width=.1)+
  geom_point(
    color="black",
    fill="#69b3a2",
    shape=21,
    alpha=0.75,
    size=4,
    stroke = 2
  )+
  scale_y_continuous(limits = c(0,110), breaks = c(0,10,35,70,105))+
  scale_x_continuous(limits = c(0,110), breaks = c(0,10,35,70,105))+
  geom_abline(intercept = 0, slope = 1, color="blue", linetype="dashed",
              size=1)+
  theme_cowplot()+
  theme(legend.position = "bottom")+
  labs(y="Observed FIBI Score", 
       x="Predicted FIBI Score")+
  ggtitle("Observed versus Predicted FIBI Score")+
  theme(axis.title = element_text(size = 14, face = "bold"))

ggsave("Predicted_Xaxis_FIBI.png", dpi = 350)

## Different axis breaks
# Use 95% confidence interval instead of SEM
ggplot(mydat2, aes(x=estimate, y=IBIScore)) + 
  #geom_errorbar(aes(ymax=FIBI.df.predicted$`2.5 %`, ymin=FIBI.df.predicted$`97.5 %`), width=.1)+
  geom_point(
    color="black",
    fill="#69b3a2",
    shape=21,
    alpha=0.75,
    size=4,
    stroke = 2
  )+
  scale_y_continuous(limits = c(0,110))+
  scale_x_continuous(limits = c(0,110))+
  geom_abline(intercept = 0, slope = 1, color="blue", linetype="dashed",
              size=1)+
  theme_cowplot()+
  theme(legend.position = "bottom")+
  labs(y="Observed FIBI Score", 
       x="Predicted FIBI Score")+
  ggtitle("Observed versus Predicted FIBI Score")+
  theme(axis.title = element_text(size = 14, face = "bold"))

ggsave("Predicted_Xaxis_FIBI.png", dpi = 350)


## Switch Axis Labels
# Use 95% confidence interval instead of SEM
ggplot(mydat2, aes(x=IBIScore, y=estimate)) + 
  geom_errorbar(aes(ymin=FIBI.df.predicted$`2.5 %`, ymax=FIBI.df.predicted$`97.5 %`), width=1)+
  geom_point(
    color="black",
    fill="#69b3a2",
    shape=21,
    alpha=0.75,
    size=4,
    stroke = 2
  )+
  scale_y_continuous(limits = c(0,max(FIBI.df.predicted$`97.5 %`)), breaks = c(0,10,35,70,105))+
  scale_x_continuous(limits = c(0,110), breaks = c(0,10,35,70,105))+
  geom_abline(intercept = 0, slope = 1, color="blue", linetype="dashed",
              size=1)+
  theme_cowplot()+
  theme(legend.position = "bottom")+
  labs(x="Observed FIBI Score", 
       y="Predicted FIBI Score")+
  ggtitle("Predicted versus Observed FIBI Score")+
  theme(axis.title = element_text(size = 14, face = "bold"))

#ggsave("Predicted_Yaxis_FIBI.png", dpi = 350)


# Split color by watershed
ggplot(mydat2, aes(x=estimate, y=IBIScore, fill=HUC8)) + 
  geom_point(
    color="black",
    #fill="#69b3a2",
    shape=21,
    alpha=0.75,
    size=3,
    stroke = 2
  )+
  scale_y_continuous(limits = c(0,110), breaks = c(0,10,35,70,105))+
  scale_x_continuous(limits = c(0,110), breaks = c(0,10,35,70,105))+
  geom_abline(intercept = 0, slope = 1, color="black", linetype="dashed",
              size=1)+
  theme_cowplot()+
  theme(legend.position = "bottom")+
  labs(y="Observed FIBI Score", 
       x="Predicted FIBI Score")+
  ggtitle("Observed versus Predicted FIBI Score")+
  theme(axis.title = element_text(size = 14, face = "bold"))

#ggsave("Predicted_Xaxis_HUC8_FIBI.png", dpi = 350)

#--------------------------------------------------------------------------------------------------
snore <- read.csv("Data/Thesis/Tidy/FIBI_df_predicted.csv", header = T)
## More Graphix
## Observed (y) vs. Predicted (x)
## 1-1 line PLUS stat smooth
summary(snore)
#snore[which(snore$estimate<0),]
#snore[32,11] <- 0
snore <- snore %>%
  rename(lcl=X2.5.., ucl=X97.5..) %>%
  mutate(cl_range = ucl-lcl)
max(snore$estimate)
max(snore$IBIScore)

ggplot(snore, aes(x=estimate, y=IBIScore)) + 
  geom_point(
    color="black",
    fill="grey70",
    shape=21,
    alpha=0.75,
    size=4,
    stroke = 1.5
  )+
  scale_y_continuous(limits = c(0,112), breaks = c(0,10,35,70,105))+
  scale_x_continuous(limits = c(0,112), breaks = c(0,10,35,70,105))+
  geom_abline(intercept = 0, slope = 1, color="black", linetype="dashed",
              size=1)+
  stat_smooth(method = "lm", se=F, color="black", size=1.5)+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(y="Observed FIBI Score", 
       x="Predicted FIBI Score")+
  theme(axis.title = element_text(size = 14, face = "bold"))+
  theme(axis.text = element_text(size = 12))+
  theme(panel.grid = element_blank())+
  annotate("text",x=20,y=110,label="Pearson's r = 0.72",
           fontface="bold", size=5)

y <- snore$IBIScore
x <- snore$estimate
cor(x,y, use = "everything", method = "pearson")
cor(x,y, use = "everything", method = "kendall")
cor(x,y, use = "everything", method = "spearman")

ggsave("FIBI_ObsY_vs_PredX_BW.png", dpi = 350)

#---------------------------------------------------

#color, no 0

ggplot(snore, aes(x=estimate, y=IBIScore)) + 
  geom_point(
    color="black",
    fill="royalblue2",
    shape=21,
    alpha=0.75,
    size=4,
    stroke = 1.5
  )+
  scale_y_continuous(limits = c(0,112), breaks = c(0,10,35,70,105))+
  scale_x_continuous(limits = c(0,112), breaks = c(0,10,35,70,105))+
  geom_abline(intercept = 0, slope = 1, color="black", linetype="dashed",
              size=1)+
  stat_smooth(method = "lm", se=F, color="black", size=1.5)+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(y="Observed FIBI Score", 
       x="Predicted FIBI Score")+
  theme(axis.title = element_text(size = 14, face = "bold"))+
  theme(axis.text = element_text(size = 12))+
  theme(panel.grid = element_blank())+
  annotate("text",x=20,y=110,label="Pearson's r = 0.72",
           fontface="bold", size=5)

ggsave("FIBI_ObsY_vs_PredX_color.png", dpi = 350)





###############################################################################
###############################################################################
#########################  Lyons et al.  ######################################
###############################################################################

##----------------------------------------
### Model for Lyons: additive, random effect for watershed (at smallest scale)
# Normal model with 
mod_norm3 <- lmer(IBIScore_L ~ HUC8 + MEANT + pctrun + pctrock + pctShade + pctBrBnk + HAiFLS_dev + HAiFLS_for + (1|watershed_sm), data = mydat)
resid_panel(mod_norm3)
#ggsave("ResidPanel_FIBImod_lyons.png", dpi = 350)
summary(mod_norm3)
Anova(mod_norm3, type = "III")
#significant covariates based on type III Anova:
# HUC8*
# MEANT**
# pctBrBnk *
##-----------------------------------------

######################################################
#predict while holding other values constant 
#####################################################
huc.YEL <- as.factor(rep("YEL", 50)) #HUC8=YEL
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
watershed_sm <- levels(mydat$watershed_sm)
watershed_sm

#------------------------------------------
#Dataframes for each predictor of interest
#------------------------------------------
huc.YEL.short <- as.factor(rep("YEL", 1)) #HUC8=YEL
huc.UPI.short <- as.factor(rep("UPI", 1)) #HUC8=UPI
huc.LMAQ.short <- as.factor(rep("LMAQ", 1)) #HUC8=LMAQ
huc <- as.factor(c("UPI", "YEL", "LMAQ"))
temp.mean.sh <- rep(mean(mydat$MEANT), 3) #MEANT
run.mean.sh <- rep(mean(mydat$pctrun), 3) #pctrun
rock.mean.sh <- rep(mean(mydat$pctrock), 3) #pctrock
shade.mean.sh <- rep(mean(mydat$pctShade), 3) #pctShade
bare.mean.sh <- rep(mean(mydat$pctBrBnk), 3) #pctBrBnk
dev.mean.sh <- rep(mean(mydat$HAiFLS_dev), 3) #HAiFLS_dev
for.mean.sh <- rep(mean(mydat$HAiFLS_for), 3) #HAiFLS_for

#HUC8
UPI.df <- data.frame(HUC8 = huc.UPI.short, MEANT = temp.mean, pctrun = run.mean,
                          pctrock = rock.mean, pctShade = shade.mean, pctBrBnk = bare.mean,
                          HAiFLS_dev = dev.mean, HAiFLS_for = for.mean)
#---
YEL.df <- data.frame(HUC8 = huc.YEL.short, MEANT = temp.mean, pctrun = run.mean,
                     pctrock = rock.mean, pctShade = shade.mean, pctBrBnk = bare.mean,
                     HAiFLS_dev = dev.mean, HAiFLS_for = for.mean)
#---
LMAQ.df <- data.frame(HUC8 = huc.LMAQ.short, MEANT = temp.mean, pctrun = run.mean,
                     pctrock = rock.mean, pctShade = shade.mean, pctBrBnk = bare.mean,
                     HAiFLS_dev = dev.mean, HAiFLS_for = for.mean)

#HUC8 combined
huc.df <- data.frame(HUC8 = huc, MEANT = temp.mean.sh, pctrun = run.mean.sh,
                     pctrock = rock.mean.sh, pctShade = shade.mean.sh, pctBrBnk = bare.mean.sh,
                     HAiFLS_dev = dev.mean.sh, HAiFLS_for = for.mean.sh)

#MEANT
min.temp <- min(mydat$MEANT)
max.temp <- max(mydat$MEANT)
temp.values <- seq(from = min.temp, to = max.temp, length = 50)
temperature <- data.frame(HUC8 = huc.YEL, MEANT = temp.values, pctrun = run.mean,
                          pctrock = rock.mean, pctShade = shade.mean, pctBrBnk = bare.mean,
                          HAiFLS_dev = dev.mean, HAiFLS_for = for.mean)
temp.df <- crossing(temperature, watershed_sm)


#-----
#UPI
#-----
temperature2 <- data.frame(HUC8 = huc.UPI, MEANT = temp.values, pctrun = run.mean,
                          pctrock = rock.mean, pctShade = shade.mean, pctBrBnk = bare.mean,
                          HAiFLS_dev = dev.mean, HAiFLS_for = for.mean)
temp.df2 <- crossing(temperature2, watershed_sm)

#-----
#LMAQ
#-----
temperature3 <- data.frame(HUC8 = huc.LMAQ, MEANT = temp.values, pctrun = run.mean,
                           pctrock = rock.mean, pctShade = shade.mean, pctBrBnk = bare.mean,
                           HAiFLS_dev = dev.mean, HAiFLS_for = for.mean)
temp.df3 <- crossing(temperature3, watershed_sm)

#pctBrBnk
min.bare <- min(mydat$pctBrBnk)
max.bare <- max(mydat$pctBrBnk)
bare.values <- seq(from = min.bare, to = max.bare, length = 50)
barebank <- data.frame(HUC8 = huc.YEL, MEANT = temp.mean, pctrun = run.mean,
                       pctrock = rock.mean, pctShade = shade.mean, pctBrBnk = bare.values,
                       HAiFLS_dev = dev.mean, HAiFLS_for = for.mean)
bare.df <- crossing(barebank, watershed_sm)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Make predictions using new function with varying temperature values.

predict.fun.temp2 <- function(my.lmm) {
  predict(my.lmm, newdata = temperature, re.form = NA)   # This is predict.merMod 
}
temperature$ml.value <- predict.fun.temp2(mod_norm3)

# Make predictions in 100 bootstraps of the LMM. Use these to get confidence
# intervals.
lmm.temp.boots2 <- bootMer(mod_norm3, predict.fun.temp2, nsim = 10000)
temp.df.predicted2 <- cbind(temperature, confint(lmm.temp.boots2))
head(temp.df.predicted2)
write.csv(temp.df.predicted2, "Data/Thesis/Tidy/temp_df_predicted_Lyons_Yellow.csv", row.names = F)

#Make ggplot for predicted FIBI as function of temperature 
#temperature <- read.csv("Data/Thesis/Tidy/temp_df_predicted.csv", header=T)
names(temp.df.predicted2)
temp.df.predicted2 <- temp.df.predicted2 %>%
  rename(lcl=`2.5 %`,ucl=`97.5 %`)

aa <- ggplot(data = temp.df.predicted2, aes(x=MEANT))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=ml.value), colour="Black", size=1)+
  labs(x="Max Daily Mean Stream Temp (°C)",
       y="Predicted FIBI Score (Lyons et al.)")+
  theme_bw()+
  theme(axis.title = element_text(face = "bold", size = 14))+
  theme(panel.grid = element_blank())+
  theme(strip.text.x = element_text(size=10,face = "bold"))+
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12))+
  scale_y_continuous(limits = c(-10,100),
                     breaks = c(0,10,30,60,90),
                     labels = c("0","10","30","60","90"))+
  scale_x_continuous(limits = c(10,22),
                     breaks = c(10,12,14,16,18,20,22))
aa
#ggsave("FIBI_vs_Temp.png", dpi = 400)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Make predictions using new function with varying BareBank values.

predict.fun.bare2 <- function(my.lmm) {
  predict(my.lmm, newdata = barebank, re.form = NA)   # This is predict.merMod 
}
barebank$ml.value <- predict.fun.bare2(mod_norm3)
head(barebank)

# Make predictions in 10000 bootstraps of the LMM. Use these to get confidence
# intervals.
lmm.bare.boots2 <- bootMer(mod_norm3, predict.fun.bare2, nsim = 10000)
bare.df.predicted2 <- cbind(barebank, confint(lmm.bare.boots2))
head(bare.df.predicted2)
write.csv(bare.df.predicted2, "Data/Thesis/Tidy/bare_df_predicted_Lyons_Yellow.csv", row.names = F)

#Make ggplot for predicted FIBI as function of barebank 
#barebank <- read.csv("Data/Thesis/Tidy/bare_df_predicted.csv", header=T)
#names(barebank)
bare.df.predicted2 <- bare.df.predicted2 %>%
  rename(lcl=`2.5 %`,ucl=`97.5 %`)
bb <- ggplot(data = bare.df.predicted2, aes(x=pctBrBnk))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=ml.value), colour="Black", size=1)+
  labs(x="Bare Bank Index",
       y="")+
  theme_bw()+
  theme(axis.title = element_text(face = "bold",size = 14))+
  theme(panel.grid = element_blank())+
  theme(strip.text.x = element_text(size=10,face = "bold"))+
  scale_y_continuous(limits = c(-15,100),
                     breaks = c(0,10,30,60,90),
                     labels = c("0","10","30","60","90"))+
  scale_x_continuous(limits = c(0,2.5),
                     breaks = c(0.0,0.5,1.0,1.5,2.0,2.5))+
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.text.x = element_text(size = 12))
bb
#ggsave("FIBI_vs_BrBank.png", dpi = 400)

#cowplot
plot_grid(aa,bb, align = "h", labels = NULL, nrow = 1)

ggsave("FIBIcovars_Lyons.png", dpi = 350)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


####################################################
######        Observed VS Predicted          ######
###################################################

#observed vs. predicted
names(mydat)
mydat2 <- mydat %>%
  select(IBIScore_L, HUC8, MEANT, pctrun, pctrock, 
         pctShade, pctBrBnk, HAiFLS_dev, HAiFLS_for,
         watershed_sm)


# Make predictions using new function
predict.fun <- function(my.lmm) {
  predict(my.lmm, newdata = mydat2, re.form = NA)   # This is predict.merMod 
}
mydat2$estimate <- predict.fun(mod_norm3)
head(mydat2)
#mydat2[32,11] <- 0

#--------------------------------------------------------------------------------------------------
#snore <- read.csv("Data/Thesis/Tidy/FIBI_df_predicted.csv", header = T)
## More Graphix
## Observed (y) vs. Predicted (x)
## 1-1 line PLUS stat smooth

ggplot(mydat2, aes(x=estimate, y=IBIScore_L)) + 
  geom_point(
    color="black",
    fill="grey70",
    shape=21,
    alpha=0.75,
    size=4,
    stroke = 1.5
  )+
  scale_y_continuous(limits = c(0, 90), breaks = c(0,10,30,60,90))+
  scale_x_continuous(limits = c(-11, 90), breaks = c(0,10,30,60,90))+
  geom_abline(intercept = 0, slope = 1, color="black", linetype="dashed",
              size=1)+
  stat_smooth(method = "lm", se=F, color="black", size=1.5)+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(y="Observed FIBI Score (Lyons et al.)", 
       x="Predicted FIBI Score (Lyons et al.)")+
  theme(axis.title = element_text(size = 14, face = "bold"))+
  theme(axis.text = element_text(size = 12))+
  theme(panel.grid = element_blank())+
  annotate("text",x=6,y=90,label="Pearson's r = 0.72",
           fontface="bold", size=5)

y <- mydat2$IBIScore_L
x <- mydat2$estimate
cor(x,y, use = "everything", method = "pearson")
cor(x,y, use = "everything", method = "kendall")
cor(x,y, use = "everything", method = "spearman")

ggsave("FIBI_ObsY_vs_PredX_LyonsEtAl.png", dpi = 350)

#-------------------------------------------------------------------------------

# Make predictions using new function
predict.fun <- function(my.lmm) {
  predict(my.lmm, newdata = huc.df, re.form = NA)   # This is predict.merMod 
}
huc.df$estimate <- predict.fun(mod_norm3)
head(huc.df)

# Make predictions in 10000 bootstraps of the LMM. Use these to get confidence
# intervals.
lmm.huc.boots <- bootMer(mod_norm3, predict.fun, nsim = 10000)
huc.df.predict <- cbind(huc.df, confint(lmm.huc.boots))
head(huc.df.predict)
write.csv(huc.df.predict, "Data/Thesis/Tidy/huc_df_predicted_Lyons.csv", row.names = F)

#-------------------------------------------------------------------------------


mydat.new <- mydat %>%
  mutate(BKT_Pres = ifelse(M6_BKTsalmonid>0,1,0)) %>%
  replace_na(list(BKT_Pres=0))

mydat.new$BKT_Pres <- as.factor(mydat.new$BKT_Pres)


#-----
library(extrafont)
#font_import()
loadfonts(device="win")   #Register fonts for Windows bitmap output
fonts()
#-----


################################################################################
#Mundahl
ggplot(mydat.new, aes(x=BKT_Pres, y=IBIScore_M))+
  stat_boxplot(geom = 'errorbar', width=0.25)+
  geom_boxplot(aes(fill=BKT_Pres), width=0.50)+
  geom_jitter(shape=16, position = position_jitter(0.2))+
  theme_bw()+
  scale_x_discrete(labels=c("Absent", "Present"))+
  theme(legend.position = "NULL")+
  scale_fill_manual(values = c("white", "grey"))+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))+
  theme(axis.text.x = element_text(size=12, family = "Times New Roman"))+
  labs(x="Brook Trout Presence", y="Mundahl and Simon FIBI")

ggsave("Mundahl_BKT_Boxplots.png", dpi = 350)

#---
#Summaries

Sites.BKT <- mydat.new %>%
  filter(BKT_Pres==1)

Sites.NoBKT <- mydat.new %>%
  filter(BKT_Pres==0)

nobkt.Mun <- mydat.new %>%
  filter(BKT_Pres==0) %>%
  summarise(avg_MIBI=mean(IBIScore_M), min=min(IBIScore_M), max=max(IBIScore_M),
            SE=(sd(IBIScore_M)/sqrt(length(Sites.NoBKT))), SmpSize = count(Sites.NoBKT)) %>%
  mutate(BKT_status=0)


bkt.Mun <- mydat.new %>%
  filter(BKT_Pres==1) %>%
  summarise(avg_MIBI=mean(IBIScore_M), min=min(IBIScore_M), max=max(IBIScore_M),
            SE=(sd(IBIScore_M)/sqrt(length(Sites.NoBKT))), SmpSize = count(Sites.BKT)) %>%
  mutate(BKT_status=1)

summ_table_Mun <- data.frame(rbind(bkt.Mun, nobkt.Mun))
write.csv(summ_table_Mun, "analysis/Undergraduate Research/SummaryTable_MunIBI_BKT_vs_NoBKT.csv", row.names = F)


# Mann Whitney U / Wilcox Sign Rank Test 

help("wilcox.test")

# Ho: Median Mun_FIBI when BKT are present = Mun_FIBI when BKT are absent
# two-sided
bkt.m.pr <- mydat.new %>%
  filter(BKT_Pres==1)
bkt.m.ab <- mydat.new %>%
  filter(BKT_Pres==0)

#Scores
a <- bkt.m.ab$IBIScore_M
b <- bkt.m.pr$IBIScore_M


#-----
#Mundahl FIBI vs BKT presence
#-----
#two sided
wilcox.test(a, b, mu=0, alt="two.sided", conf.int=T, conf.level=0.95, paired=F,
            exact=F)
# p<0.001


################################################################################
#Lyons
ggplot(mydat.new, aes(x=BKT_Pres, y=IBIScore_L))+
  stat_boxplot(geom = 'errorbar', width=0.25)+
  geom_boxplot(aes(fill=BKT_Pres), width=0.50)+
  geom_jitter(shape=16, position = position_jitter(0.2))+
  theme_bw()+
  scale_x_discrete(labels=c("Absent", "Present"))+
  theme(legend.position = "NULL")+
  scale_fill_manual(values = c("white", "grey"))+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))+
  theme(axis.text.x = element_text(size=12, family = "Times New Roman"))+
  labs(x="Brook Trout Presence", y="Lyons et al. FIBI")

ggsave("Lyons_BKT_Boxplots.png", dpi = 350)


#---
#Summaries

#Sites.BKT <- mydat.new %>%
#  filter(BKT_Pres==1)

#Sites.NoBKT <- mydat.new %>%
#  filter(BKT_Pres==0)

nobkt.Lyons <- mydat.new %>%
  filter(BKT_Pres==0) %>%
  summarise(avg_LIBI=mean(IBIScore_L), min=min(IBIScore_L), max=max(IBIScore_L),
            SE=(sd(IBIScore_L)/sqrt(length(Sites.NoBKT))), SmpSize = count(Sites.NoBKT)) %>%
  mutate(BKT_status=0)


bkt.Lyons <- mydat.new %>%
  filter(BKT_Pres==1) %>%
  summarise(avg_LIBI=mean(IBIScore_L), min=min(IBIScore_L), max=max(IBIScore_L),
            SE=(sd(IBIScore_L)/sqrt(length(Sites.NoBKT))), SmpSize = count(Sites.BKT)) %>%
  mutate(BKT_status=1)

summ_table_Lyons <- data.frame(rbind(bkt.Lyons, nobkt.Lyons))
write.csv(summ_table_Lyons, "analysis/Undergraduate Research/SummaryTable_LyonsIBI_BKT_vs_NoBKT.csv", row.names = F)


# Mann Whitney U / Wilcox Sign Rank Test 

#help("wilcox.test")

# Ho: Median Lyons_FIBI when BKT are present = Lyons_FIBI when BKT are absent
# two-sided
bkt.l.pr <- mydat.new %>%
  filter(BKT_Pres==1)
bkt.l.ab <- mydat.new %>%
  filter(BKT_Pres==0)

#Scores
aa <- bkt.l.ab$IBIScore_L
bb <- bkt.l.pr$IBIScore_L


#-----
#Lyons FIBI vs BKT presence
#-----
#two sided
wilcox.test(aa, bb, mu=0, alt="two.sided", conf.int=T, conf.level=0.95, paired=F,
            exact=F)
# p<0.001







#Scatterplots:
#1) Habitat Covars
names(mydat.new)
hab.scat.m <- mydat.new %>%
  select(IBIScore_M, MEANT, pctrun, pctrock, pctShade, pctBrBnk,
         HAiFLS_dev, HAiFLS_for)

pairs(hab.scat.m)


hab.scat.ly <- mydat.new %>%
  select(IBIScore_L, MEANT, pctrun, pctrock, pctShade, pctBrBnk,
         HAiFLS_dev, HAiFLS_for)

pairs(hab.scat.ly)

#2) IBI metrics
other_dat <- read.csv("Data/Thesis/Tidy/tidy_FIBI_FULL.csv", header = T)
names(other_dat)

#mun metrics vs MFIBI
mets.scat.m <- other_dat %>%
  select(IBIScore_M, numspecies, numCWspp, numMINspp, numBENspp, 
         numTOLspp, pctBKTsalmon, pctINTOLindv, pctTOLindv, 
         pctWSUindv, pctTOPCARNindv, CWindv150, WWindv150)
pairs(mets.scat.m)

#mun metrics vs LyFIBI
mets.scat.m2 <- other_dat %>%
  select(IBIScore_L, numspecies, numCWspp, numMINspp, numBENspp, 
         numTOLspp, pctBKTsalmon, pctINTOLindv, pctTOLindv, 
         pctWSUindv, pctTOPCARNindv, CWindv150, WWindv150)
pairs(mets.scat.m2)

#Lyons metrics vs LyFIBI
mets.scat.ly <- other_dat %>%
  select(IBIScore_L, numIntolSP, pctTOLindv, pctTC_noRBT, pctCW_noRBT, pctBKT_noRBT)
pairs(mets.scat.ly)

#Lyons metrics vs MFIBI
mets.scat.ly2 <- other_dat %>%
  select(IBIScore_M, numIntolSP, pctTOLindv, pctTC_noRBT, pctCW_noRBT, pctBKT_noRBT)
pairs(mets.scat.ly2)

################################################################################
################################################################################
################################################################################
################# NOT RUN ######################################################
################################################################################
################################################################################
################################################################################

### Models: additive, random effect for watershed (at smallest scale)
# Normal model
#mod_norm <- lmer(IBIScore ~ Year + MEANT + pctrun + pctrock + pctShade + pctBrBnk + HAiFLS_dev + HAiFLS_for + (1|watershed_sm), data = mydat)
#resid_panel(mod_norm)
#summary(mod_norm)
#Anova(mod_norm, type = "III")

# Poisson model
#mod_pois <- glmer(IBIScore ~ Year + MEANT + pctrun + pctrock + pctShade + pctBrBnk + HAiFLS_dev + HAiFLS_for + (1|watershed_sm), data = mydat, family = poisson, nAGQ = 10)
#resid_panel(mod_pois)
#summary(mod_pois)


#-------------------------------------------------------------------------
#color, no 0

ggplot(snore, aes(x=estimate, y=IBIScore)) + 
  geom_point(
    color="black",
    fill="royalblue2",
    shape=21,
    alpha=0.75,
    size=4,
    stroke = 1.5
  )+
  scale_y_continuous(limits = c(0,112), breaks = c(0,10,35,70,105))+
  scale_x_continuous(limits = c(0,112), breaks = c(0,10,35,70,105))+
  geom_abline(intercept = 0, slope = 1, color="black", linetype="dashed",
              size=1)+
  stat_smooth(method = "lm", se=F, color="black", size=1.5)+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(y="Observed FIBI Score", 
       x="Predicted FIBI Score")+
  theme(axis.title = element_text(size = 14, face = "bold"))+
  theme(axis.text = element_text(size = 12))+
  theme(panel.grid = element_blank())+
  annotate("text",x=20,y=110,label="Pearson's r = 0.72",
           fontface="bold", size=5)

ggsave("FIBI_ObsY_vs_PredX_color.png", dpi = 350)
