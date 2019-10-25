# Non-game sgcn analysis - 2018 data 
# 88 sites, 3 species: Longnose Dace (LND), Southern Redbelly Dace (SRD), and Sculpins
# Habitat variables all in-stream and taken at time of sampling
#working directory - Data2018:Analysis:AFS & Driftless

library(ggplot2)

#load data
original <- as.data.frame(read.csv(file.choose()))
head(original)
names(original)

#calculate CPUE for LND, SRD, Sculpins, and Top Carnivores
original$Eff_hr <- (original$eff.min./60)
original$Eff_sec <- original$Effort..sec.
original$EF_min <- (original$Eff_sec/60)
head(original$Eff_hr)

original$LND_cpue <- (original$LND_ab/original$Eff_hr)
hist(original$LND_cpue)

original$SRD_cpue <- (original$SRD_ab/original$Eff_hr)
hist(original$SRD_cpue)

original$Sculpin_cpue <- (original$Sculpins_ab/original$Eff_hr)
hist(original$Sculpin_cpue)

original$TC_cpue <- (original$Top.Carnivore/original$Eff_hr)
hist(original$TC_cpue)

#Log Transform all data
names(original)
logged <- as.data.frame(log(1+original[,11:58]))
names(logged)  #New DF for analysis --- !!! Presence/Absence still in "original" DF

#new DF with just presence/absence of SGCNs
pres <- as.data.frame(original[,1:8])
head(pres)

#convert 1s and 0s to "character" in presence 
class(pres$LND)
pres$LND <- as.factor(pres$LND)
pres$SRD <- as.factor(pres$SRD)
pres$Sculpins <- as.factor(pres$Sculpins)
class(pres$LND)



#MLR for LND abundance (CPUE)

Lmod1 <- lm(LND_cpue ~ temp + avwid + pctfines + pctRiffle + AVembed + logged$bnkbare., data = logged) 
summary(Lmod1)
Lmod2 <- lm(LND_cpue ~ temp + avwid + pctfines + AVembed + logged$bnkbare., data = logged) 
summary(Lmod2)
Lmod3 <- lm(LND_cpue ~ temp + avwid + pctfines + logged$bnkbare., data = logged) 
summary(Lmod3)
Lmod10 <- lm(LND_cpue ~ temp + avwid + pctfines, data = logged) 
summary(Lmod10)
AIC(Lmod10)
Lmod4 <- lm(LND_cpue ~ temp + pctfines + logged$bnkbare., data = logged) 
summary(Lmod4)
Lmod5 <- lm(LND_cpue ~ temp + pctfines, data = logged) 
summary(Lmod5)
AIC(Lmod1,Lmod2,Lmod3,Lmod4,Lmod5)

Lmod6 <- lm(LND_cpue ~ temp + sdwid + pctfines + pctslow + pctEmb1, data = logged) 
summary(Lmod6)
AIC(Lmod6)

Lmod7 <- lm(LND_cpue ~ temp + avdep + pctfines + mFlow, data = logged) 
summary(Lmod7)
AIC(Lmod7)

Lmod8 <- lm(LND_cpue ~ temp + pctfines + W.D, data = logged) 
summary(Lmod8)
AIC(Lmod8)

plot(logged$LND_cpue, logged$bnkbare.)

Lmod9 <- lm(LND_cpue ~ temp + pctfines + logged$BRTcpue.fish.min., data = logged) 
summary(Lmod9)
AIC(Lmod9)

#Model Comparisons
AIC(Lmod1,Lmod2,Lmod3,Lmod4,Lmod5,Lmod6, Lmod7, Lmod8, Lmod9)

#Null Hypothesis: restricted model (Lmod5) is 
# Statistically better than general model (Lmod4)
# 2(LogLik of Lmod5 - loglik of Lmod4)
logLik(Lmod5)
logLik(Lmod4)
2*(7.725739-6.198641) #3.054196

#LR test critical value with 1df (5-4)
qchisq(0.95,1)
#3.841459

#Since LR test (computed) < LR test (critical)
#(3.0541 < 3.841)

library(lmtest)
lrtest(Lmod5, Lmod4)
#Likelihood ratio test

#Model 1: LND_cpue ~ temp + pctfines
#Model 2: LND_cpue ~ temp + pctfines + logged$bnkbare.
#Df LogLik Df  Chisq Pr(>Chisq)  
#1   4 6.1986                       
#2   5 7.7257  1 3.0542    0.08053 .

#RESULT: Best model = Lmod10: LND_cpue ~ temp + avwid + pctfines

Lmod10 <- lm(LND_cpue ~ temp + pctcbbl, data = logged) 
summary(Lmod10)
AIC(Lmod4,Lmod5,Lmod10)

Lmod11 <- lm(LND_cpue ~ temp + pctcbbl + TC_cpue, data = logged) 
summary(Lmod11)
AIC(Lmod4,Lmod5,Lmod11)

Lmod12 <- lm(LND_cpue ~ temp^2 + pctcbbl, data = logged) 
summary(Lmod12)
AIC(Lmod4,Lmod5,Lmod12)

Lmod13 <- lm(LND_cpue ~ temp + avwid^2 + pctfines, data = logged) 
summary(Lmod13)
AIC(Lmod13)

#PLOTS for Longnose Dace
Lplot1 <- ggplot(original, aes(x=original$temp, y=original$LND_cpue)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm')+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Stream Temperature\n(Celcius)', y='Longnose Dace CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")
Lplot1


Lplot2 <- ggplot(original, aes(x=original$pctfines, y=original$LND_cpue)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm')+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Percent of Fine Substrates\n(clay,silt,sand)', y='Longnose Dace CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")
Lplot2

Lplot3 <- ggplot(original, aes(x=original$avwid, y=original$LND_cpue)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm')+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Mean Wetted Width\n(meters)', y='Longnose Dace CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")
Lplot3

library(gridExtra)
LND_mlr_grid <- grid.arrange(Lplot1,Lplot2,Lplot3, ncol=3, nrow=1)




#-------------------------------------------------
# Southern Redbelly Dace
#_________________________________________________


#MLR for SRD abundance (CPUE)
names(logged)
SRmod1 <- lm(SRD_cpue ~ temp + avwid + pctfines + pctRiffle + AVembed + logged$bnkbare. + logged$AvChnlOpen., data = logged) 
summary(SRmod1)
SRmod2 <- lm(SRD_cpue ~ temp + avdep + pctcbbl + pctrun, data = logged) 
summary(SRmod2)
SRmod3 <- lm(SRD_cpue ~ temp + TC_cpue, data = logged) 
summary(SRmod3)
SRmod4 <- lm(SRD_cpue ~ temp + TC_cpue + maxdep + mFlow + pctslow, data = logged) 
summary(SRmod4)
SRmod6 <- lm(SRD_cpue ~ temp + TC_cpue + Avbnka, data = logged) 
summary(SRmod6)
AIC(SRmod1,SRmod2,SRmod3, SRmod4, SRmod6)

#PLOTS for Longnose Dace
SRplot1 <- ggplot(original, aes(x=original$temp, y=original$SRD_cpue)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm')+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Stream Temperature\n(Celcius)', y='Southern Redbelly Dace CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")
SRplot1

SRplot2 <- ggplot(original, aes(x=original$TC_cpue, y=original$SRD_cpue)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm')+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Top Carnivore CPUE\n(fish per hour)', y='Southern Redbelly Dace CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")
SRplot2

SRD_mlr_grid <- grid.arrange(SRplot1,SRplot2, ncol=2, nrow=1)


#-------------------------------------------------
# Sculpins
#_________________________________________________


#MLR for Sculpin abundance (CPUE)
names(logged)
SCmod1 <- lm(Sculpin_cpue ~ temp + avwid + pctcbbl + pctRiffle + pctEmb1 + logged$bnkbare. + logged$AvChnlOpen., data = logged) 
summary(SCmod1)
SCmod2 <- lm(Sculpin_cpue ~ temp + avdep + TC_cpue + bnkbare., data = logged) 
summary(SCmod2)
SCmod3 <- lm(Sculpin_cpue ~ temp + TC_cpue, data = logged) 
summary(SCmod3)
SCmod4 <- lm(Sculpin_cpue ~ temp + TC_cpue + pctfines + pctslow, data = logged) 
summary(SCmod4)
SCmod5 <- lm(Sculpin_cpue ~ temp + TC_cpue + mFlow + FWD + boulder + under, data = logged) 
summary(SCmod5)
SCmod6 <- lm(Sculpin_cpue ~ temp + TC_cpue + LWD, data = logged) 
summary(SCmod6)

SCmod7 <- lm(Sculpin_cpue ~ avwid + pctslow + boulder + logged$bnkbare. + TC_cpue, data = logged)
summary(SCmod7)
SCmod8 <- lm(Sculpin_cpue ~ pctslow + boulder + TC_cpue, data = logged)
summary(SCmod8)



AIC(SCmod1,SCmod2,SCmod3,SCmod4,SCmod5,SCmod6,SCmod7,SCmod8)


#PLOTS for Sculpins
SCplot1 <- ggplot(original, aes(x=original$pctslow, y=original$Sculpin_cpue)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm')+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Percent Slow Moving Habitat\n(Glide, Pool)', y='Sculpin CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")
SCplot1

SCplot2 <- ggplot(original, aes(x=original$boulder, y=original$Sculpin_cpue)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm')+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Percent Boulder Habitat', y='Sculpin CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")
SCplot2

SCplot3 <- ggplot(original, aes(x=original$TC_cpue, y=original$Sculpin_cpue)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm')+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Top Carnivore CPUE\n(fish per hour)', y='Sculpin CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")
SCplot3

Sculpin_mlr_grid <- grid.arrange(SCplot1,SCplot2,SCplot3, ncol=3, nrow=1)


## SDMS ##
install.packages(c('raster', 'rgdal', 'dismo', 'rJava'))
library(raster)
library(rgdal)
library(dismo)
library(rJava)













