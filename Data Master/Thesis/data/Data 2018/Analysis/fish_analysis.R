###############################
#Fish and habitat associations#
###############################
install.packages("ggplot2")
library(ggplot2)
##install.packages("ggpubr")
library(ggpubr)
library(dplyr)
library(grid)
library(gridExtra)

#load data
data <- as.data.frame(read.csv(file.choose(), header=T))
#check data
head(data)

#BKT presence as factor
data$BKT <- as.factor(data$BKT)
class(data$BKT)

#plot raw data
hist(data$mWidth) 
hist(data$mDepth)
hist(data$mVelocity)
hist(data$X..Riffle)
hist(data$X..Run)
hist(data$X..Slow)
hist(data$X..Fine)
hist(data$X..Course)

#data transformations
hist(data$Temp)
boxplot(data$Temp)
data$logTemp <- log(data$Temp)
data$logmWidth <- log(data$mWidth)
data$logmDepth <- log(data$mDepth)
data$logmVelocity <- log(data$mVelocity)
data$logRiffle <- log(1 + data$X..Riffle)
data$logRiffle
data[42,]
data$logRun <- log(1 + data$X..Run)
data$logSlow <- log(1 + data$X..Slow)
data$logSlow
data$logFine <- log(1 + data$X..Fine)
data$logCourse <- log(1 + data$X..Course)

#boxplots for BKT 1/0 and variables
boxplot(data$logTemp ~ data$BKT, xlab = "BKT Presence", ylab = "Temperature (Celcius)")
boxplot(data$logmWidth ~ data$BKT, xlab = "BKT Presence", ylab = "mean wetted width (m)")
boxplot(data$logmDepth ~ data$BKT, xlab = "BKT Presence", ylab = "mean depth (m)")
boxplot(data$logmVelocity ~ data$BKT, xlab = "BKT Presence", ylab = "Velocity (m/sec)")
boxplot(data$logRiffle ~ data$BKT, xlab = "BKT Presence", ylab = "% Riffle")
boxplot(data$logRun ~ data$BKT, xlab = "BKT Presence", ylab = "% Run")
boxplot(data$logSlow ~ data$BKT, xlab = "BKT Presence", ylab = "% Slow")
slow_box_bkt <- ggplot(data, aes(data$BKT, logSlow, fill=data$BKT)) + geom_boxplot(width=.35)
slow_box_bkt
fine_box_bkt <- ggplot(data, aes(data$BKT, logFine, fill=data$BKT)) + geom_boxplot(width=.35)
fine_box_bkt
course_box_bkt <- ggplot(data, aes(data$BKT, logCourse, fill=data$BKT)) + geom_boxplot(width=.35)
course_box_bkt

########################################################################################
#plot2 <- ggplot(group, aes(fish, Temp, fill=fish)) + geom_boxplot(width=.35, notch=F)+
#  theme(plot.background = element_blank(),
#        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
#######################################################################################

#T.Test BKT
ttest1_BKT <- t.test(data$logTemp ~ data$BKT)
ttest1_BKT #SIG!
ttest2_BKT <- t.test(data$logSlow ~ data$BKT)
ttest2_BKT #SIG!
ttest3_BKT <- t.test(data$logFine ~ data$BKT)
ttest3_BKT #NOT SIG


#ANOVA BKT
AOV1_BKT <- aov(data$logTemp ~ data$BKT)
summary(AOV1_BKT) #Significant!
AOV2_BKT <- aov(data$logSlow ~ data$BKT)
summary(AOV2_BKT) #Significant!
AOV3_BKT <- aov(data$logFine ~ data$BKT)
summary(AOV3_BKT) #NOT

##SLR for BKT ~ logTemp
?lm()
bkt_mod1 <- lm(data$logTemp ~ data$BKT)
summary(bkt_mod1)
plot(bkt_mod1)

bkt_mod2 <- lm(data$logSlow ~ data$BKT)
summary(bkt_mod2)

data$BRT <- as.factor(data$BRT) #BRT presence to factor

#boxplots for BRT 1/0 and hab. variables
boxplot(data$logTemp ~ data$BRT, xlab = "BRT Presence", ylab = "Temperature (Celcius)")
boxplot(data$logmWidth ~ data$BRT, xlab = "BRT Presence", ylab = "mean wetted width (m)")
boxplot(data$logmDepth ~ data$BRT, xlab = "BRT Presence", ylab = "mean depth (m)")
boxplot(data$logmVelocity ~ data$BRT, xlab = "BRT Presence", ylab = "Velocity (m/sec)")
boxplot(data$logRiffle ~ data$BRT, xlab = "BRT Presence", ylab = "% Riffle")
boxplot(data$logRun ~ data$BRT, xlab = "BRT Presence", ylab = "% Run")
slow_box_brt <- ggplot(data, aes(data$BRT, logSlow, fill=data$BRT)) + geom_boxplot(width=.35)
slow_box_brt
fine_box_brt <- ggplot(data, aes(data$BRT, logFine, fill=data$BRT)) + geom_boxplot(width=.35)
fine_box_brt
course_box_brt <- ggplot(data, aes(data$BRT, logCourse, fill=data$BRT)) + geom_boxplot(width=.35)
course_box_brt

#T.Test BRT
ttest1_BRT <- t.test(data$logTemp ~ data$BRT)
ttest1_BRT # NOT
ttest2_BRT <- t.test(data$logmWidth ~ data$BRT)
ttest2_BRT #SIG!
ttest3_BRT <- t.test(data$logmDepth ~ data$BRT)
ttest3_BRT #P=.018
tt4_brt <- t.test(data$logmVelocity ~ data$BRT)
tt4_brt
tt5_brt <- t.test(data$logRiffle ~ data$BRT)
tt5_brt
tt6_brt <- t.test(data$logRun ~ data$BRT)
tt6_brt
tt7_brt <- t.test(data$logSlow ~ data$BRT)
tt7_brt

#ANOVA BRT
AOV1_BRT <- aov(data$logTemp ~ data$BRT)
summary(AOV1_BRT) #NOT - close as ISH
AOV2_BRT <- aov(data$logmWidth ~ data$BRT)
summary(AOV2_BRT) #Significant!
AOV3_BRT <- aov(data$logmDepth ~ data$BRT)
summary(AOV3_BRT) #P=.014

##SLR for BRT 
#?lm()
brt_mod1 <- lm(data$logmWidth ~ data$BRT)
summary(brt_mod1)
brt2 <- lm(data$logmDepth ~ data$BRT)
summary(brt2)
brt3 <- lm(data$logRiffle ~ data$BRT)
summary(brt3)


#boxplots for LND 1/0 and hab. variables
data$LND <- as.factor(data$LND)
boxplot(data$logTemp ~ data$LND, xlab = "LND Presence", ylab = "Temperature (Celcius)")
boxplot(data$logmWidth ~ data$LND, xlab = "LND Presence", ylab = "mean wetted width (m)")
boxplot(data$logmDepth ~ data$LND, xlab = "LND Presence", ylab = "mean depth (m)")
boxplot(data$logmVelocity ~ data$LND, xlab = "LND Presence", ylab = "Velocity (m/sec)")
boxplot(data$logRiffle ~ data$LND, xlab = "LND Presence", ylab = "% Riffle")
boxplot(data$logRun ~ data$LND, xlab = "LND Presence", ylab = "% Run")
slow_box_lnd <- ggplot(data, aes(data$LND, logSlow, fill=data$LND)) + geom_boxplot(width=.35)
slow_box_lnd
fine_box_lnd <- ggplot(data, aes(data$LND, logFine, fill=data$LND)) + geom_boxplot(width=.35)
fine_box_lnd
course_box_lnd <- ggplot(data, aes(data$LND, logCourse, fill=data$LND)) + geom_boxplot(width=.35)
course_box_lnd

#lnd analysis
tt1.lnd <- t.test(data$logTemp ~ data$LND)
tt1.lnd
aov1.lnd <- aov(data$logTemp ~ data$LND)
summary(aov1.lnd)
tt2.lnd <- t.test(data$logmWidth ~ data$LND)
tt2.lnd
tt3.lnd <- t.test(data$logmDepth ~ data$LND)
tt3.lnd
tt4.lnd <- t.test(data$logmVelocity ~ data$LND)
tt4.lnd
tt5.lnd <- t.test(data$logRiffle ~ data$LND)
tt5.lnd
aov2.lnd <- aov(data$logRiffle ~ data$LND)
summary(aov2.lnd)
tt6.lnd <- t.test(data$logFine ~ data$LND)
tt6.lnd
aov3.lnd <- aov(data$logFine ~ data$LND)
summary(aov3.lnd)



#boxplots for SRD 1/0 and hab. variables
data$SRD <- as.factor(data$SRD)
boxplot(data$logTemp ~ data$SRD, xlab = "SRD Presence", ylab = "Temperature (Celcius)")
boxplot(data$logmWidth ~ data$SRD, xlab = "SRD Presence", ylab = "mean wetted width (m)")
boxplot(data$logmDepth ~ data$SRD, xlab = "SRD Presence", ylab = "mean depth (m)")
boxplot(data$logmVelocity ~ data$SRD, xlab = "SRD Presence", ylab = "Velocity (m/sec)")
boxplot(data$logRiffle ~ data$SRD, xlab = "SRD Presence", ylab = "% Riffle")
boxplot(data$logRun ~ data$SRD, xlab = "SRD Presence", ylab = "% Run")
slow_box_srd <- ggplot(data, aes(data$SRD, logSlow, fill=data$SRD)) + geom_boxplot(width=.35)
slow_box_srd
fine_box_srd <- ggplot(data, aes(data$SRD, logFine, fill=data$SRD)) + geom_boxplot(width=.35)
fine_box_srd
course_box_srd <- ggplot(data, aes(data$SRD, logCourse, fill=data$SRD)) + geom_boxplot(width=.35)
course_box_srd

#analysis for SRD
tt1.srd <- t.test(data$logTemp ~ data$SRD)
tt1.srd
tt2.srd <- t.test(data$logmDepth ~ data$SRD)
tt2.srd
tt3.srd <- t.test(data$logRiffle ~ data$SRD)
tt3.srd
tt4.srd <- t.test(data$logRun ~ data$SRD)
tt4.srd




#boxplots for Cottus 1/0 and hab. variables
data$Cottus <- as.factor(data$Cottus)
boxplot(data$logTemp ~ data$Cottus, xlab = "Cottus Presence", ylab = "Temperature (Celcius)")
boxplot(data$logmWidth ~ data$Cottus, xlab = "Cottus Presence", ylab = "mean wetted width (m)")
boxplot(data$logmDepth ~ data$Cottus, xlab = "Cottus Presence", ylab = "mean depth (m)")
boxplot(data$logmVelocity ~ data$Cottus, xlab = "Cottus Presence", ylab = "Velocity (m/sec)")
boxplot(data$logRiffle ~ data$Cottus, xlab = "Cottus Presence", ylab = "% Riffle")
boxplot(data$logRun ~ data$Cottus, xlab = "Cottus Presence", ylab = "% Run")
slow_box_Cottus <- ggplot(data, aes(data$Cottus, logSlow, fill=data$Cottus)) + geom_boxplot(width=.35)
slow_box_Cottus
fine_box_Cottus <- ggplot(data, aes(data$Cottus, logFine, fill=data$Cottus)) + geom_boxplot(width=.35)
fine_box_Cottus
course_box_Cottus <- ggplot(data, aes(data$Cottus, logCourse, fill=data$Cottus)) + geom_boxplot(width=.35)
course_box_Cottus

#Cottus analysis
tt1.cot <- t.test(data$logTemp ~ data$Cottus)
tt1.cot

tt2.cot <- t.test(data$logmWidth ~ data$Cottus)
tt2.cot

aov1.cot <- aov(data$logmWidth ~ data$Cottus)
summary(aov1.cot)

tt3.cot <- t.test(data$logmDepth ~ data$Cottus)
tt3.cot

aov2.cot <- aov(data$logmDepth ~ data$Cottus)
summary(aov2.cot)

tt4.cot <- t.test(data$logmVelocity ~ data$Cottus)
tt4.cot

aov3.cot <- aov(data$logmVelocity ~ data$Cottus)
summary(aov3.cot)

tt5.cot <- t.test(data$logRiffle ~ data$Cottus)
tt5.cot
aov4.cot <- aov(data$logRiffle ~ data$Cottus)
summary(aov4.cot)

tt6.cot <- t.test(data$logRun ~ data$Cottus)
tt6.cot
aov5.cot <- aov(data$logRun ~ data$Cottus)
summary(aov5.cot)

tt7.cot <- t.test(data$logSlow ~ data$Cottus)
tt7.cot

tt8.cot <- t.test(data$logFine ~ data$Cottus)
tt8.cot





##################################################
#UPDATED ANALYSIS: 
#ANOVA for all associations with presence/absence
#Linear models for associations with abundance
##################################################


#--------------------------------------------------------------------------------------------
# Modeling effort in hrs vs mlogWidth and mlogDepth to calculate CPUE for abundance analyses
#--------------------------------------------------------------------------------------------
#load data

effort <- as.data.frame(read.csv(file.choose(), header=T))
head(effort)

#plot data
effort$EFhours <- effort$Effort..hours.
hist(effort$EFhours)
plot(effort$EFhours, data$logmWidth)
plot(effort$EFhours, data$logmDepth)

data[63,] #logmW = 2.82, logmDep =0.48

#Brook Trout

#Matrix plots and Correlation Tests
plots.WDV <- pairs(data[,24:26])
plots.RiffRunSlow <- pairs(data[,27:29])
plots.Substrate <- pairs(data[,30:31])

ctest.WDV <- cor(data[,24:26])
ctest.WDV
cor.test(data$logmDepth, data$logmWidth) #P<0.01
cor.test(data$logmWidth, data$logmVelocity) #P=0.02
cor.test(data$logmDepth, data$logmVelocity) #p=0.01
ctest.RRS <- cor(data[,27:29])
ctest.RRS
cor.test(data$logRiffle, data$logSlow) #P=0.11
cor.test(data$logRiffle, data$logRun) #P=0.07
cor.test(data$logRun, data$logSlow) #P<.01
ctest.Sub <- cor(data[,30:31])
ctest.Sub
cor.test(data$logFine, data$logCourse) #P<.01


#One-way ANOVA BKT
names(data)
AOV1_BKT <- aov(data$logTemp ~ data$BKT)
summary(AOV1_BKT) #
AOV2_BKT <- aov(data$logmWidth ~ data$BKT)
summary(AOV2_BKT) #
AOV3_BKT <- aov(data$logmDepth ~ data$BKT)
summary(AOV3_BKT) #
aov4.bkt <- aov(data$logmVelocity ~ data$BKT)
summary(aov4.bkt)
aov5.bkt <- aov(data$logRiffle ~ data$BKT)
summary(aov5.bkt)
aov6.bkt <- aov(data$logRun ~ data$BKT)
summary(aov6.bkt)
aov7.bkt <- aov(data$logSlow ~ data$BKT)
summary(aov7.bkt)
aov8.bkt <- aov(data$logFine ~ data$BKT)
summary(aov8.bkt)
aov9.bkt <- aov(data$logCourse ~ data$BKT)
summary(aov9.bkt)

#MANOVA for BKT based on correlated hab. variables
manova1.bkt <- manova(cbind(logmDepth,logmWidth)~BKT, data=data)
summary(manova1.bkt)
manova2.bkt <- manova(cbind(logmDepth,logmVelocity)~BKT, data=data)
summary(manova2.bkt)
manova3.bkt <- manova(cbind(logmVelocity,logmWidth)~BKT, data=data)
summary(manova3.bkt)
manova4.bkt <- manova(cbind(logRiffle, logRun)~BKT, data=data)
summary(manova4.bkt)
manova5.bkt <- manova(cbind(logFine, logCourse)~BKT, data=data)
summary(manova5.bkt)


##BKT abundance ~ hab variables

#plots
data$bkt_num <- data$BKT..
plot(data$logTemp, data$bkt_num)
plot(data$logmWidth, data$bkt_num)
plot(data$logmDepth, data$bkt_num)
plot(data$logmVelocity, data$bkt_num)
plot(data$logRiffle, data$bkt_num)
plot(data$logRun, data$bkt_num)
plot(data$logSlow, data$bkt_num)
plot(data$logFine, data$bkt_num)
plot(data$logCourse, data$bkt_num)

#SLR and MLR
#?lm()

bkt_mod1 <- lm(data$logTemp ~ data$bkt_num)
summary(bkt_mod1)

bkt_mod2 <- lm(data$logmWidth ~ data$bkt_num)
summary(bkt_mod2)

bkt_mod3 <- lm(data$logmDepth ~ data$bkt_num)
summary(bkt_mod3)

bkt_mod4 <- lm(data$logmVelocity ~ data$bkt_num)
summary(bkt_mod4)

bkt_mod5 <- lm(data$logRiffle ~ data$bkt_num)
summary(bkt_mod5)

bkt_mod6 <- lm(data$logRun ~ data$bkt_num)
summary(bkt_mod6)

bkt_mod7 <- lm(data$logSlow ~ data$bkt_num)
summary(bkt_mod7)

bkt_mod8 <- lm(data$logFine ~ data$bkt_num)
summary(bkt_mod8)

bkt_mod9 <- lm(data$logCourse ~ data$bkt_num)
summary(bkt_mod9)

#BKT plots for figures
library(RColorBrewer)
display.brewer.all()

#Temp
bkt.logT.box <- ggplot(data, aes(BKT, data$logTemp, fill = BKT)) + geom_boxplot(width=.35, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Set1") +
  labs(x='Brook Trout\n(0=Absent, 1=Present)', y='log Stream Temperature\n(Celcius)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

bkt.logT.box ##pub standard Boxplot


#%slow
bkt.slow.box <- ggplot(data, aes(BKT, data$logSlow, fill = BKT)) + geom_boxplot(width=.35, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Set1") +
  labs(x='Brook Trout\n(0=Absent, 1=Present)', y='log Percent Slow Macrohabitat') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")


bkt.slow.box ##pub standard Boxplot


#---------------------------------------------------
#Brown Trout

#Matrix plots and Correlation Tests
plots.WDV <- pairs(data[,24:26])
plots.RiffRunSlow <- pairs(data[,27:29])
plots.Substrate <- pairs(data[,30:31])

ctest.WDV <- cor(data[,24:26])
ctest.WDV
cor.test(data$logmDepth, data$logmWidth) #P<0.01
cor.test(data$logmWidth, data$logmVelocity) #P=0.02
cor.test(data$logmDepth, data$logmVelocity) #p=0.01
ctest.RRS <- cor(data[,27:29])
ctest.RRS
cor.test(data$logRiffle, data$logSlow) #P=0.11
cor.test(data$logRiffle, data$logRun) #P=0.07
cor.test(data$logRun, data$logSlow) #P<.01
ctest.Sub <- cor(data[,30:31])
ctest.Sub
cor.test(data$logFine, data$logCourse) #P<.01


#One-way ANOVA BRT
names(data)
AOV1_BRT <- aov(data$logTemp ~ data$BRT)
summary(AOV1_BRT) #F=3.93,P=0.51
AOV2_BRT <- aov(data$logmWidth ~ data$BRT) #SIG
summary(AOV2_BRT) #F=28.17, P<.01
AOV3_BRT <- aov(data$logmDepth ~ data$BRT) #SIG
summary(AOV3_BRT) #F=6.3, P=.01
aov4.brt <- aov(data$logmVelocity ~ data$BRT)
summary(aov4.brt) #F=2.79, P=0.10
aov5.brt <- aov(data$logRiffle ~ data$BRT) #SIG
summary(aov5.brt) #F=9.21, P<.01
aov6.brt <- aov(data$logRun ~ data$BRT)
summary(aov6.brt) #F=1.62, P=0.21
aov7.brt <- aov(data$logSlow ~ data$BRT)
summary(aov7.brt) #F=1.66, P=0.20
aov8.brt <- aov(data$logFine ~ data$BRT)
summary(aov8.brt) #2.30, P=0.13
aov9.brt <- aov(data$logCourse ~ data$BRT) #SIG
summary(aov9.brt) #F=5.41, P=0.02

#MANOVA for BRT based on correlated hab. variables
manova1.brt <- manova(cbind(logmDepth,logmWidth)~BRT, data=data)
summary(manova1.brt)
manova2.brt <- manova(cbind(logmDepth,logmVelocity)~BRT, data=data)
summary(manova2.brt)
manova3.brt <- manova(cbind(logmVelocity,logmWidth)~BRT, data=data)
summary(manova3.brt)
manova4.brt <- manova(cbind(logRiffle, logRun)~BRT, data=data)
summary(manova4.brt)
manova5.brt <- manova(cbind(logFine, logCourse)~BRT, data=data)
summary(manova5.brt)


##BRT abundance ~ hab variables

data$BRT_CPUE <- (data$brt_num/effort$EFhours)
head(data)
#plots
data$brt_num <- data$BRT..
plot(data$logTemp, data$BRT_CPUE)
plot(data$logmWidth, data$BRT_CPUE)
plot(data$logmDepth, data$BRT_CPUE)
plot(data$logmVelocity, data$BRT_CPUE)
plot(data$logRiffle, data$BRT_CPUE)
plot(data$logRun, data$BRT_CPUE)
plot(data$logSlow, data$BRT_CPUE)
plot(data$logFine, data$BRT_CPUE)
plot(data$logCourse, data$BRT_CPUE)

#SLR and MLR
#?lm()

###### SLR #######
brt_mod1 <- lm(data$logTemp ~ data$BRT_CPUE)
summary(brt_mod1)

brt_mod2 <- lm(data$logmWidth ~ data$BRT_CPUE) #SIG
summary(brt_mod2)

brt_mod3 <- lm(data$logmDepth ~ data$BRT_CPUE) 
summary(brt_mod3)

brt_mod4 <- lm(data$logmVelocity ~ data$BRT_CPUE)
summary(brt_mod4)

brt_mod5 <- lm(data$logRiffle ~ data$BRT_CPUE) #SIG
summary(brt_mod5)

brt_mod6 <- lm(data$logRun ~ data$BRT_CPUE)
summary(brt_mod6)

brt_mod7 <- lm(data$logSlow ~ data$BRT_CPUE)
summary(brt_mod7)

brt_mod8 <- lm(data$logFine ~ data$BRT_CPUE)
summary(brt_mod8)

brt_mod9 <- lm(data$logCourse ~ data$BRT_CPUE)
summary(brt_mod9)

max(data$BRT_CPUE)
max(data$brt_num)


#plots
##BRT CPUE vs logmWidth
ggplot(data, aes(x=data$logmWidth, y=data$BRT_CPUE)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm')+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='log Mean Wetted Width (m)', y='Brown Trout CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

#BRT CPUE vs log%Riffle
ggplot(data, aes(x=data$logRiffle, y=data$BRT_CPUE)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm')+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='log Percent Riffle Habitat', y='Brown Trout CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")


#BRT plots for figures
library(RColorBrewer)
display.brewer.all()

#Width
brt.logW.box <- ggplot(data, aes(BRT, data$logmWidth, fill = BRT)) + geom_boxplot(width=.35, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Dark2") +
  labs(x='Brown Trout\n(0=Absent, 1=Present)', y='log Mean Wetted Width\n(m)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

brt.logW.box ##pub standard Boxplot


#Depth
brt.logD.box <- ggplot(data, aes(BRT, data$logmDepth, fill = BRT)) + geom_boxplot(width=.35, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Dark2") +
  labs(x='Brown Trout\n(0=Absent, 1=Present)', y='log Mean Depth\n(m)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

brt.logD.box ##pub standard Boxplot

#riffle
brt.logRiff.box <- ggplot(data, aes(BRT, data$logRiffle, fill = BRT)) + geom_boxplot(width=.35, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Dark2") +
  labs(x='Brown Trout\n(0=Absent, 1=Present)', y='log Percent Riffle Habitat') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

brt.logRiff.box ##pub standard Boxplot

#Course
brt.logCourse.box <- ggplot(data, aes(BRT, data$logCourse, fill = BRT)) + geom_boxplot(width=.35, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Dark2") +
  labs(x='Brown Trout\n(0=Absent, 1=Present)', y='log Percent Course Substrate\n(gravel or cobble)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

brt.logCourse.box ##pub standard Boxplot


#-----------------------
#Longnose Dace----------
#_______________________

#One-way ANOVA LND
names(data)
AOV1_LND <- aov(data$logTemp ~ data$LND) #SIG
summary(AOV1_LND) #F=5.01,P=0.03
AOV2_LND <- aov(data$logmWidth ~ data$LND) #SIG
summary(AOV2_LND) #F=17.49, P<0.01
AOV3_LND <- aov(data$logmDepth ~ data$LND) #SIG
summary(AOV3_LND) #F=4.70, P=0.03
aov4.LND <- aov(data$logmVelocity ~ data$LND) #SIG
summary(aov4.LND) #F=6.56, P=0.01
aov5.LND <- aov(data$logRiffle ~ data$LND) 
summary(aov5.LND) #F=1.97, P=0.17
aov6.LND <- aov(data$logRun ~ data$LND)
summary(aov6.LND) #F=2.30, P=0.13
aov7.LND <- aov(data$logSlow ~ data$LND)
summary(aov7.LND) #F=1.20, P=0.28
aov8.LND <- aov(data$logFine ~ data$LND) #SIG
summary(aov8.LND) #4.54, P=0.04
aov9.LND <- aov(data$logCourse ~ data$LND) 
summary(aov9.LND) #F=2.98, P=0.09


##LND abundance ~ hab variables

#add LND cpue
data$LND_CPUE <- (data$LND../effort$EFhours)
head(data)

#plots
plot(data$logTemp, data$LND_CPUE)
plot(data$logmWidth, data$LND_CPUE)
plot(data$logmDepth, data$LND_CPUE)
plot(data$logmVelocity, data$LND_CPUE)
plot(data$logRiffle, data$LND_CPUE)
plot(data$logRun, data$LND_CPUE)
plot(data$logSlow, data$LND_CPUE)
plot(data$logFine, data$LND_CPUE)
plot(data$logCourse, data$LND_CPUE)

#SLR
#?lm()
names(data)

###### SLR #######
lnd_mod1 <- lm(data$logTemp ~ data$LND_CPUE) #P=0.02
summary(lnd_mod1)

lnd_mod2 <- lm(data$logmWidth ~ data$LND_CPUE) 
summary(lnd_mod2)

lnd_mod3 <- lm(data$logmDepth ~ data$LND_CPUE) 
summary(lnd_mod3)

lnd_mod4 <- lm(data$logmVelocity ~ data$LND_CPUE)
summary(lnd_mod4)

lnd_mod5 <- lm(data$logRiffle ~ data$LND_CPUE) 
summary(lnd_mod5)

lnd_mod6 <- lm(data$logRun ~ data$LND_CPUE)
summary(lnd_mod6)

lnd_mod7 <- lm(data$logSlow ~ data$LND_CPUE)
summary(lnd_mod7)

lnd_mod8 <- lm(data$logFine ~ data$LND_CPUE) #P=0.01
summary(lnd_mod8)

lnd_mod9 <- lm(data$logCourse ~ data$LND_CPUE)
summary(lnd_mod9)



#CPUE plots -- SIG

##LND CPUE vs logTemp
lnd.T <- ggplot(data, aes(x=data$Temp, y=data$LND_CPUE)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm')+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Stream Temperature\n(Celcius)', y='Longnose Dace CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

#LND CPUE vs log%Fine
lnd.fine <- ggplot(data, aes(x=data$X..Fine, y=data$LND_CPUE)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm')+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Percent Fine Substrate\n(clay, silt, sand)', y='Longnose Dace CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

#CPUE plots -- NOT SIG

#LND CPUE vs wid
lnd.width <- ggplot(data, aes(x=data$mWidth, y=data$LND_CPUE)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm', color = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Mean Wetted Width\n(m)', y='Longnose Dace CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")
lnd.width

lnd.dep <- ggplot(data, aes(x=data$mDepth, y=data$LND_CPUE)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm', color = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Depth\n(m)', y='Longnose Dace CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

lnd.vel <- ggplot(data, aes(x=data$mVelocity, y=data$LND_CPUE)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm', color = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='In-Current Velocity\n(m/sec)', y='Longnose Dace CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

lnd.riff <- ggplot(data, aes(x=data$X..Riffle, y=data$LND_CPUE)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm', color = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Percent Riffle Habitat', y='Longnose Dace CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

lnd.run <- ggplot(data, aes(x=data$X..Run, y=data$LND_CPUE)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm', color = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Percent Run Habitat', y='Longnose Dace CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

lnd.slow <- ggplot(data, aes(x=data$X..Slow, y=data$LND_CPUE)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm', color = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Percent Slow Moving\nHabitat (glide or pool)', y='Longnose Dace CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

LND_course <- ggplot(data, aes(x=data$X..Course, y=data$LND_CPUE)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm', color = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Percent Course Substrate\n(gravel or cobble)', y='Longnose Dace CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

lnd.slr.grid <- grid.arrange(lnd.T,lnd.width, lnd.dep, lnd.vel, lnd.riff, lnd.run,
                             lnd.slow,lnd.fine,LND_course, ncol=3, nrow=3)




#LND plots for figures
library(RColorBrewer)
display.brewer.all()
#plots with sig relationships = "Paired"
names(data)
class(data$LND)
data$LND <- as.factor(data$LND)

#Temp
lnd.logT.box <- ggplot(data, aes(LND, data$Temp, fill = LND)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Paired") +
  labs(y='Stream Temperature\n(Celcius)') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))

lnd.logT.box ##pub standard Boxplot

#Width
lnd.logW.box <- ggplot(data, aes(LND, data$mWidth, fill = LND)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Paired") +
  labs(y='Mean Wetted Width\n(m)') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))

lnd.logW.box ##pub standard Boxplot

#Depth
lnd.logD.box <- ggplot(data, aes(LND, data$mDepth, fill = LND)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Paired") +
  labs(y='Depth\n(m)') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))

lnd.logD.box ##pub standard Boxplot

#Velocity
lnd.logV.box <- ggplot(data, aes(LND, data$mVelocity, fill = LND)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Paired") +
  labs(y='In-Current Velocity\n(m/sec)') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))

lnd.logV.box ##pub standard Boxplot

#%Fine
lnd.logFine.box <- ggplot(data, aes(LND, data$X..Fine, fill = LND)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Paired") +
  labs(y='Percent Fine Substrate\n(clay, silt, sand)') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))

lnd.logFine.box ##pub standard Boxplot

##LND plots without sig relationships = "Greys"
#LND: riffle, run, slow, course

#%Riffle
lnd.logriff.box <- ggplot(data, aes(LND, data$X..Riffle, fill = LND)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Greys") +
  labs(y='Percent Riffle Habitat') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))
lnd.logriff.box

#%Run
lnd.logrun.box <- ggplot(data, aes(LND, data$X..Run, fill = LND)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Greys") +
  labs(y='Percent Run Habitat') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))
lnd.logrun.box

#%Slow
lnd.logslow.box <- ggplot(data, aes(LND, data$X..Slow, fill = LND)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Greys") +
  labs(y='Percent Slow Moving\nHabitat (pool or glide)') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))
lnd.logslow.box

#%Course
lnd.logcourse.box <- ggplot(data, aes(LND, data$X..Course, fill = LND)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Greys") +
  labs(y='Percent Course Substrate\n(gravel or cobble)') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))
lnd.logcourse.box

##GRID PLOT##

#### RESET = par(mfrow = c(1,1))###
## old_par <- par()
## plot(old_par)
#old_par <- par()

#install.packages("gridExtra")
lnd.grid <- grid.arrange(lnd.logT.box,lnd.logW.box,lnd.logD.box,
             lnd.logV.box, lnd.logriff.box, lnd.logrun.box,
             lnd.logslow.box, lnd.logFine.box, lnd.logcourse.box,ncol=3, nrow=3)


###################### Brook Trout ###############################

#calculate Brook Trout CPUE in fish per hour
names(data)
data$BKT <- as.factor(data$BKT)

data$bkt_cpue <- (data$BKT../effort$EFhours)
data$bkt_cpue


###### SLR #######
bkt_mod1 <- lm(data$logTemp ~ data$bkt_cpue) #P=
summary(bkt_mod1)

bkt_mod2 <- lm(data$logmWidth ~ data$bkt_cpue) 
summary(bkt_mod2)

bkt_mod3 <- lm(data$logmDepth ~ data$bkt_cpue) 
summary(bkt_mod3)

bkt_mod4 <- lm(data$logmVelocity ~ data$bkt_cpue)
summary(bkt_mod4)

bkt_mod5 <- lm(data$logRiffle ~ data$bkt_cpue) 
summary(bkt_mod5)

bkt_mod6 <- lm(data$logRun ~ data$bkt_cpue)
summary(bkt_mod6)

bkt_mod7 <- lm(data$logSlow ~ data$bkt_cpue)
summary(bkt_mod7)

bkt_mod8 <- lm(data$logFine ~ data$bkt_cpue) #P=
summary(bkt_mod8)

bkt_mod9 <- lm(data$logCourse ~ data$bkt_cpue)
summary(bkt_mod9)


######CPUE plots#####

#temp
bkt.T <- ggplot(data, aes(x=data$Temp, y=data$bkt_cpue)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm', color = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Stream Temperature\n(Celcius)', y='Brook Trout CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")


bkt.wid <- ggplot(data, aes(x=data$mWidth, y=data$bkt_cpue)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm', color = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Mean Wetted Width\n(m)', y='Brook Trout CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

bkt.dep <- ggplot(data, aes(x=data$mDepth, y=data$bkt_cpue)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm', color = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Depth\n(m)', y='Brook Trout CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

bkt.vel <- ggplot(data, aes(x=data$mVelocity, y=data$bkt_cpue)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm', color = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='In-Currrent Velocity\n(m/sec)', y='Brook Trout CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

bkt.riff <- ggplot(data, aes(x=data$X..Riffle, y=data$bkt_cpue)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm', color = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Percent Riffle Habitat', y='Brook Trout CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

bkt.run <- ggplot(data, aes(x=data$X..Run, y=data$bkt_cpue)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm', color = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Percent Run Habitat', y='Brook Trout CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

bkt.slow <- ggplot(data, aes(x=data$X..Slow, y=data$bkt_cpue)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm', color = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Percent Slow Moving\nHabitat (glide or pool)', y='Brook Trout CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

bkt.fine <- ggplot(data, aes(x=data$X..Fine, y=data$bkt_cpue)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm', color = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Percent Fine Substrate\n(clay, silt, sand)', y='Brook Trout CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

BKT_c <- ggplot(data, aes(x=data$X..Course, y=data$bkt_cpue)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm', color = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Percent Course Substrate\n(gravel or cobble)', y='Brook Trout CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

bkt.slr.grid <- grid.arrange(bkt.T,bkt.wid,bkt.dep,bkt.vel,bkt.riff,bkt.run,bkt.slow,
                             bkt.fine, BKT_c,ncol=3, nrow=3)




#BKT Boxplots
library(RColorBrewer)
display.brewer.all()
#plots with sig relationships = "Paired"
#plots without sig relationships = "Greys"
#Sig for BKT = temp, %slow

#Temp
bkt.temp.box <- ggplot(data, aes(BKT, data$Temp, fill = BKT)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Greens") +
  labs(y='Stream Temperature\n(Celcius)') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))
bkt.boxT


#Width
bkt.wid.box <- ggplot(data, aes(BKT, data$mWidth, fill = BKT)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Greys") +
  labs(y='Mean Wetted Width\n(m)') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))


#Depth
bkt.dep.box <- ggplot(data, aes(BKT, data$mDepth, fill = BKT)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Greys") +
  labs(y='Depth\n(m)') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))


#Velocity
bkt.vel.box <- ggplot(data, aes(BKT, data$mVelocity, fill = BKT)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Greys") +
  labs(y='In-Current Velocity\n(m/sec)') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))


#%Fine
bkt.fine.box <- ggplot(data, aes(BKT, data$X..Fine, fill = BKT)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Greys") +
  labs(y='Percent Fine Substrate\n(clay, silt, sand)') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))


#%Riffle
bkt.riff.box <- ggplot(data, aes(BKT, data$X..Riffle, fill = BKT)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Greys") +
  labs(y='Percent Riffle Habitat') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))

#%Run
bkt.run.box <- ggplot(data, aes(BKT, data$X..Run, fill = BKT)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Greys") +
  labs(y='Percent Run Habitat') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))

#%Slow
bkt.slow.box <- ggplot(data, aes(BKT, data$X..Slow, fill = BKT)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Greens") +
  labs(y='Percent Slow Moving Habitat\n(pool or glide)') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))


#%Course
bkt.course.box <- ggplot(data, aes(BKT, data$X..Course, fill = BKT)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Greys") +
  labs(y='Percent Course Substrate\n(gravel or cobble)') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))

##GRID PLOT##

#### RESET = par(mfrow = c(1,1))###
## old_par <- par()
## plot(old_par)
#old_par <- par()

#install.packages("gridExtra")
bkt.grid <- grid.arrange(bkt.temp.box,bkt.wid.box,bkt.dep.box,
                         bkt.vel.box,bkt.riff.box,bkt.run.box,
                         bkt.slow.box,bkt.fine.box,bkt.course.box,ncol=3, nrow=3)





##### BROWN TROUT #####
data$BRT <- as.factor(data$BRT)
names(data)
data$BRT_CPUE <- (data$BRT../effort$EFhours)
data$BRT_CPUE
######CPUE plots#####

#temp
brt.T <- ggplot(data, aes(x=data$Temp, y=data$BRT_CPUE)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm', color = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Stream Temperature\n(Celcius)', y='Brown Trout CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")


brt.wid <- ggplot(data, aes(x=data$mWidth, y=data$BRT_CPUE)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm')+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Mean Wetted Width\n(m)', y='Brown Trout CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

brt.dep <- ggplot(data, aes(x=data$mDepth, y=data$BRT_CPUE)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm', color = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Depth\n(m)', y='Brown Trout CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

brt.vel <- ggplot(data, aes(x=data$mVelocity, y=data$BRT_CPUE)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm', color = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='In-Currrent Velocity\n(m/sec)', y='Brown Trout CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

brt.riff <- ggplot(data, aes(x=data$X..Riffle, y=data$BRT_CPUE)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm')+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Percent Riffle Habitat', y='Brown Trout CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

brt.run <- ggplot(data, aes(x=data$X..Run, y=data$BRT_CPUE)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm', color = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Percent Run Habitat', y='Brown Trout CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

brt.slow <- ggplot(data, aes(x=data$X..Slow, y=data$BRT_CPUE)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm', color = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Percent Slow Moving\nHabitat (glide or pool)', y='Brown Trout CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

brt.fine <- ggplot(data, aes(x=data$X..Fine, y=data$BRT_CPUE)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm', color = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Percent Fine Substrate\n(clay, silt, sand)', y='Brown Trout CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

brt.course <- ggplot(data, aes(x=data$X..Course, y=data$BRT_CPUE)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm', color = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Percent Course Substrate\n(gravel or cobble)', y='Brown Trout CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

brt.slr.grid <- grid.arrange(brt.T,brt.wid,brt.dep,brt.vel,brt.riff,brt.run,brt.slow,
                             brt.fine,brt.course,ncol=3, nrow=3)



#BRT Boxplots
library(RColorBrewer)
display.brewer.all()
#plots with sig relationships = "Oranges"
#plots without sig relationships = "Greys"
#Sig for BRT = wid, dep, riffle, course

#Temp
brt.temp.box <- ggplot(data, aes(BRT, data$Temp, fill = BRT)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Greys") +
  labs(y='Stream Temperature\n(Celcius)') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))
brt.temp.box


#Width
brt.wid.box <- ggplot(data, aes(BRT, data$mWidth, fill = BRT)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Oranges") +
  labs(y='Mean Wetted Width\n(m)') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))


#Depth
brt.dep.box <- ggplot(data, aes(BRT, data$mDepth, fill = BRT)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Oranges") +
  labs(y='Depth\n(m)') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))


#Velocity
brt.vel.box <- ggplot(data, aes(BRT, data$mVelocity, fill = BRT)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Greys") +
  labs(y='In-Current Velocity\n(m/sec)') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))


#%Fine
brt.fine.box <- ggplot(data, aes(BRT, data$X..Fine, fill = BRT)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Greys") +
  labs(y='Percent Fine Substrate\n(clay, silt, sand)') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))


#%Riffle
brt.riff.box <- ggplot(data, aes(BRT, data$X..Riffle, fill = BRT)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Oranges") +
  labs(y='Percent Riffle Habitat') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))

#%Run
brt.run.box <- ggplot(data, aes(BRT, data$X..Run, fill = BRT)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Greys") +
  labs(y='Percent Run Habitat') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))

#%Slow
brt.slow.box <- ggplot(data, aes(BRT, data$X..Slow, fill = BRT)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Greys") +
  labs(y='Percent Slow Moving Habitat\n(pool or glide)') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))


#%Course
brt.course.box <- ggplot(data, aes(BRT, data$X..Course, fill = BRT)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Oranges") +
  labs(y='Percent Course Substrate\n(gravel or cobble)') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))


brt.grid <- grid.arrange(brt.temp.box,brt.wid.box,brt.dep.box,
                         brt.vel.box,brt.riff.box,brt.run.box,
                         brt.slow.box,brt.fine.box,brt.course.box,ncol=3, nrow=3)






#----------------------------#
### Southern redbelly dace ###
#----------------------------#

#add column for srd cpue
names(data)
class(data$SRD)
data$SRD <- as.factor(data$SRD)
class(data$SRD..)
data$SRD_CPUE <- (data$SRD../effort$EFhours)
data$SRD_CPUE

## ANOVA with Presence/Absence ##
AOV1_srd <- aov(data$logTemp ~ data$SRD)
summary(AOV1_srd) #5.293 0.0238 *
AOV2_srd <- aov(data$logmWidth ~ data$SRD)
summary(AOV2_srd) #0.138  0.711
AOV3_srd <- aov(data$logmDepth ~ data$SRD)
summary(AOV3_srd) #1.3  0.257
aov4.srd <- aov(data$logmVelocity ~ data$SRD)
summary(aov4.srd) #0.389  0.534
aov5.srd <- aov(data$logRiffle ~ data$SRD)
summary(aov5.srd) #0.278  0.599
aov6.srd <- aov(data$logRun ~ data$SRD)
summary(aov6.srd) #0.852  0.359
aov7.srd <- aov(data$logSlow ~ data$SRD)
summary(aov7.srd) #0.219  0.641
aov8.srd <- aov(data$logFine ~ data$SRD)
summary(aov8.srd) #0.965  0.329
aov9.srd <- aov(data$logCourse ~ data$SRD)
summary(aov9.srd) #0.199  0.657

###### SLR #######
srd_mod1 <- lm(data$logTemp ~ data$SRD_CPUE) 
summary(srd_mod1) #F-statistic: 5.225 on 1 and 86 DF,  p-value: 0.02472*

srd_mod2 <- lm(data$logmWidth ~ data$SRD_CPUE) 
summary(srd_mod2) #F-statistic: 2.463 on 1 and 86 DF,  p-value: 0.1202

srd_mod3 <- lm(data$logmDepth ~ data$SRD_CPUE) 
summary(srd_mod3) #F-statistic: 0.1798 on 1 and 86 DF,  p-value: 0.6726

srd_mod4 <- lm(data$logmVelocity ~ data$SRD_CPUE)
summary(srd_mod4) #F-statistic: 0.01951 on 1 and 86 DF,  p-value: 0.8893

srd_mod5 <- lm(data$logRiffle ~ data$SRD_CPUE) 
summary(srd_mod5) #F-statistic: 0.007055 on 1 and 86 DF,  p-value: 0.9333

srd_mod6 <- lm(data$logRun ~ data$SRD_CPUE)
summary(srd_mod6) #F-statistic: 0.3086 on 1 and 86 DF,  p-value: 0.58

srd_mod7 <- lm(data$logSlow ~ data$SRD_CPUE)
summary(srd_mod7) #F-statistic: 0.8841 on 1 and 86 DF,  p-value: 0.3497

srd_mod8 <- lm(data$logFine ~ data$SRD_CPUE) 
summary(srd_mod8) #F-statistic: 0.2411 on 1 and 86 DF,  p-value: 0.6247

srd_mod9 <- lm(data$logCourse ~ data$SRD_CPUE)
summary(srd_mod9) #F-statistic: 0.06115 on 1 and 86 DF,  p-value: 0.8053


######CPUE plots#####

srd.T <- ggplot(data, aes(x=data$Temp, y=data$SRD_CPUE)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm')+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Stream Temperature\n(Celcius)', y='Southern Redbelly Dace\nCPUE (fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")


srd.wid <- ggplot(data, aes(x=data$mWidth, y=data$SRD_CPUE)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm', color = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Mean Wetted Width\n(m)', y='Southern Redbelly Dace\nCPUE (fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

srd.dep <- ggplot(data, aes(x=data$mDepth, y=data$SRD_CPUE)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm', color = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Depth\n(m)', y='Southern Redbelly Dace\nCPUE (fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

srd.vel <- ggplot(data, aes(x=data$mVelocity, y=data$SRD_CPUE)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm', color = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='In-Currrent Velocity\n(m/sec)', y='Southern Redbelly Dace\nCPUE (fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

srd.riff <- ggplot(data, aes(x=data$X..Riffle, y=data$SRD_CPUE)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm', color="black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Percent Riffle Habitat', y='Southern Redbelly Dace\nCPUE (fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

srd.run <- ggplot(data, aes(x=data$X..Run, y=data$SRD_CPUE)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm', color = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Percent Run Habitat', y='Southern Redbelly Dace\nCPUE (fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

srd.slow <- ggplot(data, aes(x=data$X..Slow, y=data$SRD_CPUE)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm', color = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Percent Slow Moving\nHabitat (glide or pool)', y='Southern Redbelly Dace\nCPUE (fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

srd.fine <- ggplot(data, aes(x=data$X..Fine, y=data$SRD_CPUE)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm', color = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Percent Fine Substrate\n(clay, silt, sand)', y='Southern Redbelly Dace\nCPUE (fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

srd.course <- ggplot(data, aes(x=data$X..Course, y=data$SRD_CPUE)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm', color = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Percent Course Substrate\n(gravel or cobble)', y='Southern Redbelly Dace\nCPUE (fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

srd.slr.grid <- grid.arrange(srd.T,srd.wid,srd.dep,srd.vel,srd.riff,srd.run,srd.slow,
                             srd.fine,srd.course,ncol=3, nrow=3)



#SRD Boxplots
library(RColorBrewer)
display.brewer.all()
#plots with sig relationships = "Purples"
#plots without sig relationships = "Greys"
#Sig for SRD=Temp

#Temp
srd.temp.box <- ggplot(data, aes(SRD, data$Temp, fill = SRD)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Purples") +
  labs(y='Stream Temperature\n(Celcius)') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))
srd.temp.box


#Width
srd.wid.box <- ggplot(data, aes(SRD, data$mWidth, fill = SRD)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Greys") +
  labs(y='Mean Wetted Width\n(m)') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))


#Depth
srd.dep.box <- ggplot(data, aes(SRD, data$mDepth, fill = SRD)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Greys") +
  labs(y='Depth\n(m)') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))


#Velocity
srd.vel.box <- ggplot(data, aes(SRD, data$mVelocity, fill = SRD)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Greys") +
  labs(y='In-Current Velocity\n(m/sec)') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))


#%Fine
srd.fine.box <- ggplot(data, aes(SRD, data$X..Fine, fill = SRD)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Greys") +
  labs(y='Percent Fine Substrate\n(clay, silt, sand)') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))


#%Riffle
srd.riff.box <- ggplot(data, aes(SRD, data$X..Riffle, fill =SRD)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Greys") +
  labs(y='Percent Riffle Habitat') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))

#%Run
srd.run.box <- ggplot(data, aes(SRD, data$X..Run, fill = SRD)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Greys") +
  labs(y='Percent Run Habitat') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))

#%Slow
srd.slow.box <- ggplot(data, aes(SRD, data$X..Slow, fill = SRD)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Greys") +
  labs(y='Percent Slow Moving Habitat\n(pool or glide)') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))


#%Course
srd.course.box <- ggplot(data, aes(SRD, data$X..Course, fill = SRD)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Greys") +
  labs(y='Percent Course Substrate\n(gravel or cobble)') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))


srd.grid <- grid.arrange(srd.temp.box,srd.wid.box,srd.dep.box,
                         srd.vel.box,srd.riff.box,srd.run.box,
                         srd.slow.box,srd.fine.box,srd.course.box,ncol=3, nrow=3)


#-----------------------------#
#---------- Sculpins ---------#
#_____________________________#

data$Cottus <- as.factor(data$Cottus)
data$Cottus
data$cot_cpue <- (data$Cottus../effort$EFhours)
data$cot_cpue

## ANOVA with Presence/Absence ##
AOV1_cot <- aov(data$logTemp ~ data$Cottus)
summary(AOV1_cot) #0.278  0.599
AOV2_cot <- aov(data$logmWidth ~ data$Cottus)
summary(AOV2_cot) #5.971 0.0166 *
AOV3_cot <- aov(data$logmDepth ~ data$Cottus)
summary(AOV3_cot) # 1.913   0.17
aov4.cot <- aov(data$logmVelocity ~ data$Cottus)
summary(aov4.cot) #3.369 0.0699
aov5.cot <- aov(data$logRiffle ~ data$Cottus)
summary(aov5.cot) #0.24  0.626
aov6.cot <- aov(data$logRun ~ data$Cottus)
summary(aov6.cot) #1.348  0.249
aov7.cot <- aov(data$logSlow ~ data$Cottus)
summary(aov7.cot) #7.067 0.00936 **
aov8.cot <- aov(data$logFine ~ data$Cottus)
summary(aov8.cot) #0.37  0.544
aov9.cot <- aov(data$logCourse ~ data$Cottus)
summary(aov9.cot) #0.345  0.558

###### SLR #######
cot_mod1 <- lm(data$logTemp ~ data$cot_cpue) 
summary(cot_mod1) #F-statistic: 0.4544 on 1 and 86 DF,  p-value: 0.502

cot_mod2 <- lm(data$logmWidth ~ data$cot_cpue) 
summary(cot_mod2) #F-statistic: 4.828 on 1 and 86 DF,  p-value: 0.03069*

cot_mod3 <- lm(data$logmDepth ~ data$cot_cpue) 
summary(cot_mod3) #F-statistic: 0.3342 on 1 and 86 DF,  p-value: 0.5647

cot_mod4 <- lm(data$logmVelocity ~ data$cot_cpue)
summary(cot_mod4) #F-statistic: 0.4904 on 1 and 86 DF,  p-value: 0.4856

cot_mod5 <- lm(data$logRiffle ~ data$cot_cpue) 
summary(cot_mod5) #F-statistic: 1.084 on 1 and 86 DF,  p-value: 0.3007

cot_mod6 <- lm(data$logRun ~ data$cot_cpue)
summary(cot_mod6) #F-statistic: 1.57 on 1 and 86 DF,  p-value: 0.2136

cot_mod7 <- lm(data$logSlow ~ data$cot_cpue)
summary(cot_mod7) #F-statistic: 1.958 on 1 and 86 DF,  p-value: 0.1654

cot_mod8 <- lm(data$logFine ~ data$cot_cpue) 
summary(cot_mod8) #F-statistic: 0.007911 on 1 and 86 DF,  p-value: 0.9293

cot_mod9 <- lm(data$logCourse ~ data$cot_cpue)
summary(cot_mod9) #F-statistic: 0.1059 on 1 and 86 DF,  p-value: 0.7456


######CPUE plots#####

cot_T <- ggplot(data, aes(x=data$Temp, y=data$cot_cpue)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm', color = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Stream Temperature\n(Celcius)', y='Sculpin CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")
cot_T


cot_wid <- ggplot(data, aes(x=data$mWidth, y=data$cot_cpue)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm')+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Mean Wetted Width\n(m)', y='Sculpin CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

cot_dep <- ggplot(data, aes(x=data$mDepth, y=data$cot_cpue)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm', color = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Depth\n(m)', y='Sculpin CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

cot_vel <- ggplot(data, aes(x=data$mVelocity, y=data$cot_cpue)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm', color = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='In-Currrent Velocity\n(m/sec)', y='Sculpin CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

cot_riff <- ggplot(data, aes(x=data$X..Riffle, y=data$cot_cpue)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm', color="black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Percent Riffle Habitat', y='Sculpin CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

cot_run <- ggplot(data, aes(x=data$X..Run, y=data$cot_cpue)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm', color = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Percent Run Habitat', y='Sculpin CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

cot_slow <- ggplot(data, aes(x=data$X..Slow, y=data$cot_cpue)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm', color = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Percent Slow Moving\nHabitat (glide or pool)', y='Sculpin CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

cot_fine <- ggplot(data, aes(x=data$X..Fine, y=data$cot_cpue)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm', color = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Percent Fine Substrate\n(clay, silt, sand)', y='Sculpin CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

cot_course <- ggplot(data, aes(x=data$X..Course, y=data$cot_cpue)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm', color = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Percent Course Substrate\n(gravel or cobble)', y='Sculpin CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

cot_slr_grid <- grid.arrange(cot_T,cot_wid,cot_dep,cot_vel,cot_riff,cot_run,cot_slow,
                             cot_fine,cot_course,ncol=3, nrow=3)



#COTTUS Boxplots
library(RColorBrewer)
display.brewer.all()
#plots with sig relationships = "Reds"
#plots without sig relationships = "Greys"
#Sig for Sculpin=mWid, %Slow

#Temp
cot.temp.box <- ggplot(data, aes(Cottus, data$Temp, fill = Cottus)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Greys") +
  labs(y='Stream Temperature\n(Celcius)') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))
cot.temp.box


#Width
cot.wid.box <- ggplot(data, aes(Cottus, data$mWidth, fill = Cottus)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Reds") +
  labs(y='Mean Wetted Width\n(m)') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))


#Depth
cot.dep.box <- ggplot(data, aes(Cottus, data$mDepth, fill = Cottus)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Greys") +
  labs(y='Depth\n(m)') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))


#Velocity
cot.vel.box <- ggplot(data, aes(Cottus, data$mVelocity, fill = Cottus)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Greys") +
  labs(y='In-Current Velocity\n(m/sec)') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))


#%Fine
cot.fine.box <- ggplot(data, aes(Cottus, data$X..Fine, fill = Cottus)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Greys") +
  labs(y='Percent Fine Substrate\n(clay, silt, sand)') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))


#%Riffle
cot.riff.box <- ggplot(data, aes(Cottus, data$X..Riffle, fill =Cottus)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Greys") +
  labs(y='Percent Riffle Habitat') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))

#%Run
cot.run.box <- ggplot(data, aes(Cottus, data$X..Run, fill = Cottus)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Greys") +
  labs(y='Percent Run Habitat') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))

#%Slow
cot.slow.box <- ggplot(data, aes(Cottus, data$X..Slow, fill = Cottus)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Reds") +
  labs(y='Percent Slow Moving Habitat\n(pool or glide)') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))


#%Course
cot.course.box <- ggplot(data, aes(Cottus, data$X..Course, fill = Cottus)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Greys") +
  labs(y='Percent Course Substrate\n(gravel or cobble)') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))


cot.grid <- grid.arrange(cot.temp.box,cot.wid.box,cot.dep.box,
                         cot.vel.box,cot.riff.box,cot.run.box,
                         cot.slow.box,cot.fine.box,cot.course.box,ncol=3, nrow=3)





















