rich <- as.data.frame(read.csv(file.choose(), header=T))
rich
cor(data1$Trout.Presence, data1$SGCN.Presence)
mod <- lm(data1$SGCN.Presence ~ data1$Trout.Presence)
summary(mod)


data2 <- read.csv(file.choose(), header=T)
data2

rich <- lm(data2$SGCN.Presence ~ data2$Richness)
summary(rich)
rich2 <-lm(data2$Richness ~ data2$SGCN.Presence)
summary(rich2)
plot(rich2)

plot(data2$Richness, data2$SGCN.Presence)
plot(data2$SGCN.Presence, data2$Richness)


rich #richness and SGCN presence linear model

temp <- lm(data2$SGCN.Presence ~ data2$Temp)
temp
summary(temp)
plot(data2$SGCN.Presence, data2$Temp)
data2$Temp
min(data2$Temp)


class(data2$Trout.Presence)
class(data2$SGCN.Presence)
class(data2$Richness)
class(data2$Temp)

newdat <- data.frame(data2)
newdat
class(newdat)
is.factor(newdat$SGCN.Presence)
is.factor(newdat$Trout.Presence)
class(newdat$SGCN.Presence)
as.factor(newdat$Trout.Presence)
as.factor(newdat$SGCN.Presence)
newdat$SGCN <- as.factor(newdat$SGCN.Presence)
is.factor(newdat$SGCN)
newdat$Trout <- as.factor(newdat$Trout.Presence)
is.factor(newdat$Trout)
is.numeric(newdat$Temp)
class(newdat$Temp)
class(newdat$Richness)
newdat$temp <- as.numeric(newdat$Temp)
class(newdat$temp)
class(newdat$Richness)

plot(newdat$Trout, newdat$temp)
plot(newdat$SGCN, newdat$temp)
plot(newdat$SGCN, newdat$Richness, main= 'Richness')


newmod <- lm(newdat$Richness ~ newdat$SGCN)
summary(newmod)

rich$SGCN <- as.factor(rich$SGCN)
names(rich)
#boxplot of SGCN presence and Site Species Richness
library(ggplot2)
install.packages("ggplot2")
rich_sgcn <- ggplot(rich, aes(SGCN, Richness, fill=SGCN)) + geom_boxplot(width=.35, notch=T)+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("Site Species Richness")+
  xlab("SGCN Status") +
  theme(axis.title.x =element_text(size = 14)) + theme(axis.title.y = element_text(size = 14)) + 
  theme(legend.position = c(.10,.90), legend.justification = c(.10,.90)) +
  theme(legend.key = element_blank())+
  scale_x_discrete(labels = c("Absent","Present"))
rich_sgcn
#------------------------------------------------------------------------------
rich_sgcn.box <- ggplot(rich, aes(rich$SGCN, rich$Richness, fill = SGCN)) + geom_boxplot(width=.35, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Accent") +
  labs(x='SGCN Status', y='Species Richness') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))

rich_sgcn.box





#Boxplot for Trout Sites and Temp @ Time of Sampling
temp <- as.data.frame(read.csv(file.choose(), header=T))
names(temp)
class(temp$Trout.Presence)
temp$Trout <- as.factor(temp$Trout.Presence)
class(temp$Temp)
?boxplot()
boxplot(Temp ~ Trout, data = temp)
box2 <- ggplot(temp, aes(Trout, Temp, fill=Trout)) + geom_boxplot(width=.35, notch = T)
box2

?aov
#Ho: mean temp at time of sampling is the same for sites with and without trout

ANOVA1 <- aov(temp$Temp ~ temp$Trout)
ANOVA1
summary(ANOVA1)
#P = 0.0584 ----- unequal sample size, presence of outliers -- Should we accept this result?

ttest1 <- t.test(temp$Temp ~ temp$Trout)
ttest1

#Remove outliers from Temperature data
tempnew <- temp[-c(23,68,73,86),] 
boxplot(Temp ~ Trout, data=tempnew)

ANOVA2 <- aov(tempnew$Temp ~ tempnew$Trout)
summary(ANOVA2)

ttest2 <- t.test(tempnew$Temp ~ tempnew$Trout)
ttest2

Box3 <- ggplot(tempnew, aes(Trout, Temp, fill=Trout)) + geom_boxplot(width=.35, notch = T)
Box3

#Rerun analysis with log transformed data

#create log transformed temps on temp datafile
temp
temp$logT <- log(temp$Temp)
names(temp)

#Plot new log transformed data
?hist()
hist(temp$logT)


#rerun analysis

#T.Test
ttest3 <- t.test(temp$logT ~ temp$Trout)
ttest3 #P=.06007
#ANOVA
ANOVA3 <- aov(temp$logT ~ temp$Trout)
summary(ANOVA3) #P=.0577

#Boxplot of logTemp vs Trout Presence - final below 
logbox <- ggplot(temp, aes(Trout, logT, fill=Trout)) + geom_boxplot(width=.35, notch=F) +
  ggtitle("Stream Temperature at Time of Sampling") +
  xlab("Trout Presence\n(0=Absent, 1=Present)") + ylab("Log Temperature\n(Celcius)") +
  theme(plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor=element_blank())
  
logbox


#boxplot of SGCN presence and Site Species Richness
library(ggplot)
#install.packages("ggplot2")
Box <- ggplot(newdat, aes(SGCN, Richness, fill=SGCN)) + geom_boxplot(width=.35, notch=F)+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
Box
Box + ylim(0,15) 
Box + ylab("Number of Observed Species") + xlab("SGCN Presence\n(0=Absent, 1=Present)") +
  ggtitle("Site Specific Species Richness")


#new analysis for report

hist(data1$Richness) #checking for normality
hist(data1$SGCN.Presence)

data1$logRich <- log(1 + data1$Richness) #transform richness data

class(data1$SGCN.Presence)
data1$SGCN <- as.factor(data1$SGCN.Presence) #convert to factor from integer

aov_SGCN_Richness <- aov(data1$logRich ~ data1$SGCN)
summary(aov_SGCN_Richness)


#color brewer####################
#install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all()
#########################################################################################################################

#boxplot of SGCN presence and Site Species Richness
library(ggplot2)
#install.packages("ggplot2")

sgcn.box <- ggplot(data1, aes(SGCN, logRich, fill = SGCN)) + geom_boxplot(width=.35, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Accent") +
  labs(x='Species of Greatest Conservation Need\n(0=Absent, 1=Present)', y='log Species Richness') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")
  
  
sgcn.box ##pub standard Boxplot
##########################################################################################################################


Box + ylim(0,15) 
Box + ylab("Number of Observed Species") + xlab("SGCN Presence\n(0=Absent, 1=Present)") +
  ggtitle("Site Specific Species Richness")



