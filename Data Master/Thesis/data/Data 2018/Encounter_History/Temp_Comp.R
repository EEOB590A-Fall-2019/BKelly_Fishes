#Comparison of temperatures from 3 site types:
#Brook Trout Sites, BRT/RBT sites, Non-Trout Sites

#load data
tdata <- as.data.frame(read.csv(file.choose(), header=T))
tdata

##NEW DATA FORMAT WITH THREE GROUPS 
##0=BKT, 1=BRT/RBT, 2=TROUTLESS


library(ggplot2)

class(tdata$Group) #convert group to factor
tdata$Site_Type <- as.factor(tdata$Group)

#Boxplot of data
#########################################################################################################################

#boxplot of SGCN presence and Site Species Richness
library(ggplot2)
#install.packages("ggplot2")

temp_comp.box <- ggplot(tdata, aes(Site_Type, tdata$Temp, fill = Site_Type)) + geom_boxplot(width=.35, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Accent") +
  labs(x='Site Type', y='Stream Temperature\n(Celcius)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Brook Trout","Brown Trout","Troutless"))


temp_comp.box ##pub standard Boxplot
##########################################################################################################################





#subset data into individual groups
names(group)

bkt <- group[group$fish=="0",]
bkt

brt <- group[group$fish=="1",]
brt

non <- group[group$fish=="2",]
non

#plot subsetted data by group
hist(bkt$Temp)
hist(brt$Temp)
hist(non$Temp)

#log transform Temps for bkt 
bkt$Temp <- log(bkt$Temp)
bkt

#view log transformed data for other 2 groups
hist(log(brt$Temp))
hist(log(non$Temp))

#transform all data
brt$Temp <- log(brt$Temp)
non$Temp <- log(non$Temp)

group$Temp <- log(group$Temp)

#Parametric tests: ANOVA

#Ho: mean temp at time of sampling is the same for all site types
aov1 <- aov(group$Temp ~ group$fish)
summary(aov1)

TukeyHSD(aov1)
plot(TukeyHSD(aov1), las=1)



summary(plot1)
summary(bkt$Temp)


#Report median/mean temp values for each site type (backtransformed)
original <- as.data.frame(read.csv(file.choose(), header=T))
names(original)

median(original$Temp[original$Group=="0"]) #14.35 - BKT Sites
median(original$Temp[original$Group=="1"]) #14.80 - BRT Sites
median(original$Temp[original$Group=="2"]) #15.95 - NON-TROUT Sites

mean(original$Temp[original$Group=="0"]) #13.79 -BKT
mean(original$Temp[original$Group=="1"]) #15.63 -BRT
mean(original$Temp[original$Group=="2"]) #16.30 -NON

#new boxplot
plot2 <- ggplot(group, aes(fish, Temp, fill=fish)) + geom_boxplot(width=.35, notch=F)+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plot2 + xlab("Site Type\n(0 = Brook Trout, 1 = Brown Trout, 2 = Troutless)") + 
  ylab("log10 Stream Temperature\n(Celcius)") +
  ggtitle("log Stream Temperature at Time of Sampling")

#Original Data Plot
class(group$Group)
original$SiteType <- as.factor(original$Group)
class(original$SiteType)
plot3 <- ggplot(original, aes(SiteType, Temp, fill=SiteType)) + geom_boxplot(width=.35, notch=F)+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plot3 + xlab("Site Type\n(0 = Brook Trout, 1 = Brown Trout, 2 = Troutless)") + 
  ylab("Stream Temperature\n(Celcius)") +
  ggtitle("Stream Temperature at Time of Sampling")



max(original$Temp[original$SiteType=="0"]) #15.9






