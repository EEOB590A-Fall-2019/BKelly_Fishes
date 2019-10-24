#------------------------------------#
#SGCN-Habitat analysis 2018----------#
#____________________________________#

#read in sgcn data and then check data
sgcn <- as.data.frame(read.csv(file.choose()))
sgcn
names(sgcn)

#log transform hab variables
sgcn$logTemp <- log(sgcn$Temp)
sgcn$logmWidth <- log(sgcn$mWidth)
sgcn$logmDepth <- log(sgcn$mDepth)
sgcn$logmVelocity <- log(sgcn$mVelocity)
sgcn$logRiffle <- log(1 + sgcn$X..Riffle)
sgcn$logRun <- log(1 + sgcn$X..Run)
sgcn$logSlow <- log(1 + sgcn$X..Slow)
sgcn$logFine <- log(1 + sgcn$X..Fine)
sgcn$logCourse <- log(1 + sgcn$X..Course)
names(sgcn)

#convert to factor
class(sgcn$SGCN)
sgcn$SGCN <- as.factor(sgcn$SGCN)
class(sgcn$SGCN)


#read in electrofishing hours data
effort <- as.data.frame(read.csv(file.choose()))
effort

#create sgcn-cpue column
sgcn$cpue <- (sgcn$SGCN_ab/effort$effort_hrs)
sgcn$cpue


#ANOVA for SGCN presence and hab. variables
AOV1_sg <- aov(sgcn$logTemp ~ sgcn$SGCN)
summary(AOV1_sg) 
#             Df Sum Sq Mean Sq F value Pr(>F)
#sgcn$SGCN    1 0.0582 0.05821   1.791  0.184

AOV2_sg <- aov(sgcn$logmWidth ~ sgcn$SGCN) 
summary(AOV2_sg) 
#             Df Sum Sq Mean Sq F value Pr(>F)  
#sgcn$SGCN    1  0.794  0.7943    3.76 0.0558

AOV3_sg <- aov(sgcn$logmDepth ~ sgcn$SGCN) #*
summary(AOV3_sg) 
#               Df Sum Sq Mean Sq F value Pr(>F)  
#sgcn$SGCN    1  0.886  0.8857   6.809 0.0107 *

aov4_sg <- aov(sgcn$logmVelocity ~ sgcn$SGCN)
summary(aov4_sg) 
#             Df Sum Sq Mean Sq F value Pr(>F)  
#sgcn$SGCN    1  0.735  0.7349   2.926 0.0908 .

aov5_sg <- aov(sgcn$logRiffle ~ sgcn$SGCN) 
summary(aov5_sg) 
#             Df Sum Sq Mean Sq F value Pr(>F)
#sgcn$SGCN    1   0.21  0.2067    0.29  0.592

aov6_sg <- aov(sgcn$logRun ~ sgcn$SGCN)
summary(aov6_sg) 
#             Df Sum Sq Mean Sq F value Pr(>F)
#sgcn$SGCN    1   0.14  0.1389   0.228  0.635

aov7_sg <- aov(sgcn$logSlow ~ sgcn$SGCN) #*
summary(aov7_sg) 
#              Df Sum Sq Mean Sq F value Pr(>F)  
#sgcn$SGCN    1  10.59  10.590   5.709 0.0191 *

aov8_sg <- aov(sgcn$logFine ~ sgcn$SGCN)
summary(aov8_sg) 
#             Df Sum Sq Mean Sq F value Pr(>F)
#sgcn$SGCN    1   4.49   4.486   2.251  0.137

aov9_sg <- aov(sgcn$logCourse ~ sgcn$SGCN) 
summary(aov9_sg) 
#             Df Sum Sq Mean Sq F value Pr(>F)
#sgcn$SGCN    1   0.05  0.0502   0.107  0.745



#____________________
#     Boxplots
#____________________

library(RColorBrewer)
library(ggplot2)

#Temp
tbox <- ggplot(sgcn, aes(SGCN, sgcn$Temp, fill = SGCN)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Greys") +
  labs(y='Stream Temperature\n(Celcius)') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))

tbox ##pub standard Boxplot

#Width
wid_box <- ggplot(sgcn, aes(SGCN, sgcn$mWidth, fill = SGCN)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Greys") +
  labs(y='Mean Wetted Width\n(m)') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))


#Depth
dep_box <- ggplot(sgcn, aes(SGCN, sgcn$mDepth, fill = SGCN)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Reds") +
  labs(y='Depth\n(m)') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))


#Velocity
Vbox <- ggplot(sgcn, aes(SGCN, sgcn$mVelocity, fill = SGCN)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Greys") +
  labs(y='In-Current Velocity\n(m/sec)') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))


#%Fine
fine_box <- ggplot(sgcn, aes(SGCN, sgcn$X..Fine, fill = SGCN)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Greys") +
  labs(y='Percent Fine Substrate\n(clay, silt, sand)') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))


#%Riffle
riff_box <- ggplot(sgcn, aes(SGCN, sgcn$X..Riffle, fill = SGCN)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Greys") +
  labs(y='Percent Riffle Habitat') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))


#%Run
run_box <- ggplot(sgcn, aes(SGCN, sgcn$X..Run, fill = SGCN)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Greys") +
  labs(y='Percent Run Habitat') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))


#%Slow
slow_box <- ggplot(sgcn, aes(SGCN, sgcn$X..Slow, fill = SGCN)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Reds") +
  labs(y='Percent Slow Moving\nHabitat (pool or glide)') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))


#%Course
course_box <- ggplot(sgcn, aes(SGCN, sgcn$X..Course, fill = SGCN)) + geom_boxplot(width=.50, notch=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Greys") +
  labs(y='Percent Coarse Substrate\n(gravel or cobble)') +
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_x_discrete(labels = c("Absent","Present"))


##GRID PLOT##

#install.packages("gridExtra")
library(gridExtra)
sgcn_grid <- grid.arrange(tbox,wid_box,dep_box,Vbox,riff_box,
                          run_box,slow_box,fine_box,course_box,ncol=3, nrow=3)



###### SLR for SGCN_sp #######
sgcn_mod1 <- lm(sgcn$logTemp ~ sgcn$SGCN_sp)
summary(sgcn_mod1) #F-statistic: 0.7603 on 1 and 86 DF,  p-value: 0.3857

sgcn_mod2 <- lm(sgcn$logmWidth ~ sgcn$SGCN_sp) 
summary(sgcn_mod2) #F-statistic: 11.56 on 1 and 86 DF,  p-value: 0.001022***

sgcn_mod3 <- lm(sgcn$logmDepth ~ sgcn$SGCN_sp) 
summary(sgcn_mod3) #F-statistic: 8.824 on 1 and 86 DF,  p-value: 0.003852***

sgcn_mod4 <- lm(sgcn$logmVelocity ~ sgcn$SGCN_sp)
summary(sgcn_mod4) #F-statistic: 0.5249 on 1 and 86 DF,  p-value: 0.4707

sgcn_mod5 <- lm(sgcn$logRiffle ~ sgcn$SGCN_sp) 
summary(sgcn_mod5) #F-statistic: 0.05224 on 1 and 86 DF,  p-value: 0.8198

sgcn_mod6 <- lm(sgcn$logRun ~ sgcn$SGCN_sp)
summary(sgcn_mod6) #F-statistic: 0.1211 on 1 and 86 DF,  p-value: 0.7287

sgcn_mod7 <- lm(sgcn$logSlow ~ sgcn$SGCN_sp)
summary(sgcn_mod7) #F-statistic: 2.074 on 1 and 86 DF,  p-value: 0.1535

sgcn_mod8 <- lm(sgcn$logFine ~ sgcn$SGCN_sp)
summary(sgcn_mod8) #F-statistic: 1.206 on 1 and 86 DF,  p-value: 0.2753

sgcn_mod9 <- lm(sgcn$logCourse ~ sgcn$SGCN_sp)
summary(sgcn_mod9) #F-statistic: 0.6818 on 1 and 86 DF,  p-value: 0.4112



#----SLR plots for SGCN_sp----#
mod1 <- ggplot(sgcn, aes(x=sgcn$Temp, y=sgcn$SGCN_sp)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm', color="black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Stream Temperature\n(Celcius)', y='SGCN Richness') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")
mod1


mod2 <- ggplot(sgcn, aes(x=sgcn$mWidth, y=sgcn$SGCN_sp)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm')+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Mean Wetted Width\n(m)', y='SGCN Richness') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

mod3 <- ggplot(sgcn, aes(x=sgcn$mDepth, y=sgcn$SGCN_sp)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm')+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Mean Depth\n(m)', y='SGCN Richness') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

mod4 <- ggplot(sgcn, aes(x=sgcn$mVelocity, y=sgcn$SGCN_sp)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm',colour = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='In-Current Velocity\n(m/sec)', y='SGCN Richness') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

mod5 <- ggplot(sgcn, aes(x=sgcn$X..Riffle, y=sgcn$SGCN_sp)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm',colour = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Percent Riffle Habitat', y='SGCN Richness') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

mod6 <- ggplot(sgcn, aes(x=sgcn$X..Run, y=sgcn$SGCN_sp)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm',colour = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Percent Run Habitat', y='SGCN Richness') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

mod7 <- ggplot(sgcn, aes(x=sgcn$X..Slow, y=sgcn$SGCN_sp)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm',colour = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Percent Slow Moving\n Habitat (pool or glide)', y='SGCN Richness') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

mod8 <- ggplot(sgcn, aes(x=sgcn$X..Fine, y=sgcn$SGCN_sp)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm',colour = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Percent Fine Substrate\n(clay, silt, sand)', y='SGCN Richness') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

mod9 <- ggplot(sgcn, aes(x=sgcn$X..Course, y=sgcn$SGCN_sp)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm',colour = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Percent Coarse Substrate\n(gravel or cobble)', y='SGCN Richness') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

sgcn_sp_grid <- grid.arrange(mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8,mod9, ncol=3, nrow=3)


#----------------------
#SLR for sgcn_cpue-----
#----------------------

sgcn_mod10 <- lm(sgcn$logTemp ~ sgcn$SGCN_ab)
summary(sgcn_mod10) #F-statistic: 0.01786 on 1 and 86 DF,  p-value: 0.894

sgcn_mod11 <- lm(sgcn$logmWidth ~ sgcn$SGCN_ab) 
summary(sgcn_mod11) #F-statistic:11.26 on 1 and 86 DF,  p-value: 0.001181*** 

sgcn_mod12 <- lm(sgcn$logmDepth ~ sgcn$SGCN_ab) 
summary(sgcn_mod12) #F-statistic: 2.907 on 1 and 86 DF,  p-value: 0.0918

sgcn_mod13 <- lm(sgcn$logmVelocity ~ sgcn$SGCN_ab)
summary(sgcn_mod13) #F-statistic: 0.07808 on 1 and 86 DF,  p-value: 0.7806

sgcn_mod14 <- lm(sgcn$logRiffle ~ sgcn$SGCN_ab) 
summary(sgcn_mod14) #F-statistic: 1.472 on 1 and 86 DF,  p-value: 0.2283

sgcn_mod15 <- lm(sgcn$logRun ~ sgcn$SGCN_ab)
summary(sgcn_mod15) #F-statistic: 1.565 on 1 and 86 DF,  p-value: 0.2143

sgcn_mod16 <- lm(sgcn$logSlow ~ sgcn$SGCN_ab)
summary(sgcn_mod16) #F-statistic: 4.991 on 1 and 86 DF,  p-value: 0.02807*

sgcn_mod17 <- lm(sgcn$logFine ~ sgcn$SGCN_ab)
summary(sgcn_mod17) #F-statistic: 0.8595 on 1 and 86 DF,  p-value: 0.3565

sgcn_mod18 <- lm(sgcn$logCourse ~ sgcn$SGCN_ab)
summary(sgcn_mod18) #F-statistic: 0.7095 on 1 and 86 DF,  p-value: 0.4019


#_____________________________#
#----SLR plots for SGCN_sp----#
#_____________________________#


mod10 <- ggplot(sgcn, aes(x=sgcn$Temp, y=sgcn$SGCN_ab)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm', color="black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Stream Temperature\n(Celcius)', y='SGCN CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")
mod10


mod11 <- ggplot(sgcn, aes(x=sgcn$mWidth, y=sgcn$SGCN_ab)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm')+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Mean Wetted Width\n(m)', y='SGCN CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

mod12 <- ggplot(sgcn, aes(x=sgcn$mDepth, y=sgcn$SGCN_ab)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm', color="black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Mean Depth\n(m)', y='SGCN CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

mod13 <- ggplot(sgcn, aes(x=sgcn$mVelocity, y=sgcn$SGCN_ab)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm',colour = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='In-Current Velocity\n(m/sec)', y='SGCN CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

mod14 <- ggplot(sgcn, aes(x=sgcn$X..Riffle, y=sgcn$SGCN_ab)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm',colour = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Percent Riffle Habitat', y='SGCN CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

mod15 <- ggplot(sgcn, aes(x=sgcn$X..Run, y=sgcn$SGCN_ab)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm',colour = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Percent Run Habitat', y='SGCN CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

mod16 <- ggplot(sgcn, aes(x=sgcn$X..Slow, y=sgcn$SGCN_ab)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm')+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Percent Slow Moving\n Habitat (pool or glide)', y='SGCN CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

mod17 <- ggplot(sgcn, aes(x=sgcn$X..Fine, y=sgcn$SGCN_ab)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm',colour = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Percent Fine Substrate\n(clay, silt, sand)', y='SGCN CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

mod18 <- ggplot(sgcn, aes(x=sgcn$X..Course, y=sgcn$SGCN_ab)) +geom_point()+
  geom_jitter() + geom_smooth(method = 'lm',colour = "black", se=F)+
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  labs(x='Percent Coarse Substrate\n(gravel or cobble)', y='SGCN CPUE\n(fish per hour)') +
  theme(axis.title.x = element_text(size = 15, color = 'black'), axis.title.y = element_text(size = 15, color = 'black'),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  theme(legend.position = "none")

sgcn_ab_grid <- grid.arrange(mod10,mod11,mod12,mod13,mod14,mod15,mod16,mod17,mod18, ncol=3, nrow=3)















