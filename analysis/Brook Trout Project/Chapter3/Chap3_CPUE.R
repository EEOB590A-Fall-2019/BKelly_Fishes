#Chapter three statistical analysis:
##Adundace comparisons and models

library(tidyverse)
library(skimr)
library(cowplot)
library(coin)

#Terms to define:
#LND = Longnose Dace
#SRD = Southern Redbelly Dace
#Cottus = the genus of Sculpins 
#BRT = Brown Trout
#SGCN = Species of Greatest Conservation Need
#CPUE = Catch Per Unit Effort; in this case = fish/100m or "fish per 100 meter of stream distance sampled"
#TC = Top Carnivore
#Eurythermal = wide thermal tolerance
#Sympatry = alongside Brown Trout 
#Allopatry = in the absence of Brown Trout


#load data
newdat <- read.csv("Data/Thesis/Tidy/chpt3_tidy.csv", header=T)
mydat <- read.csv("Data/Thesis/Tidy/SGCN_AllCovariates.csv", header=T)

#inspect
skim(mydat)

#wrangle
mydat <- mydat %>%
  mutate_at(vars(c("BRT","LND","SRD","Cottus")), as.factor)

#############################################################################

#----------------------------Boxplots of CPUE------------------------------#

#-----------------------Filter by presence of SGCN-------------------------#

#LND
ldace <- mydat %>%
  filter(LND == 1)

p1 <- ggplot(data = ldace, aes(x=BRT,y=LND_CPUE)) +
  stat_boxplot(geom = 'errorbar', width=0.25)+
  geom_boxplot(aes(fill=BRT), width = 0.50)+
  labs(x=NULL, y="Longnose Dace CPUE (fish/100m)")+
  theme_bw()+
  scale_x_discrete(labels=c("Absent", "Present"))+
  theme(legend.position = "NULL")+
  scale_fill_manual(values = c("white", "grey"))+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(size = 12))+
  ggtitle("(a)")+
  theme(plot.title = element_text(size=14, face = "bold"))

#SRD
sdace <- mydat %>%
  filter(SRD == 1)

p3 <- ggplot(data = sdace, aes(x=BRT,y=SRD_CPUE)) +
  stat_boxplot(geom = 'errorbar', width=0.25)+
  geom_boxplot(aes(fill=BRT), width=0.50)+
  labs(x=NULL, y="Southern Redbelly Dace CPUE (fish/100m)")+
  theme_bw()+
  scale_x_discrete(labels=c("Absent", "Present"))+
  theme(legend.position = "NULL")+
  scale_fill_manual(values = c("white", "grey"))+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(size = 12))+
  ggtitle("(c)")+
  theme(plot.title = element_text(size=14, face = "bold"))
  

#Sculpins
cott <- mydat %>%
  filter(Cottus == 1)

p2 <- ggplot(data = cott, aes(x=BRT,y=Cottus_CPUE)) +
  stat_boxplot(geom = 'errorbar', width=0.25)+
  geom_boxplot(aes(fill=BRT), width=0.50)+
  labs(x=NULL, y="Sculpin CPUE (fish/100m)")+
  theme_bw()+
  scale_x_discrete(labels=c("Absent", "Present"))+
  theme(legend.position = "NULL")+
  scale_fill_manual(values = c("white", "grey"))+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(size = 12))+
  ggtitle("(b)")+
  theme(plot.title = element_text(size=14, face = "bold"))
p2


#Overall figure for thesis:
boxplot.figure <- plot_grid(p1, p2, p3, labels = NULL, ncol=3)
boxplot.figure

#create common x axis label
library(gridExtra)
library(grid)
x.grob <- textGrob("Brown Trout Status (Presence/Absence)", 
                   gp=gpar(col="black", fontsize=14))
#add to plot
bf2 <- grid.arrange(arrangeGrob(boxplot.figure, bottom = x.grob))
bf2

ggsave("Figure_3.png", plot = bf2, dpi = 600)


