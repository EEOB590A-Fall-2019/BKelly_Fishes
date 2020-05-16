# Thesis Chapter Three:
## Size Comparison Analysis:
### Permutation Tests for each size bin for two groups (1. no Brown Trout, 2. Brown Trout present),
### and three species of greatest conservation need (1. Longnose Dace, 2. SouthernRedbelly Dace, 3.Sculpins)

#libraries
library(tidyverse)
#library(skimr)
library(coin)
library(tidyselect)

library(extrafont)
#font_import()
loadfonts(device="win")   #Register fonts for Windows bitmap output
fonts() 

#load data
sizes <- read.csv("Data/Thesis/Tidy/sgcn_size_tidy.csv", header = T)
skim(sizes)


#select data
nms <- names(sizes)

lnd.list <- vars_select(nms, ends_with("LND"))
srd.list <- vars_select(nms, ends_with("SRD"))
cott.list <- vars_select(nms, ends_with("Cottus"))

#-----------------------------------
#LND
lnd.brt <- sizes %>%
  select(BRT, lnd.list)


lnd.b.long <- lnd.brt %>%
  rename(bin1 = bin1_LND, bin2 = bin2_LND, bin3 = bin3_LND) %>%
  pivot_longer(
    cols = starts_with("bin"),
    names_to = "Bin",
    names_prefix = "bin",
    values_to = "count"
  ) %>%
  mutate_if(is.character, as.factor) %>%
  mutate_at("BRT", as.factor) %>%
  group_by(BRT, Bin) %>%
  summarise(Count = sum(count), Mean_Count = mean(count), SD = sd(count))

a <- ggplot(lnd.b.long, aes(fill=BRT, y=Count, x=Bin)) + 
  geom_bar(position="dodge", stat="identity", colour = "black") +
  theme_bw() +
  theme(panel.grid = element_blank())+
  labs(y=NULL, x=NULL)+
  scale_x_discrete(labels = c("1" = "30-60", "2" = "60-90", "3" = "90-120"))+
  scale_fill_manual(values = c("white", "black"),
                    labels = c("0" = "No Brown Trout", "1" = "Brown Trout"))+
  theme(axis.title = element_text(size = 12))+
  ggtitle("Longnose Dace")+
  theme(legend.position = c(0.85,0.80))+
  theme(legend.title = element_blank())+
  theme(plot.title = element_text(size=16, family = "Times New Roman"))
a  


#lnd.ad <- sizes %>%
#  select(adult_status, lnd.list)

lnd_sums <- sizes2 %>%
  select(newID, BRT, adult_status, lnd.list) %>%
  mutate(Sum = bin1_LND+bin2_LND+bin3_LND) %>%
  mutate(P_A = ifelse(Sum>0,1,0)) %>%
  mutate_at("P_A", as.factor) %>%
  filter(P_A == 1)%>%
  rename(bin1 = bin1_LND, bin2 = bin2_LND, bin3 = bin3_LND) %>%
  pivot_longer(
    cols = starts_with("bin"),
    names_to = "Bin",
    names_prefix = "bin",
    values_to = "count"
  ) %>%
  mutate_if(is.character, as.factor) %>%
  mutate_at("BRT", as.factor) %>%
  group_by(BRT, Bin) %>%
  summarise(Count = sum(count), Mean_Count = mean(count), SD = sd(count), Med = median(count)) %>%
  mutate(SE = (SD/sqrt(33)))

a2 <- ggplot(lnd_sums, aes(fill=BRT, y=Mean_Count, x=Bin)) + 
  geom_bar(position="dodge", stat="identity", colour = "black") +
  geom_errorbar(aes(ymin=Mean_Count, ymax=Mean_Count+SE), width=.2,
                position=position_dodge(.9))+
  theme_bw() +
  theme(panel.grid = element_blank())+
  labs(y=NULL, x=NULL)+
  scale_x_discrete(labels = c("1" = "30-60", "2" = "60-90", "3" = "90-120"))+
  scale_fill_manual(values = c("white", "black"))+
  theme(axis.title = element_text(size = 12))+
  ggtitle("Longnose Dace")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(plot.title = element_text(size=16, family = "Times New Roman"))+
  theme(legend.text = element_text(family = "Times New Roman", size = 12))
a2



#-----------------------------------


#-----------------------------------
#Cottus
cott.brt <- sizes %>%
  select(BRT, cott.list)

cott.b.long <- cott.brt %>%
  rename(bin1 = bin1_Cottus, bin2 = bin2_Cottus, bin3 = bin3_Cottus, bin4 = bin4_Cottus) %>%
  pivot_longer(
    cols = starts_with("bin"),
    names_to = "Bin",
    names_prefix = "bin",
    values_to = "count"
  ) %>%
  mutate_if(is.character, as.factor) %>%
  mutate_at("BRT", as.factor) %>%
  group_by(BRT, Bin) %>%
  summarise(Count = sum(count), Mean_Count = mean(count), SD = sd(count))

b <- ggplot(cott.b.long, aes(fill=BRT, y=Count, x=Bin)) + 
  geom_bar(position="dodge", stat="identity", colour = "black") +
  theme_bw() +
  theme(panel.grid = element_blank())+
  labs(y=NULL, x=NULL)+
  scale_x_discrete(labels = c("1" = "30-60", "2" = "60-90", "3" = "90-120", "4" = "120-150"))+
  scale_fill_manual(values = c("white", "black"))+
  theme(axis.title = element_text(size = 12))+
  ggtitle("(b)")+
  theme(plot.title = element_text(size=14, face = "bold"))+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  annotate("text", x = 0.625, y = 749, label = "Sculpins")+
  scale_y_continuous(limits = c(0,750),
                     breaks = c(0,100,200,300,400,500,600,700),
                     labels = c("0","100","200","300","400","500","600","700"))
b


#cott.ad <- sizes %>%
#  select(adult_status, cott.list)

cott_sums <- sizes2 %>%
  select(newID, BRT, adult_status, cott.list) %>%
  mutate(Sum = bin1_Cottus+bin2_Cottus+bin3_Cottus+bin4_Cottus) %>%
  mutate(P_A = ifelse(Sum>0,1,0)) %>%
  mutate_at("P_A", as.factor) %>%
  filter(P_A == 1)%>%
  rename(bin1 = bin1_Cottus, bin2 = bin2_Cottus, bin3 = bin3_Cottus, bin4 = bin4_Cottus) %>%
  pivot_longer(
    cols = starts_with("bin"),
    names_to = "Bin",
    names_prefix = "bin",
    values_to = "count"
  ) %>%
  mutate_if(is.character, as.factor) %>%
  mutate_at("BRT", as.factor) %>%
  group_by(BRT, Bin) %>%
  summarise(Count = sum(count), Mean_Count = mean(count), SD = sd(count), Med = median(count)) %>%
  mutate(SE = (SD/sqrt(33)))

b2 <- ggplot(cott_sums, aes(fill=BRT, y=Mean_Count, x=Bin)) + 
  geom_bar(position="dodge", stat="identity", colour = "black") +
  geom_errorbar(aes(ymin=Mean_Count, ymax=Mean_Count+SE), width=.2,
                position=position_dodge(.9))+
  theme_bw() +
  theme(panel.grid = element_blank())+
  labs(y=NULL, x=NULL)+
  scale_x_discrete(labels = c("1" = "30-60", "2" = "60-90", "3" = "90-120", "4" = "120-150"))+
  scale_fill_manual(values = c("white", "black"),
                    labels = c("0" = "No Brown Trout", "1" = "Brown Trout"))+
  theme(axis.title = element_text(size = 12))+
  ggtitle("Sculpin")+
  theme(legend.position = c(0.75,0.80))+
  theme(legend.title = element_blank())+
  theme(plot.title = element_text(size=16, family = "Times New Roman"))+
  theme(legend.text = element_text(family = "Times New Roman", size = 12))
b2

#-----------------------------------


#-----------------------------------
#SRD
srd.brt <- sizes %>%
  select(BRT, srd.list)

srd.b.long <- srd.brt %>%
  rename(bin1 = bin1_SRD, bin2 = bin2_SRD, bin3 = bin3_SRD) %>%
  pivot_longer(
    cols = starts_with("bin"),
    names_to = "Bin",
    names_prefix = "bin",
    values_to = "count"
  ) %>%
  mutate_if(is.character, as.factor) %>%
  mutate_at("BRT", as.factor) %>%
  group_by(BRT, Bin) %>%
  summarise(Count = sum(count), Mean_Count = mean(count), SD = sd(count))

c <- ggplot(srd.b.long, aes(fill=BRT, y=Count, x=Bin)) + 
  geom_bar(position="dodge", stat="identity", colour = "black") +
  theme_bw() +
  theme(panel.grid = element_blank())+
  labs(y=NULL, x=NULL)+
  scale_x_discrete(labels = c("1" = "30-60", "2" = "60-90", "3" = "90-120"))+
  scale_fill_manual(values = c("white", "black"))+
  theme(axis.title = element_text(size = 12))+
  ggtitle("(c)")+
  theme(plot.title = element_text(size=14, face = "bold"))+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  annotate("text", x = 0.85, y = 399, label = "Southern Redbelly Dace")+
  scale_y_continuous(limits = c(0,400),
                     breaks = c(0,100,200,300,400),
                     labels = c("0","100","200","300","400"))
c

library(cowplot)
combo <- plot_grid(a,b,c, ncol=1)

#srd.ad <- sizes %>%
#  select(adult_status, srd.list)
srd_sums <- sizes2 %>%
  select(newID, BRT, adult_status, srd.list) %>%
  mutate(Sum = bin1_SRD+bin2_SRD+bin3_SRD) %>%
  mutate(P_A = ifelse(Sum>0,1,0)) %>%
  mutate_at("P_A", as.factor) %>%
  filter(P_A == 1)%>%
  rename(bin1 = bin1_SRD, bin2 = bin2_SRD, bin3 = bin3_SRD) %>%
  pivot_longer(
    cols = starts_with("bin"),
    names_to = "Bin",
    names_prefix = "bin",
    values_to = "count"
  ) %>%
  mutate_if(is.character, as.factor) %>%
  mutate_at("BRT", as.factor) %>%
  group_by(BRT, Bin) %>%
  summarise(Count = sum(count), Mean_Count = mean(count), SD = sd(count), Med = median(count)) %>%
  mutate(SE = (SD/sqrt(33)))

c2 <- ggplot(srd_sums, aes(fill=BRT, y=Mean_Count, x=Bin)) + 
  geom_bar(position="dodge", stat="identity", colour = "black") +
  geom_errorbar(aes(ymin=Mean_Count, ymax=Mean_Count+SE), width=.2,
                position=position_dodge(.9))+
  theme_bw() +
  theme(panel.grid = element_blank())+
  labs(y=NULL, x=NULL)+
  scale_x_discrete(labels = c("1" = "30-60", "2" = "60-90", "3" = "90-120", "4" = "120-150"))+
  scale_fill_manual(values = c("white", "black"))+
  theme(axis.title = element_text(size = 12))+
  ggtitle("Southern Redbelly Dace")+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(plot.title = element_text(size=16, family = "Times New Roman"))+
  theme(legend.text = element_text(family = "Times New Roman", size = 12))
c2


combo2 <- plot_grid(b2,a2,c2, ncol=1)
combo2
#-----------------------------------

#create common x axis label
library(gridExtra)
library(grid)
y.grob <- textGrob("Number of Fish", 
                   gp=gpar(fontface="bold", col="black", fontsize=14, fontfamily="Times New Roman"),
                   rot = 90)
x.grob <- textGrob("Size Class (mm)", 
                   gp=gpar(fontface="bold", col="black", fontsize=14, fontfamily="Times New Roman"))
#add to plot
fig_5 <- grid.arrange(arrangeGrob(combo, left = y.grob, bottom = x.grob))
fig_5

fig_5.2 <- grid.arrange(arrangeGrob(combo2, left = y.grob, bottom = x.grob))
fig_5.2
ggsave("Figure_6_barcharts.png", plot = fig_5.2, dpi = 600)
#-----------------------------------


#---------------
# Comparisons
#---------------

str(sizes)

sizes2 <- sizes %>%
  mutate_at(c("BRT","adult_status"), as.factor)


#############################################################################

# Mann Whitney U / Wilcox Sign Rank Test 

# using subsetted data -- only when SGCNs of interest are present

help("wilcox.test")

# Ho: Median CPUE of SGCN when BRT are present = CPUE when BRT are absent
# two-sided

#-----
#LND
#-----
lnd.comp <- sizes2 %>%
  select(newID, BRT, adult_status, lnd.list) %>%
  mutate(Sum = bin1_LND+bin2_LND+bin3_LND) %>%
  mutate(P_A = ifelse(Sum>0,1,0)) %>%
  mutate_at("P_A", as.factor) %>%
  filter(P_A == 1)

## compare across BRT status
#size class 1
wilcox.test(lnd.comp$bin1_LND ~ lnd.comp$BRT, mu=0, alt="two.sided", conf.int=T, conf.level=0.95, paired=F,
            exact=F)
# p-value = 0.3945

#size class 2
wilcox.test(lnd.comp$bin2_LND ~ lnd.comp$BRT, mu=0, alt="two.sided", conf.int=T, conf.level=0.95, paired=F,
            exact=F)
# p-value = 0.6726

#size class 3
wilcox.test(lnd.comp$bin3_LND ~ lnd.comp$BRT, mu=0, alt="two.sided", conf.int=T, conf.level=0.95, paired=F,
            exact=F)
# p-value = 0.6321

#-----
## compare across adult proportion status
#size class 1
wilcox.test(lnd.comp$bin1_LND ~ lnd.comp$adult_status, mu=0, alt="two.sided", conf.int=T, conf.level=0.95, paired=F,
            exact=F)
# p-value = 0.3081

#size class 2
wilcox.test(lnd.comp$bin2_LND ~ lnd.comp$adult_status, mu=0, alt="two.sided", conf.int=T, conf.level=0.95, paired=F,
            exact=F)
# p-value = 0.7436

#size class 3
wilcox.test(lnd.comp$bin3_LND ~ lnd.comp$adult_status, mu=0, alt="two.sided", conf.int=T, conf.level=0.95, paired=F,
            exact=F)
# p-value = 0.7078




#-----
#SRD
#-----
srd.comp <- sizes2 %>%
  select(newID, BRT, adult_status, srd.list) %>%
  mutate(Sum = bin1_SRD+bin2_SRD+bin3_SRD) %>%
  mutate(P_A = ifelse(Sum>0,1,0)) %>%
  mutate_at("P_A", as.factor) %>%
  filter(P_A == 1)

## compare across BRT status
#size class 1
wilcox.test(srd.comp$bin1_SRD ~ srd.comp$BRT, mu=0, alt="two.sided", conf.int=T, conf.level=0.95, paired=F,
            exact=F)
# p-value = 0.1305

#size class 2
wilcox.test(srd.comp$bin2_SRD ~ srd.comp$BRT, mu=0, alt="two.sided", conf.int=T, conf.level=0.95, paired=F,
            exact=F)
# p-value = 0.6886

#size class 3
wilcox.test(srd.comp$bin3_SRD ~ srd.comp$BRT, mu=0, alt="two.sided", conf.int=T, conf.level=0.95, paired=F,
            exact=F)
# p-value = 1

#-----
## compare across adult proportion status
#size class 1
wilcox.test(srd.comp$bin1_SRD ~ srd.comp$adult_status, mu=0, alt="two.sided", conf.int=T, conf.level=0.95, paired=F,
            exact=F)
# p-value = 0.6122

#size class 2
wilcox.test(srd.comp$bin2_SRD ~ srd.comp$adult_status, mu=0, alt="two.sided", conf.int=T, conf.level=0.95, paired=F,
            exact=F)
# p-value = 0.3798

#size class 3
wilcox.test(srd.comp$bin3_SRD ~ srd.comp$adult_status, mu=0, alt="two.sided", conf.int=T, conf.level=0.95, paired=F,
            exact=F)
# p-value = 0.6737



#-----
#Cottus
#-----
cott.comp <- sizes2 %>%
  select(newID, BRT, adult_status, cott.list) %>%
  mutate(Sum = bin1_Cottus+bin2_Cottus+bin3_Cottus+bin4_Cottus) %>%
  mutate(P_A = ifelse(Sum>0,1,0)) %>%
  mutate_at("P_A", as.factor) %>%
  filter(P_A == 1)

## compare across BRT status
#size class 1
wilcox.test(cott.comp$bin1_Cottus ~ cott.comp$BRT, mu=0, alt="two.sided", conf.int=T, conf.level=0.95, paired=F,
            exact=F)
# p-value = 0.7411

#size class 2
wilcox.test(cott.comp$bin2_Cottus ~ cott.comp$BRT, mu=0, alt="two.sided", conf.int=T, conf.level=0.95, paired=F,
            exact=F)
# p-value = 0.6579

#size class 3
wilcox.test(cott.comp$bin3_Cottus ~ cott.comp$BRT, mu=0, alt="two.sided", conf.int=T, conf.level=0.95, paired=F,
            exact=F)
# p-value = 0.7484

#size class 4
wilcox.test(cott.comp$bin4_Cottus ~ cott.comp$BRT, mu=0, alt="two.sided", conf.int=T, conf.level=0.95, paired=F,
            exact=F)
# p-value = 0.611

#-----
## compare across adult proportion status
#size class 1
wilcox.test(cott.comp$bin1_Cottus ~ cott.comp$adult_status, mu=0, alt="two.sided", conf.int=T, conf.level=0.95, paired=F,
            exact=F)
# p-value = 0.5448

#size class 2
wilcox.test(cott.comp$bin2_Cottus ~ cott.comp$adult_status, mu=0, alt="two.sided", conf.int=T, conf.level=0.95, paired=F,
            exact=F)
# p-value = 0.967

#size class 3
wilcox.test(cott.comp$bin3_Cottus ~ cott.comp$adult_status, mu=0, alt="two.sided", conf.int=T, conf.level=0.95, paired=F,
            exact=F)
# p-value = 0.801

#size class 4
wilcox.test(cott.comp$bin4_Cottus ~ cott.comp$adult_status, mu=0, alt="two.sided", conf.int=T, conf.level=0.95, paired=F,
            exact=F)
# p-value = 0.9469

#############################################################################

# Using the "coin" package
# Exact Wilcoxon Mann Whitney Rank Sum Test
# where y is numeric and A is a binary factor

#LND
wilcox_test(bin1_LND~BRT, data=lnd.comp, distribution="exact") #p-value = 0.4046
wilcox_test(bin2_LND~BRT, data=lnd.comp, distribution="exact") #p-value = 0.6701
wilcox_test(bin3_LND~BRT, data=lnd.comp, distribution="exact") #p-value = 0.6269

#SRD
wilcox_test(bin1_SRD~BRT, data=srd.comp, distribution="exact") #p-value = 0.1284
wilcox_test(bin2_SRD~BRT, data=srd.comp, distribution="exact") #p-value = 0.6859
wilcox_test(bin3_SRD~BRT, data=srd.comp, distribution="exact") #p-value = 1

#Cottus
wilcox_test(bin1_Cottus~BRT, data=cott.comp, distribution="exact") #p-value = 0.7368
wilcox_test(bin2_Cottus~BRT, data=cott.comp, distribution="exact") #p-value = 0.6526
wilcox_test(bin3_Cottus~BRT, data=cott.comp, distribution="exact") #p-value = 0.7368
wilcox_test(bin4_Cottus~BRT, data=cott.comp, distribution="exact") #p-value = 1

# One-Way Permutation Test based on 9999 Monte-Carlo
# resamplings. y is numeric and A is a categorical factor

#LND
oneway_test(bin1_LND~BRT, data=lnd.comp,
            distribution=approximate(B=9999)) #p = 0.1881
oneway_test(bin2_LND~BRT, data=lnd.comp,
            distribution=approximate(B=9999)) #p = 0.9501
oneway_test(bin3_LND~BRT, data=lnd.comp,
            distribution=approximate(B=9999)) #p = 0.5884

#SRD
oneway_test(bin1_SRD~BRT, data=srd.comp,
            distribution=approximate(B=9999)) #p = 0.1119
oneway_test(bin2_SRD~BRT, data=srd.comp,
            distribution=approximate(B=9999)) #p = 0.3327
oneway_test(bin3_SRD~BRT, data=srd.comp,
            distribution=approximate(B=9999)) #p = 0.7605
#Cottus
oneway_test(bin1_Cottus~BRT, data=cott.comp,
            distribution=approximate(B=9999)) #p = 0.6017
oneway_test(bin2_Cottus~BRT, data=cott.comp,
            distribution=approximate(B=9999)) #p = 0.7618
oneway_test(bin3_Cottus~BRT, data=cott.comp,
            distribution=approximate(B=9999)) #p = 0.8168
oneway_test(bin4_Cottus~BRT, data=cott.comp,
            distribution=approximate(B=9999)) #p = 1
#############################################################################

# Results:
# Longnose Dace:
# occurr = 33
# sympatry = 19, allopatry = 14
# no significant difference in size when BRT present vs. absent
# Southern Redbelly Dace:
# occurr = 33
# sympatry = 12, allopatry = 21
# no significant difference in size when BRT present vs. absent
# Sculpins:
# occurr = 20
# sympatry = 18, allopatry = 2
# no significant difference in size when BRT present vs. absent

#############################################################################









