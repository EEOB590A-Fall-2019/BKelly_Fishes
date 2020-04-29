# Thesis Chapter Three:
## Size Comparison Analysis:
### Permutation Tests for each size bin for two groups (1. no Brown Trout, 2. Brown Trout present),
### and three species of greatest conservation need (1. Longnose Dace, 2. SouthernRedbelly Dace, 3.Sculpins)

#libraries
library(tidyverse)
#library(skimr)
library(coin)
library(tidyselect)

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
  ggtitle("(a)")+
  theme(plot.title = element_text(size=14, face = "bold"))+
  theme(legend.position = c(0.85,0.80))+
  theme(legend.title = element_blank())+
  annotate("text", x = 0.70, y = 299, label = "Longnose Dace")
a  


#lnd.ad <- sizes %>%
#  select(adult_status, lnd.list)
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

#library(cowplot)
combo <- plot_grid(a,b,c, ncol=1)

#srd.ad <- sizes %>%
#  select(adult_status, srd.list)
#-----------------------------------

#create common x axis label
library(gridExtra)
library(grid)
y.grob <- textGrob("Count", 
                   gp=gpar(col="black", fontsize=14),
                   rot = 90)
x.grob <- textGrob("Size Class (mm)", 
                   gp=gpar(col="black", fontsize=14))
#add to plot
fig_5 <- grid.arrange(arrangeGrob(combo, left = y.grob, bottom = x.grob))
fig_5
ggsave("Figure_5.png", plot = fig_5, dpi = 600)










