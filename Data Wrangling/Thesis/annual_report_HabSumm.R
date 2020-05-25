##Habitat Summaries:
#-------------------

library(tidyverse)
library(skimr)
library(cowplot)

#data
hab <- read.csv("Data/Thesis/Tidy/AllCovariates.csv", header = T)
evrt <- read.csv("Data/Thesis/Tidy/enviro_tidy.csv", header = T)

skim(hab)

ttab <- hab %>%
  select(newID=HUC_Site, HUC8, Year, avgT) %>%
  group_by(Year, HUC8)%>%
  summarise(mT=mean(avgT,na.rm = T), minT=min(avgT,na.rm = T), maxT=max(avgT,na.rm = T), sdT=sd(avgT,na.rm = T))

temp <- hab %>%
  select(newID=HUC_Site, HUC8, Site, Year, RNGT, MEANT, MAXT) %>%
  drop_na()
temp$Year <- as.factor(temp$Year)

#boxplots - MEANT
top <- ggplot(data = temp, aes(x=HUC8, y=MEANT, fill=Year))+
  geom_boxplot(
    width=0.5
  )+
  theme_bw()+
  labs(x=NULL, y="Max Daily Mean Temperature (°C)")+
  theme(axis.title = element_text(face = "bold", size = 12))+
  theme(panel.grid = element_blank())+
  theme(legend.position = c(0.2,0.1))+
  theme(legend.title = element_text(face = "bold"))+
  theme(legend.background = element_rect(color = "black"))+
  scale_y_continuous(limits = c(9,25),
                     breaks = c(10,15,20,25),
                     labels = c("10","15","20","25"))+
  scale_x_discrete(labels=c("Upper Iowa", "Yellow", "Little Maquoketa"))+
  theme(axis.text = element_text(size = 10))+
  scale_fill_manual(values = c("#999999", "#E69F00"))
top

#boxplots - MAXT
mid <- ggplot(data = temp, aes(x=HUC8, y=MAXT, fill=Year))+
  geom_boxplot(
    width=0.5
  )+
  theme_bw()+
  labs(x=NULL, y="Max Summer Temperature (°C)")+
  theme(axis.title = element_text(face = "bold", size = 12))+
  theme(panel.grid = element_blank())+
  theme(legend.position = "none")+
  theme(legend.title = element_text(face = "bold"))+
  #scale_y_continuous(limits = c(9,25),
   #                  breaks = c(10,15,20,25),
    #                 labels = c("10","15","20","25"))+
  scale_x_discrete(labels=c("Upper Iowa", "Yellow", "Little Maquoketa"))+
  theme(axis.text = element_text(size = 10))+
  scale_fill_manual(values = c("#999999", "#E69F00"))
mid

#boxplots - RNGT
bot <- ggplot(data = temp, aes(x=HUC8, y=RNGT, fill=Year))+
  geom_boxplot(
    width=0.5
  )+
  theme_bw()+
  labs(x=NULL, y="Max Daily Temperature Range (°C)")+
  theme(axis.title = element_text(face = "bold", size = 12))+
  theme(panel.grid = element_blank())+
  theme(legend.position = "none")+
  theme(legend.title = element_text(face = "bold"))+
  #scale_y_continuous(limits = c(9,25),
   #                  breaks = c(10,15,20,25),
    #                 labels = c("10","15","20","25"))+
  scale_x_discrete(labels=c("Upper Iowa", "Yellow", "Little Maquoketa"))+
  theme(axis.text = element_text(size = 10))+
  scale_fill_manual(values = c("#999999", "#E69F00"))
bot


t.p <- plot_grid(top,mid,bot,
          ncol=3, labels=c("a","b","c"))
ggsave("Temp_boxplots.png", plot = t.p, dpi = 600)

#create common x axis label
#library(gridExtra)
#library(grid)
#x.grob <- textGrob("Watershed (HUC8)", 
                   #gp=gpar(col="black", fontsize=12, fontface="bold"))
#add to plot
#t.p2 <- grid.arrange(arrangeGrob(t.p, bottom = x.grob))
#t.p2

#ggsave("Figure_4_boxplots.png", plot = bf2, dpi = 600)



#-------------------
##Fish Summaries:
#-------------------
fish <- read_csv("Data/Thesis/Tidy/tidyfish1.csv", col_names = T)

#remove fishless sites
fish[13,] #UPI_29
fish[54,] #YEL_33
fish[46,3] <- "201"
fish[47,3] <- "202"

fish2 <- fish %>%
  filter(uid != 13 & uid != 54)
fish2[115,3] <- "97b"
##exploration of SGCN
#Species of Greatest Conservation Need: BKT, SRD, LND, Cottus, ABL
names(fish2)
sgcn <- fish2 %>%
  select(uid, HUC8, site, BKT, SRD, LND, Cottus, ABL, MSM, SMM, CMM)%>%
  mutate(SGCN_pres = ifelse(BKT+SRD+LND+Cottus+ABL+MSM+SMM+CMM>0,1,0), SGCN_rich = (BKT + SRD + LND + Cottus + ABL+MSM+SMM+CMM))%>%
  select(HUC8, site, SGCN_pres, SGCN_rich) %>%
  unite("newID", c(HUC8,site), sep = "_", remove = F)

yr <- hab %>%
  select(newID=HUC_Site, Year, Easting, Northing)
yr[99,3]=574087
yr[99,4]=4805365
yr$Year <- as.factor(yr$Year)
names(yr)

part1 <- left_join(sgcn,yr, by="newID")
#---------------------------------------
sgcn_summary <- sgcn %>%
  group_by(HUC8)%>%
  summarise(SGCN_sites = sum(SGCN_pres))


#make a "Trout Status" variable for the fill argument
trout2 <- fish2 %>%
  mutate(Trout_Status = ifelse(BKT+BRT>1,3,ifelse(BKT-BRT>0,2,ifelse(BKT-BRT<0,1,0)))) %>%
  select(HUC8, site, BKT, BRT, Trout_Status) %>%
  unite("newID", c(HUC8,site), sep = "_", remove = T)

trout2$Trout_Status <- as.factor(trout2$Trout_Status)

skim(trout2)

both_parts <- left_join(part1, trout2)

write.csv(both_parts, "Data/Thesis/Tidy/data_for_USFWS_report.csv", row.names = F)



#------------------
trout <- fish2 %>%
  select(uid, HUC8, site, BKT, BKT_ab, BRT, BRT_ab, RBT, RBT_ab, TGT, TGT_ab)%>%
  mutate(BKT_BRT = ifelse(BKT+BRT>1,1,0), BKT_RBT = ifelse(BKT+RBT>1,1,0), BRT_RBT = ifelse(BRT+RBT>1,1,0),
         BK_BR_RB = ifelse(BKT+BRT+RBT>2,1,0))

tws <- trout %>%
  group_by(HUC8) %>%
  summarise(BK_total = sum(BKT_ab), BR_total = sum(BRT_ab), RB_total = sum(RBT_ab), 
            BkBr_Coex = sum(BKT_BRT), BkRb_Coex = sum(BKT_RBT), BrRb_Coex = sum(BRT_RBT),
            All_coex = sum(BK_BR_RB), PCT_BKT = (sum(BKT_ab)/(sum(BRT_ab+RBT_ab))*100))
ts <- trout %>%
  summarise(BK_total = sum(BKT_ab), BR_total = sum(BRT_ab), RB_total = sum(RBT_ab), 
            BkBr_Coex = sum(BKT_BRT), BkRb_Coex = sum(BKT_RBT), BrRb_Coex = sum(BRT_RBT),
            All_coex = sum(BK_BR_RB), PCT_BKT = (sum(BKT_ab)/(sum(BRT_ab+RBT_ab))*100))



####################################################################

## Make csv for mapping spatial distribution of Trout (BKT and BRT)

###################################################################


