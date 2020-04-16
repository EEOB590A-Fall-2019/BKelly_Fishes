## occupancy graphics 

library(tidyverse)
library(cowplot)

#Brown Trout Detection Probability
effort.preds <- read.csv("Data/Thesis/BRT_p_effort_preds.csv", header = T)

brt.p <- ggplot(data=effort.preds, aes(x=Effort_sec))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  labs(x="Electrofishing Effort (sec)",
       y="Detection Probability (p)")+
  scale_y_continuous(limits = c(0,1), breaks = c(0.00,0.25,0.50,0.75,1.00), labels = c("0.00","0.25","0.50","0.75","1.00"))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 12))+
  ggtitle("Brown Trout")
brt.p

#Brown Trout Cumulative Detection Probability
brt_cdp <- data.frame(reach = 1:3, p = c(0.81,0.96,0.99), lcl = c(0.75,0.94,0.98),
                      ucl = c(0.86,0.98,1.00))

brt.cdp <- 
ggplot(data = brt_cdp, aes(x=reach))+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), colour="black", width=.1) +
  geom_line(aes(y=p), size=1, color="black")+
  geom_point(aes(y=p))+
  labs(x="Number of Sampling Occassions",
       y="Cumulative Detection Probability")+
  scale_y_continuous(limits = c(0.4,1), breaks = c(0.40,0.60,0.80,1.00), labels = c("0.40","0.60","0.80","1.00"))+
  scale_x_continuous(breaks = c(1,2,3), labels = c("1","2","3"))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 12))
brt.cdp

#Brown Trout combination figure
brt.cow <- plot_grid(brt.p, brt.cdp, ncol = 1)
brt.cow

#-----

#Brook Trout Detection Probability
effort.bkt <- read.csv("Data/Thesis/Tidy/P_predictions_effort.csv", header = T)
names(effort.bkt)

bkt.p <- ggplot(data=effort.bkt, aes(x=Effort_sec))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  labs(x="Electrofishing Effort (sec)",
       y="")+
  scale_y_continuous(limits = c(0,1), breaks = c(0.00,0.25,0.50,0.75,1.00), labels = c("0.00","0.25","0.50","0.75","1.00"))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 12))+
  ggtitle("Brook Trout")
bkt.p

#Brook Trout Cumulative Detection Probability
bkt_cdp <- data.frame(reach = 1:3, p = c(0.74,0.93,0.98), lcl = c(0.58,0.82,0.93),
                      ucl = c(0.86,0.98,1.00))

bkt.cdp <- 
  ggplot(data = bkt_cdp, aes(x=reach))+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), colour="black", width=.1) +
  geom_line(aes(y=p), size=1, color="black")+
  geom_point(aes(y=p))+
  labs(x="Number of Sampling Occassions",
       y="")+
  scale_y_continuous(limits = c(0.4,1), breaks = c(0.40,0.60,0.80,1.00), labels = c("0.40","0.60","0.80","1.00"))+
  scale_x_continuous(breaks = c(1,2,3), labels = c("1","2","3"))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 12))
bkt.cdp

#Brook Trout combination figure
bkt.cow <- plot_grid(bkt.p, bkt.cdp, ncol = 1)
bkt.cow


#combination figure
dprob <- plot_grid(brt.cow, bkt.cow, ncol = 2)
dprob


# save final figure
ggsave("DetProb_4_16_2020.png",
       dpi = 600)





#-----
#Occupancy figures at the catchment-scale
#-----

#brown trout
###########################################################################
#predict across range of observed values (HAiFLS_for, Area_km2, Cross_Cat)
##########################################################################

#predictions of Psi for full range of HAiFLS_for and mean values of other covars
cat.for.preds <- read.csv("Data/Thesis/Tidy/BRT_cat_for_preds.csv", header = T)

ap <- 
  ggplot(data=cat.for.preds, aes(x=HAiFLS_for))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  labs(x= "% HAiFLS Forest Land Cover",
       y="Occupancy Probability (Ψ)")+
  theme_bw()+
  theme(panel.grid = element_blank())+
  scale_y_continuous(limits = c(0.00,1.00),
                     breaks = c(0.00, 0.25, 0.50, 0.75, 1.00),
                     labels = c("0.00", "0.25", "0.50", "0.75", "1.00"))+
  theme(axis.title = element_text(size = 12, face = "bold"))+
  theme(axis.title.x = element_text(margin = margin(b = 6)))+
  ggtitle("Brown Trout")
ap



#predictions of Psi for full range of Area_km2 and mean values of other covars
cat.area.preds <- read.csv("Data/Thesis/Tidy/BRT_cat_area_preds.csv", header = T)

bp <- 
  ggplot(data=cat.area.preds, aes(x=Area_km2))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  labs(x= bquote(bold('Upstream Catchment Area' ~(km^2))),
       y="Occupancy Probability (Ψ)")+
  theme_bw()+
  theme(panel.grid = element_blank())+
  scale_y_continuous(limits = c(0.00,1.00),
                     breaks = c(0.00, 0.25, 0.50, 0.75, 1.00),
                     labels = c("0.00", "0.25", "0.50", "0.75", "1.00"))+
  theme(axis.title = element_text(size = 12, face = "bold"))+
  theme(axis.title.x = element_text(margin = margin(b = 0.1)))
bp

brt_grid <- plot_grid(ap, bp, ncol = 1)
#-----

#brook trout
Psi.for_Cat <- read_csv("Data/Thesis/Tidy/BKT_Catchment_Model_Predictions.csv", col_names = T)

cp <- ggplot(data=Psi.for_Cat, aes(x=covdata))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  labs(x="% HAiFLS Forest Land Cover",
       y="")+
  theme_bw()+
  theme(panel.grid = element_blank())+
  scale_y_continuous(limits = c(0,1),
                     breaks = c(0.00, 0.25, 0.50, 0.75, 1.00),
                     labels = c("0.00", "0.25", "0.50", "0.75", "1.00"))+
  theme(axis.title = element_text(size = 12, face = "bold"))+
  ggtitle("Brook Trout")


# save final figure
plot_grid(ap, cp, bp, ncol = 2)
ggsave("trout_psi_catchment_scale.png", dpi = 600)
###############################################################################
pred.temps <- read.csv("Data/Thesis/Tidy/Psi_predictions_avgT.csv", header=T)
pred.bare <- read.csv("Data/Thesis/Tidy/Psi_predictions_BrBnk.csv", header=T)
pred.pool <- read.csv("Data/Thesis/Tidy/Psi_predictions_pctpool.csv", header = T)


#Make ggplot for predicted occupancy probabilies 
Psi1 <- ggplot(data = pred.temps, aes(x=estimates.avgT))+
  geom_ribbon(aes(ymin=estimates.lcl, ymax=estimates.ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimates.estimate), colour="black", size=1)+
  scale_y_continuous(limits = c(0,1), breaks = c(0.00,0.25,0.50,0.75,1.00))+
  labs(x="Average Summer Stream Temperature (°C)",
       y="Occupancy Probability (Ψ)")+
  theme_bw()+
  theme(axis.title = element_text(face = "bold", size = 12))+
  theme(panel.grid = element_blank())+
  theme(strip.text.x = element_text(size=10,face = "bold"))
Psi1


###############################################################################

Psi2 <- ggplot(data = pred.bare, aes(x=estimates.BrBnk))+
  geom_ribbon(aes(ymin=estimates.lcl, ymax=estimates.ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimates.estimate), colour="black", size=1)+
  scale_y_continuous(limits = c(0,1), breaks = c(0.00, 0.25, 0.50, 0.75, 1.00),
                     labels = c("0.00","0.25","0.50","0.75","1.00"))+
  labs(x="Bare Bank Index",
       y=NULL)+
  theme_bw()+
  theme(axis.title = element_text(face = "bold", size = 12))+
  theme(panel.grid = element_blank())+
  theme(strip.text.x = element_text(size=10,face = "bold"))
Psi2

###############################################################################

Psi3 <- ggplot(data = pred.pool, aes(x=estimates.pctpool))+
  geom_ribbon(aes(ymin=estimates.lcl, ymax=estimates.ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimates.estimate), colour="black", size=1)+
  scale_y_continuous(limits = c(0,1), breaks = c(0.00,0.25,0.50,0.75,1.00))+
  labs(x="Percent Pool Macrohabitat",
       y=NULL)+
  theme_bw()+
  theme(axis.title = element_text(face = "bold", size = 12))+
  theme(panel.grid = element_blank())+
  theme(strip.text.x = element_text(size=10,face = "bold"))
Psi3

#cowplot
plot_grid(Psi1,Psi3,Psi2, align = "h", labels = c(NA,NA,NA), nrow = 1)

ggsave("bkt_OccuProb_AvgT_Bnk_Pool.png",
       dpi = 600)


############################


####################################################
##     Psi predictions for Brown Trout CatMod     ## 
####################################################

#-----
#mFlow
#-----
flow.preds <- read.csv("Data/Thesis/Tidy/BRT_psi_flow_preds.csv", header = T)
a <- ggplot(data=flow.preds, aes(x=mFlow))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  labs(x="Mean Flow Velocity (m/sec)",
       y=NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold"))
a

#-----
#pctrun
#-----
run.preds <- read.csv("Data/Thesis/Tidy/BRT_psi_run_preds.csv", header = T)
b <- ggplot(data=run.preds, aes(x=pctrun))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  labs(x="Proportion of Run Macrohabitat",
       y=NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold"))
b

#-----
#pctBrBnk
#-----
bare.preds <- read.csv("Data/Thesis/Tidy/BRT_psi_bare_preds.csv", header = T)
e <- ggplot(data=bare.preds, aes(x=pctBrBnk))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  labs(x="Bare Bank Index",
       y=NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold"))+
  scale_y_continuous(limits = c(0,1),
                     breaks = c(0,0.25,0.50,0.75,1),
                     labels = c("0.00","0.25","0.50","0.75","1.00"))
e
#-----
#HAiFLS_for
#-----
for.preds <- read.csv("Data/Thesis/Tidy/BRT_psi_for_preds.csv", header = T)
c <- ggplot(data=for.preds, aes(x=HAiFLS_for))+
  geom_ribbon(aes(ymin=for.preds$lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.25,0.50,0.75,1.00), labels = c("0.00","0.25","0.50","0.75","1.00"))+
  labs(x="% HAiFLS Forest Land Cover",
       y=NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold"))
c


#-----
#Area_km2
#-----
area.preds <- read.csv("Data/Thesis/Tidy/BRT_psi_area_preds.csv", header=T)
d <- ggplot(data=area.preds, aes(x=Area_km2))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.25,0.50,0.75,1.00), labels = c("0.00","0.25","0.50","0.75","1.00"))+
  labs(x="Upstream Catchment Area (km^2)",
       y=NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold"))
d

#cowplot
vert <- plot_grid(a,b,e,c,d, labels = NULL, ncol = 1)

#create common y axis label
library(gridExtra)
library(grid)
y.grob <- textGrob("Occupancy Probability (Ψ)", 
                   gp=gpar(fontface="bold", col="black", fontsize=14), rot=90)
#add to plot
f <- grid.arrange(arrangeGrob(vert, left = y.grob))


ggsave("brt_occupancy_all_2020.png", plot=f, dpi = 600)





































