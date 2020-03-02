###########################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                        Visualizing Brook Trout Occupancy Results

# We have predictions of Psi and p as functions of (pctex21 & HAiFLS_for) and (Effort_sec)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###########################################################################################


library(tidyverse)
library(cowplot)

#load data
Psi.temp <- read_csv("Data/Thesis/Tidy/Psi_predictions_avgT.csv", col_names = T)
Psi.bare <- read_csv("Data/Thesis/Tidy/Psi_predictions_BrBnk.csv", col_names = T)
p.effort <- read_csv("Data/Thesis/Tidy/P_predictions_effort.csv", col_names = T)
Psi.for_Cat <- read_csv("Data/Thesis/Tidy/BKT_Catchment_Model_Predictions.csv", col_names = T)

# Add column that translates the value of the constant covariate to a factor with three levels

Psi.temp2 <- Psi.temp %>%
  mutate(Bare_Status = ifelse(pctBrBnk<0.5,"Bare Bank Index (0.43)",ifelse(pctBrBnk<1,"Bare Bank Index (0.87)", ifelse(pctBrBnk>1,"Bare Bank Index (1.20)", "NA"))))
Psi.temp2$Bare_Status <- factor(Psi.temp2$Bare_Status, c("Bare Bank Index (0.43)","Bare Bank Index (0.87)","Bare Bank Index (1.20)"))

Psi.bare2 <- Psi.bare %>%
  mutate(avgT_Status = ifelse(avgT<15,"avgT (14.39°C)",ifelse(avgT<16,"avgT (15.58°C)", ifelse(avgT<17,"avgT (16.79°C)", "NA"))))
Psi.bare2$avgT_Status <- factor(Psi.bare2$avgT_Status, c("avgT (14.39°C)","avgT (15.58°C)","avgT (16.79°C)"))


#Make ggplot for predicted occupancy probabilies as function of temperature and forested catchment
Psi1 <- ggplot(data = Psi.temp2, aes(x=avgT))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), colour="Blue", size=1)+
  facet_grid(cols = vars(Bare_Status))+
  #scale_x_continuous(limits = c(0,10.5))+
  labs(x="Average Summer Stream Temperature (°C)",
       y="Occupancy Probability (Psi)")+
  theme_bw()+
  theme(axis.title = element_text(face = "bold"))+
  theme(panel.grid = element_blank())+
  theme(strip.text.x = element_text(size=10,face = "bold"))
Psi1

Psi2 <- ggplot(data = Psi.bare2, aes(x=pctBrBnk))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), colour="Blue", size=1)+
  facet_grid(cols = vars(avgT_Status))+
  labs(x="Bare Bank Index",
       y="Occupancy Probability (Psi)")+
  theme_bw()+
  theme(axis.title = element_text(face = "bold"))+
  theme(panel.grid = element_blank())+
  theme(strip.text.x = element_text(size=10,face = "bold"))
Psi2

#cowplot
plot_grid(Psi1,Psi2, labels = NULL, nrow = 2)
ggsave("bkt_OccuProb.png",
       dpi = 350)


############################
#Catchment model predictions
############################

cfor <- Psi.for_Cat %>%
  select(covdata, estimate, se, lcl, ucl) %>%
  rename(HAiFLS_for = covdata)

cp <- ggplot(data = cfor, aes(x=HAiFLS_for))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate),colour="blue", size=1)+
  labs(x="% HAiFLS Forest Land Cover in Catchment",
       y="Occupancy Probability (Psi)")+
  theme_classic()+
  theme(axis.title = element_text(face = "bold"))
cp
ggsave("bkt_Occu_Cat.png",
       dpi = 300)




###################################
#Detection Probability predictions
##################################

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#### Visualizing effort effect on p   ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

ggplot(data=p.effort, aes(x=Effort_sec))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  labs(x="Electrofishing Effort (s)",
       y="Detection Probability (p)")+
  scale_y_continuous(limits = c(0,1), breaks = c(0.00,0.25,0.50,0.75,1.00))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold"))

ggsave("bkt_DetProb.png",
       dpi = 350,
       width = 5,
       height = 4.75, 
       units = "in")

