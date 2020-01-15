###########################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                        Visualizing Brook Trout Occupancy Results

# We have predictions of Psi and p as functions of (pctex21 & HAiFLS_for) and (Effort_sec)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###########################################################################################


library(tidyverse)
library(cowplot)

#load data
Psi.temp <- read_csv("Data/Thesis/Tidy/Psi_predictions_pctex21.csv", col_names = T)
Psi.forest <- read_csv("Data/Thesis/Tidy/Psi_predictions_HAiFLS_for.csv", col_names = T)
p.effort <- read_csv("Data/Thesis/Tidy/P_predictions_effort.csv", col_names = T)
Psi.for_Cat <- read_csv("Data/Thesis/Tidy/BKT_Catchment_Model_Predictions.csv", col_names = T)

# Add column that translates the value of the constant covariate to a factor with three levels

Psi.temp2 <- Psi.temp %>%
  mutate(For_Status = ifelse(HAiFLS_for==0,"HAiFLS Forest (0%)",ifelse(HAiFLS_for<27,"HAiFLS Forest (27%)", ifelse(HAiFLS_for>50,"HAiFLS Forest (55%)", "NA"))))
Psi.temp2$For_Status <- factor(Psi.temp2$For_Status, c("HAiFLS Forest (0%)","HAiFLS Forest (27%)","HAiFLS Forest (55%)"))

Psi.forest2 <- Psi.forest %>%
  mutate(P21_Status = ifelse(pctex21==0,"PctEx21 (0%)",ifelse(pctex21<6,"PctEx21 (5%)", ifelse(pctex21>10,"PctEx21 (12%)", "NA"))))
Psi.forest2$P21_Status <- factor(Psi.forest2$P21_Status, c("PctEx21 (0%)","PctEx21 (5%)","PctEx21 (12%)"))


#Make ggplot for predicted occupancy probabilies as function of temperature and forested catchment
Psi1 <- ggplot(data = Psi.temp2, aes(x=pctex21))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), colour="Blue", size=1)+
  facet_grid(cols = vars(For_Status))+
  scale_x_continuous(limits = c(0,5.1))+
  labs(x="Percent of Summer Stream Temperature > 21C",
       y="Occupancy Probability (Psi)")+
  theme_bw()+
  theme(axis.title = element_text(face = "bold"))+
  theme(panel.grid = element_blank())
Psi1

Psi2 <- ggplot(data = Psi.forest2, aes(x=HAiFLS_for))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), colour="Blue", size=1)+
  facet_grid(cols = vars(P21_Status))+
  labs(x="% HAiFLS Forest Land Cover in Catchment",
       y="Occupancy Probability (Psi)")+
  theme_bw()+
  theme(axis.title = element_text(face = "bold"))+
  theme(panel.grid = element_blank())
Psi2

#cowplot
plot_grid(Psi1,Psi2, labels = NULL, nrow = 2)
ggsave("bkt_OccuProb.png",
       dpi = 300)


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
