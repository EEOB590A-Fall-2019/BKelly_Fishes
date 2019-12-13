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

# Add column that translates the value of the constant covariate to a factor with three levels

Psi.temp2 <- Psi.temp %>%
  mutate(For_Status = ifelse(HAiFLS_for==0,"HAiFLS -1sd",ifelse(HAiFLS_for<27,"Mean HAiFLS", ifelse(HAiFLS_for>50,"HAiFLS +1sd", "NA"))))
Psi.temp2$For_Status <- factor(Psi.temp2$For_Status, c("HAiFLS -1sd","Mean HAiFLS","HAiFLS +1sd"))

Psi.forest2 <- Psi.forest %>%
  mutate(P21_Status = ifelse(pctex21==0,"PctEx21 -1sd",ifelse(pctex21<6,"Mean PctEx21", ifelse(pctex21>10,"PctEx21 +1sd", "NA"))))
Psi.forest2$P21_Status <- factor(Psi.forest2$P21_Status, c("PctEx21 -1sd","Mean PctEx21","PctEx21 +1sd"))


#Make ggplot for predicted occupancy probabilies as function of temperature and forested catchment
Psi1 <- ggplot(data = Psi.temp2, aes(x=pctex21))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), colour="Blue", size=1.5)+
  facet_grid(cols = vars(For_Status))+
  labs(x="Percent of Summer Stream Temperature > 21C",
       y="Occupancy Probability (Psi)")+
  theme_minimal_grid()

Psi2 <- ggplot(data = Psi.forest2, aes(x=HAiFLS_for))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), colour="Blue", size=1.5)+
  facet_grid(cols = vars(P21_Status))+
  labs(x="% Forest Landcover in Catchment",
       y="Occupancy Probability (Psi)")+
  theme_minimal_grid()
Psi2

#cowplot
plot_grid(Psi1,Psi2, labels = NULL, label_size = 12, nrow = 2)





