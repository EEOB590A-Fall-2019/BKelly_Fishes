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
ef <- read.csv("Data/Thesis/Tidy/AllCovariates.csv", header=T) %>%
  select(HUC_Site, effsec) %>%
  rename(newID = HUC_Site)

names(newdat)
names(mydat)

cobble <- mydat %>%
  select(newID, HUC8, HUC_10, pctcbbl, SegLen, LND_ab, SRD_ab, Cottus_ab)

newdata <- left_join(newdat, cobble, by="newID")
newdata <- left_join(newdata, ef, by="newID")

summary(newdata)

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

#ggsave("Figure_3.png", plot = bf2, dpi = 600)






#############################################################################

# Mann Whitney U / Wilcox Sign Rank Test 

# using subsetted data -- only when SGCNs of interest are present

help("wilcox.test")

# Ho: Median CPUE of SGCN when BRT are present = CPUE when BRT are absent
# two-sided

#-----
#LND
#-----
class(ldace$BRT)
wilcox.test(ldace$LND_CPUE ~ ldace$BRT, mu=0, alt="two.sided", conf.int=T, conf.level=0.95, paired=F,
            exact=F)
# no difference

#-----
#SRD
#-----
class(sdace$BRT)
wilcox.test(sdace$SRD_CPUE ~ sdace$BRT, mu=0, alt="two.sided", conf.int=T, conf.level=0.95, paired=F,
            exact=F)
#no difference
#-----
#Cottus
#-----
class(cott$BRT)
wilcox.test(cott$Cottus_CPUE ~ cott$BRT, mu=0, alt="two.sided", conf.int=T, conf.level=0.95, paired=F,
            exact=F)
#no difference

#############################################################################

# Using the "coin" package
# Exact Wilcoxon Mann Whitney Rank Sum Test
# where y is numeric and A is a binary factor

#LND
wilcox_test(LND_CPUE~BRT, data=ldace, distribution="exact") #p = 0.95
#SRD
wilcox_test(SRD_CPUE~BRT, data=sdace, distribution="exact") #p = 0.25
#Cottus
wilcox_test(Cottus_CPUE~BRT, data=cott, distribution="exact") #p = 1

# One-Way Permutation Test based on 9999 Monte-Carlo
# resamplings. y is numeric and A is a categorical factor

#LND
oneway_test(LND_CPUE~BRT, data=ldace,
            distribution=approximate(B=9999)) #p = 0.95
#SRD
oneway_test(SRD_CPUE~BRT, data=sdace,
            distribution=approximate(B=9999)) #p = 0.16
#Cottus
oneway_test(Cottus_CPUE~BRT, data=cott,
            distribution=approximate(B=9999)) #p = 0.91
#############################################################################

# Results:
# Longnose Dace:
# occurr = 33
# sympatry = 19, allopatry = 14
# no significant difference in CPUE when BRT present vs. absent
# Southern Redbelly Dace:
# occurr = 33
# sympatry = 12, allopatry = 21
# no significant difference in CPUE when BRT present vs. absent
# Sculpins:
# occurr = 20
# sympatry = 18, allopatry = 2
# no significant difference in CPUE when BRT present vs. absent

#############################################################################





#############################################################################

#---------------------------------CPUE models--------------------------------

#############################################################################
#libraries
library(pscl)
#library(MASS)
library(boot)

#explore data
names(newdat)


#inspect response variable(s)
ggplot(newdat, aes(LND_CPUE))+
  geom_histogram(binwidth = 10) #Longnose Dace (CPUE)

ggplot(newdat, aes(SRD_CPUE))+
  geom_histogram(binwidth = 10) #Southern Redbelly Dace CPUE

ggplot(newdat, aes(Cottus_CPUE))+
  geom_histogram(binwidth = 10) #Sculpins CPUE

#inspect predictor variables
predictors <- newdat %>%
  select(avwid, pctfines, pctriffle, BrBank, MEANT, HAiFLS_alt, HAiFLS_for, pctSlope, avgT, mFlow, 
         adult_100m, med_len, mean_len)

ggplot(predictors, aes(avwid))+
  geom_histogram(binwidth = 1)
ggplot(predictors, aes(pctfines))+
  geom_histogram(binwidth = 1)
ggplot(predictors, aes(pctriffle))+
  geom_histogram(binwidth = 1)
ggplot(predictors, aes(BrBank))+
  geom_histogram(binwidth = 1)
ggplot(predictors, aes(MEANT))+
  geom_histogram(binwidth = 1)
ggplot(predictors, aes(HAiFLS_alt))+
  geom_histogram(binwidth = 1)
ggplot(predictors, aes(HAiFLS_for))+
  geom_histogram(binwidth = 1)
ggplot(predictors, aes(pctSlope))+
  geom_histogram(binwidth = 1)
ggplot(predictors, aes(avgT))+
  geom_histogram(binwidth = 1)
ggplot(predictors, aes(mFlow))+
  geom_histogram(binwidth = 1)
ggplot(predictors, aes(adult_100m))+
  geom_histogram(binwidth = 1)
ggplot(predictors, aes(med_len))+
  geom_histogram(binwidth = 1)
ggplot(predictors, aes(mean_len))+
  geom_histogram(binwidth = 1)

#############################################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Let's build some CPUE models for each non-game fish with a combination of 
# hypotheses: 1) full model, 2) environment only, 3) BRT variables only, 
# 4) null model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Longnose Dace
# Intermediate tolerance, benthic, not TC, native eurythermal
# Hypotheses:
# Environment: %cobble(+), avwidth(+), pctSlope(-)
# BRT influence: (~) Habitat overlap seemingly with BRT, benthic sp. 
# maybe less susceptible to predation 

# Southern Redbelly Dace
# Intermediate tolerance, column, not TC, native eurythermal
# Hypotheses:
# Environment: MEANT(+), HAiFLS_alt(-), avdep(+)
# BRT influence: (-) column dwelling cyprinid - seems
# susceptible to predation

#Sculpins
# Intolerant, not TC, native coldwater, benthic
# Hypotheses:
# Environment: avgT(-), BrBank(-), HAiFLS_for(+), mFlow(-)
# BRT influence: (+) habitat overlap, BRT invade -- abundance more
# of a function of habitat since intolerant, and coldwater

#############################################################################
require(pscl)
require(MASS)
require(boot)


names(newdata)

#Longnose Dace all additive global model
#zero-inflated negative binomial model 

lnd.full.mod <- zeroinfl(LND_ab ~ avwid+pctcbbl+pctSlope+med_len+BRT_100m | 1,
               data = newdata,
               dist = "negbin",
               offset = log(SegLen)
               )
summary(lnd.full.mod)

lnd.env.mod <- zeroinfl(LND_ab ~ avwid+pctcbbl+pctSlope | 1,
                        data = newdata,
                        dist = "negbin",
                        offset = log(SegLen)
)
summary(lnd.env.mod)

lnd.brt.mod <- zeroinfl(LND_ab ~ med_len+BRT_100m | 1,
                        data = newdata,
                        dist = "negbin",
                        offset = log(SegLen)
)
summary(lnd.brt.mod)

lnd.null.mod <- zeroinfl(LND_ab ~ 1 | 1,
                        data = newdata,
                        dist = "negbin",
                        offset = log(SegLen)
)
summary(lnd.null.mod)

AIC(lnd.full.mod, lnd.env.mod, lnd.brt.mod, lnd.null.mod)

# emmeans on continuous predictors
# See Russ Lenth's long response to this question:
# https://stackoverflow.com/questions/52381434/emmeans-continuous-independant-variable
# Also the "basics" vignette to emmeans
library(emmeans)
library(MuMIn)

ref_grid(lnd.full.mod) #these are the mean values for all the covariates

# look at temperature and BRT_CPUE
summary(mydat$MEANT)
summary(mydat$BRT_CPUE)

# Plot at quantile values
emmip(negbin_mod, BRT_CPUE ~ MEANT, at = list(MEANT = c(9.76, 24.08), BRT_CPUE = c(0, 6.13, 7.25, 41.56)), type = "response")  +
  labs(title = "Effect of temperature on response, at levels of predator",
       subtitle = "Levels of predator are min = 25% quantile = 0, mean = 6.13, 75% quantile = 7.25, max = 41.56\nValues of other covariates held at their means")

emmip(negbin_mod, MEANT ~ BRT_CPUE, at = list(MEANT = c(9.76, 17.82, 18.98, 20.61, 24.08), BRT_CPUE = c(0, 41.5556)), type = "response") +
  labs(title = "Effect of predator on response, at levels of temperature",
       subtitle = "Levels of temperature are min = 9.76, 25% quantile = 17.82, mean = 18.94, 75% quantile = 20.61, max = 24.08\nValues of other covariates held at their means")

# Plot effect of temp only (other covariates at their mean)
temp_rg <- ref_grid(negbin_mod, at = list(MEANT = mydat$MEANT))
temp_val <- temp_rg@grid$MEANT
temp_pred <- predict(temp_rg, type = "response")

plot_df_temp <- data.frame(MEANT = temp_val, effect = temp_pred)
ggplot(plot_df_temp, aes(x = MEANT, y = effect)) + geom_line() +
  xlab("MEANT") + ylab("Predicted effect") +
  labs(title = "Predicted effect of mean temperature on response",
       subtitle = "Other covariates held at their mean values")

# Plot effect of predator
brt_rg <- ref_grid(negbin_mod, at = list(BRT_CPUE = mydat$BRT_CPUE))
brt_val <- brt_rg@grid$BRT_CPUE
brt_pred <- predict(brt_rg, type = "response")

plot_df_brt <- data.frame(BRT_CPUE = brt_val, effect = brt_pred)
ggplot(plot_df_brt, aes(x = BRT_CPUE, y = effect)) + geom_line() +
  xlab("BRT_CPUE") + ylab("Predicted effect") +
  labs(title = "Predicted effect of predator on response",
       subtitle = "Other covariates held at their mean values")

# Plot them together
plot_df_merge <- merge(plot_df_brt, plot_df_temp, by = "effect", all.x = T, all.y = T)
plot_df_both <- melt(plot_df_merge, id.vars = "effect"); rm(plot_df_merge)
ggplot(plot_df_both, aes(x = value, y = effect, color = variable)) + geom_line() +
  xlab("Predicted effect") + ylab("Value of covariate") +
  scale_color_discrete(name = "covariate") +
  labs(title = "Effect of individual covariates on response",
       subtitle = "All other covariate values held at their mean")
# You can extend this to keep adding your covariates

# Plot predicted vs observed
all_rg <- predict(negbin_mod, newdata = mydat[,c(3:8)]) # This gives you predictions at your data points
plot_df_compare <- data.frame(obs_value = mydat$LND_CPUE, estimate = all_rg)
ggplot(data = plot_df_compare, aes(x = obs_value, y = estimate)) + geom_point() +
  xlab("Observed CPUE") + ylab("Predicted CPUE")
# This looks weird because you're using a two-part model.  The line of points at zero is the zero-inflated part of the model.

