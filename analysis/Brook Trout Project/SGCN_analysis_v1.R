library(tidyverse)
library(lme4)
library(MASS)
library(car)
library(ggResidpanel)
library(corrplot)
library(GLMMadaptive)

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
mydat <- read.csv("Data/Thesis/Tidy/SGCN_AllCovariates.csv", header=T)

#arrange watersheds as factors
mydat$HUC10 <- as.factor(sub('.*(?=.{3}$)', '', mydat$HUC_10, perl=T))
mydat$HUC12 <- as.factor(sub('.*(?=.{2}$)', '', mydat$HUC_12, perl=T))
mydat$watershed_med <- as.factor(paste(mydat$HUC8, mydat$HUC10, sep = "_"))
mydat$watershed_sm <- as.factor(paste(mydat$watershed_med, mydat$HUC12, sep = "_"))
mydat$HUC8 <- as.factor(mydat$HUC8)
mydat <- mydat %>%
  mutate_at(vars(c("BRT","LND","SRD","Cottus")), as.factor)
str(mydat)
#############################################################################

#----------------------------Boxplots of CPUE------------------------------#

#-----------------------Filter by presence of SGCN-------------------------#

#LND
ldace <- mydat %>%
  filter(LND == 1)
ggplot(data = ldace, aes(x=BRT,y=LND_CPUE)) +
  geom_boxplot(aes(fill=BRT))+
  labs(x="Brown Trout Presence", y="Longnose Dace CPUE (fish/100m)")+
  theme_minimal()+
  scale_x_discrete(labels=c("Absent", "Present"))+
  theme(legend.position = "NULL")

#SRD
sdace <- mydat %>%
  filter(SRD == 1)
ggplot(data = sdace, aes(x=BRT,y=SRD_CPUE)) +
  geom_boxplot(aes(fill=BRT))+
  labs(x="Brown Trout Presence", y="Southern Redbelly Dace CPUE (fish/100m)")+
  theme_minimal()+
  scale_x_discrete(labels=c("Absent", "Present"))+
  theme(legend.position = "NULL")

#Sculpins
cott <- mydat %>%
  filter(Cottus == 1)
ggplot(data = cott, aes(x=BRT,y=Cottus_CPUE)) +
  geom_boxplot(aes(fill=BRT))+
  labs(x="Brown Trout Presence", y="Sculpin CPUE (fish/100m)")+
  theme_minimal()+
  scale_x_discrete(labels=c("Absent", "Present"))+
  theme(legend.position = "NULL")


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
library(coin)
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

#---------------------------------CPUE models--------------------------------

#############################################################################

#inspect response variable(s)
ggplot(mydat, aes(LND_CPUE))+
  geom_histogram(binwidth = 10) #Longnose Dace (CPUE)

ggplot(mydat, aes(SRD_CPUE))+
  geom_histogram(binwidth = 10) #Southern Redbelly Dace CPUE

ggplot(mydat, aes(Cottus_CPUE))+
  geom_histogram(binwidth = 10) #Sculpins CPUE


#inspect collinearity of predictors
predictors <- mydat %>%
  select_at(vars(20:33))

M <- cor(predictors)
corrplot(M, method = "number", type = "upper")

#correlated predictors to not include in same model (|r| > 0.6)

#Local:
#pctfines & pctcbbl
#pctfines & pctrock
#pctfines & pctriffle
#pctcbbl & pctrock
#pctcbbl & pctriffle
#pctrock & pctriffle
#mFlow & pctpool
#Catchment:
#HAiFLS_alt & HAiFLS_for

#Honorable mentions (|r| > 0.5):
#avwid & avdep (0.5)
#pctriffle & pctrun (-0.54)
#pctrun & pctpool (-0.52)

#############################################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Let's build some CPUE models for each non-game fish with a combination of 
# hypotheses: 1) only environment, 2) Brown Trout CPUE, 3) other "top carnivore" CPUE, 
# 4) BRT+environment, 5) OTC+environment, 6) null
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Longnose Dace
# Intermediate tolerance, benthic, not TC, native eurythermal
# Hypotheses:
# Environment: MEANT(+), %riffle(+), %cobble(+), mflow(+), avwidth(+), HAiFLS_alt(-)
# BRT influence: (~) Habitat overlap seemingly with BRT, benthic sp. 
# maybe less susceptible to predation 

# Southern Redbelly Dace
# Intermediate tolerance, column, not TC, native eurythermal
# Hypotheses:
# Environment: MEANT(+), %fines(+), %run(+), mdepth(m), HAiFLS_alt(-)
# BRT influence: (-) column dwelling cyprinid - seems
# susceptible to predation

#Sculpins
# Intolerant, not TC, native coldwater, benthic
# Hypotheses:
# Environment: MEANT(+), %cobble(+), HAiFLS_for(-), pctBrBnk(-), pctShade(+), pctpool(+)
# BRT influence: (+) habitat overlap, BRT invade -- abundance more
# of a function of habitat since intolerant, and coldwater

#############################################################################

#Longnose Dace all additive global model with watershed_small as random effect
#zero-inflated negative binomial model fitted using GLMMadaptive package, mixed_model() function

lnd_GLMM <- mixed_model(fixed = LND_CPUE ~ MEANT+pctcbbl+mFlow+avwid+HAiFLS_alt+BRT_CPUE,
                        random = ~ 1 | watershed_sm, data = mydat,
                        family = zi.negative.binomial(), zi_fixed = ~ 1, iter_EM=0)


summary(lnd_GLMM)
