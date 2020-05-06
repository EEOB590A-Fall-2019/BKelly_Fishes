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
#libraries
library(pscl)
#library(MASS)
library(boot)


names(newdata)

#Longnose Dace Count = response, segment length = offset
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

#Compare models
library(MuMIn)
lnd.mod.AICc <- model.sel(lnd.full.mod, lnd.env.mod, lnd.brt.mod, lnd.null.mod)
lnd.mod.AICc

#export model table
lnd.cpue.table <- as.data.frame(lnd.mod.AICc) %>%
  select(df, logLik, AICc, delta, weight)

write.csv(lnd.cpue.table, "Data/Thesis/Tidy/lnd_cpue_ModelTable.csv", row.names = T)


#-----
#extracting confidence intervals for the parameters

#top model 
dput(round(coef(lnd.full.mod, "count"), 4)) #count process
dput(round(coef(lnd.full.mod, "zero"), 4)) #extra zero process

f <- function(data, i) {
  require(pscl)
  m <- zeroinfl(LND_ab ~ avwid+pctcbbl+pctSlope+med_len+BRT_100m | 1,
                data = data[i, ],
                dist = "negbin",
                offset = log(SegLen),
                start = list(count = c(-7.724,0.7464,0.0186,-0.0781,
                                       0.0027,-0.0808),
                             zero = c(-8.7639)))
  as.vector(t(do.call(rbind, coef(summary(m)))[, 1:2]))
}

set.seed(10)
(res <- boot(newdata, f, R = 10000))

## basic parameter estimates with percentile and bias adjusted CIs
parms <- t(sapply(c(1, 3, 5, 7, 9, 11, 15), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "bca"))
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5],
              bcaLL = bca[4], bcaUL = bca[5]))
}))

## add row names
row.names(parms) <- names(coef(lnd.full.mod))
## print results
parms

## compare with normal based approximation
confint(lnd.full.mod)

## exponentiated parameter estimates with percentile and bias adjusted CIs
expparms <- t(sapply(c(1, 3, 5, 7, 9, 11, 15), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "bca"), h = exp)
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5],
              bcaLL = bca[4], bcaUL = bca[5]))
}))

## add row names
row.names(expparms) <- names(coef(lnd.full.mod))
## print results
expparms


#########################################################################
#width
min.width <- min(newdata$avwid)
max.width <- max(newdata$avwid)
width.values <- seq(from = min.width, to = max.width, length = 100)
mean.width <- mean(newdata$avwid)
#cobble
min.cobble <- min(newdata$pctcbbl)
max.cobble <- max(newdata$pctcbbl)
cobble.values <- seq(from = min.cobble, to = max.cobble, length = 100)
mean.cobble <- mean(newdata$pctcbbl)
#slope
min.slope <- min(newdata$pctSlope)
max.slope <- max(newdata$pctSlope)
slope.values <- seq(from = min.slope, to = max.slope, length = 100)
mean.slope <- mean(newdata$pctSlope)
#median TL
min.length <- min(newdata$med_len)
max.length <- max(newdata$med_len)
length.values <- seq(from = min.length, to = max.length, length = 100)
mean.length <- mean(newdata$med_len)
#brown trout density
min.trout <- min(newdata$BRT_100m)
max.trout <- max(newdata$BRT_100m)
trout.values <- seq(from = min.trout, to = max.trout, length = 100)
mean.trout <- mean(newdata$BRT_100m)
med.trout <- median(newdata$BRT_100m)
#Segment Length
segment <- mean(newdata$SegLen)
segment <- 100
#########################################################################

#new df for predictions: based on width
names(newdata)

lnd.width.df <- data.frame(avwid = width.values,
                           pctcbbl = mean.cobble,
                           pctSlope = mean.slope,
                           med_len = mean.length,
                           BRT_100m = mean.trout,
                           SegLen = segment)

lnd.width.df$phat <- predict(lnd.full.mod, lnd.width.df)

ggplot(lnd.width.df, aes(x = avwid, y = phat)) +
  #geom_point() +
  geom_line() +
  labs(x = "Mean Wetted Width (m)", y = "Predicted Longnose Dace Count")








