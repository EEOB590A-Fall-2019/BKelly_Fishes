# negative binomial glm example
# response = fish count
# predictors = environmental conditions (habitat and brown trout)
# offset or exposure variable = length of stream segment sampled


#libraries
library(tidyverse)
library(foreign)
library(MASS)


#load data
newdata <- read.csv("Data/Thesis/Tidy/cpue_data.csv", header = T)


#inspect response variable(s)
ggplot(newdata, aes(LND_CPUE))+
  geom_histogram(binwidth = 10) #Longnose Dace (CPUE)

ggplot(newdata, aes(SRD_CPUE))+
  geom_histogram(binwidth = 10) #Southern Redbelly Dace CPUE

ggplot(newdata, aes(Cottus_CPUE))+
  geom_histogram(binwidth = 10) #Sculpins CPUE

#inspect predictor variables
#----------------------------------------------------------------

#Longnose Dace Count = response
#offset variable = SegLen or segment length (m)
#Predictors:
# avwid - stream width
# pctcbbl - % cobble substrate
# pctSlope - average watershed gradient (%)
# med_len - median Brown Trout length for that site
# BRT_100m - Brown Trout density

lnd.full.mod <- glm.nb(LND_ab ~ avwid+pctcbbl+pctSlope+med_len+BRT_100m+offset(log(SegLen)),
                         data = newdata)
summary(lnd.full.mod)

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
segment <- 100

lnd.width.df <- data.frame(avwid = width.values,
                           pctcbbl = mean.cobble,
                           pctSlope = mean.slope,
                           med_len = mean.length,
                           BRT_100m = mean.trout,
                           SegLen = segment)

lnd.width.df <- cbind(lnd.width.df, predict(lnd.full.mod, lnd.width.df, type = "link", se.fit=TRUE))

lnd.width.df <- within(lnd.width.df, {
  Pr_LND <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})

ggplot(lnd.width.df, aes(avwid, Pr_LND)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = "red"), alpha = .25) +
  geom_line(aes(colour = "black"), size = 2) +
  labs(x = "Mean Wetted Width (m)", y = "Predicted LND Count")+
  scale_y_continuous(limits = c(0,175))
#----------------------------------------------------------------


