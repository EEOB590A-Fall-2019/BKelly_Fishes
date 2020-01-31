#############################
##  Summary Stats of Data
#############################

#load data
data <- read.csv("Data/Thesis/Tidy/TroutStatus_sites.csv", header = T)

str(data)

summary(data$HUC8)

#mean wetted width

#need covariate data
dat <- read.csv("Data/Thesis/Tidy/BKT_occDF_RMARK.csv", header = T)

names(dat)

max(dat$avwid) #11.2
min(dat$avwid) #0.82
hist(dat$avwid)
boxplot(dat$avwid)
