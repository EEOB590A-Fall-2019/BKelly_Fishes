library(ordinal)
library(ggplot2)
library(car)

### Massage the dataset
mydat <- read.csv("Data/Thesis/Tidy/FIBI_tidy2.csv", header = T)
mydat$Rating <- factor(mydat$Rating, levels = c("Very Poor", "Poor", "Fair", "Good", "Excellent"))
mydat$Year <- factor(mydat$Year, levels = c("2018", "2019"))
table(mydat$Rating)
with(mydat, table(Rating, HUC8))

mydat$HUC10 <- as.factor(sub('.*(?=.{3}$)', '', mydat$HUC_10, perl=T))
mydat$HUC12 <- as.factor(sub('.*(?=.{2}$)', '', mydat$HUC_12, perl=T))
mydat$watershed_med <- as.factor(paste(mydat$HUC8, mydat$HUC10, sep = "_"))
mydat$watershed_sm <- as.factor(paste(mydat$watershed_med, mydat$HUC12, sep = "_"))

with(mydat, table(Rating, watershed_med))
with(mydat, table(Year, watershed_med))

### Models
ord_mod_eqi <- clmm2(Rating ~ Year + MEANT + pctrun + pctrock + pctShade + pctBrBnk + HAiFLS_dev + HAiFLS_for, random = watershed_med, data = mydat, threshold = "equidistant", Hess = T, nAGQ=10)

ord_mod <- clmm2(Rating ~ Year + MEANT + pctrun + pctrock + pctShade + pctBrBnk + HAiFLS_dev + HAiFLS_for, random = watershed_med, data = mydat, Hess = T, nAGQ=10)

ord_mod_fix <- clm2(Rating ~ Year + watershed_med + MEANT + pctrun + pctrock + pctShade + pctBrBnk + HAiFLS_dev + HAiFLS_for, data = mydat)

anova(ord_mod, ord_mod_eqi) # Flexible thresholds are better, barely
anova(ord_mod_eqi, ord_mod_fix) # Random watershed is better than fixed
ord_mod <- ord_mod_eqi  # use the equidistant model

summary(ord_mod)  # This is really all you need to decide which env variables are important - look a the p-values

### Predictions
  # Plot estimated probability for data class
predictions <- predict(ord_mod) # fit is the probability associated with the class given in the data, conditioned on watershed
predictions <- data.frame(watershed = mydat$watershed_med,
                          year = mydat$Year,
                          IBIscore = mydat$IBIScore,
                          Rating = mydat$Rating,
                          fit = round(predictions, 4))
predictions$obs <- rownames(predictions)
predictions$fit_rank <- rank(predictions$fit, ties.method = "first")

ggplot(predictions, aes(x = Rating, y = fit)) + geom_boxplot() +
  ylab("estimated probability") + ggtitle("Probabilities by IBI Rating") +
  geom_point(data = predictions, aes(x = Rating, y = fit, color = watershed), size = 2) +
  theme(legend.position = "none")

ggplot(predictions, aes(x = IBIscore, y = fit)) + geom_point() +
  ylab("estimated probability") + xlab("IBI score") +
  ggtitle("Fitted probabilities of observation landing in class identified by data, by IBI score")

ggplot(predictions, aes(x = fit_rank, y = fit, color = IBIscore)) + geom_point() +
  ylab("estimated probability") + 
  scale_x_continuous(name = NULL, breaks = NULL, labels = NULL) +
  ggtitle("Fitted probabilities of observation landing in class identified by data")

ggplot(predictions, aes(x = fit, fill = Rating)) + geom_histogram(binwidth = 0.025)

predictions_low <- predictions[which(predictions$fit < 0.3),]
ggplot(predictions_low, aes(x = fit, fill = Rating)) + geom_histogram(binwidth = 0.01)
