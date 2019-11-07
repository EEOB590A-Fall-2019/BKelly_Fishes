#read in the tidy IBI file and then create a data report
library(DataExplorer)
library(skimr)
library(tidyverse)
library(readxl)
#read in data, check working directory
getwd()


IBI <- read_csv("Data/Thesis/Tidy/tidy_IBI1.csv", col_names = TRUE)

#explore data
skim(IBI)
create_report(IBI)
summary(IBI)

#load sam's code and compare to mine
SamIBI <- read_xlsx("Data/Undergraduate Research/Raw/IBIscores_SamG.xlsx", col_names = T, sheet = 6)
skim(SamIBI)
names(SamIBI)
SamScores <- SamIBI %>%
  select(-HUC8, -site) %>%
  mutate_if(is.character, as.numeric)
summary(SamScores)

#plot scores against each other
plot(SamScores$TOTSPscore, IBI$M1_spp)
plot(SamScores$`#CWSPscore`, IBI$M2_CWspp)
plot(SamScores$`#CWINDscore`, IBI$M11_CWindv150)

class(IBI$Rating)
hist(IBI$Rating)
IBI$Rating <- as.factor(IBI$Rating)
plot_bar(IBI$Rating)
levels(IBI$Rating)

x <- IBI[4,]
