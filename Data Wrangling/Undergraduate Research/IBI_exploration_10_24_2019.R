#read in the tidy IBI file and then create a data report
library(DataExplorer)
library(skimr)
library(tidyverse)
#read in data, check working directory
getwd()

?read_csv
IBI <- read_csv("Data/Thesis/Tidy/tidy_IBI1.csv", col_names = TRUE)

#explore data
skim(IBI)
create_report(IBI)
summary(IBI)

