# EEOB590A
# Data_visualization practice exercise
# Please use ggplot2 for all graphing below
# And don't forget to use your dplyr functions like filter, group_by, summarize, and select
library(tidyverse)
library(ggplot2)
library(lubridate)

#1) Read in poll_long_tidy.csv
pollong <- read.csv("data/tidy/poll_long_tidy.csv")

#inspect data
head(pollong)
names(pollong)

#remove extra columns
poll <- pollong %>% 
  select(uniqueID, island, site, transect, topcolor, bowlcolor, insectorder,
         numinsects, dateout, datecoll, duration)
names(poll)

#look at data summary and structure
summary(poll)
str(poll)

#change dates to dates using lubridate
poll$dateout <- ymd(as.character(poll$dateout))
poll$datecoll <- ymd(as.character(poll$datecoll))
class(poll$datecoll)

#2) Use table and dplyr to calculate the number of top colors and bowl colors at each transect.
#Each transect was supposed to have each topcolor represented once, with all four (b, r, w, and y) bowl colors under each top color. 

topcol <- with(poll, ftable(site, transect, topcolor))
bowlcol <- with(poll, ftable(site, transect, bowlcolor))
bowlcol

#3a) Make a histogram for numinsects. 

NsectPlot <- ggplot(data = poll, aes(numinsects)) +
                      geom_histogram()
NsectPlot

#3b) Make another histogram for numinsects but omit all rows with 0 insects
transplant_guam <- transplant %>%
  filter(island == "guam")

poll_nozeros <- poll %>%
  filter(numinsects > 0)

nsect2 <- ggplot(data = poll_nozeros, aes(numinsects)) +
  geom_histogram(binwidth = 10)
nsect2

#4a) Make a barplot to show the counts for each level of the site. Were sites evenly sampled? 

barplot1 <- ggplot(data = poll, aes(site)) +
  geom_bar()
barplot1 #no, ladtg was sampled much less frequently

#4b) Make a graph to visualize the duration pan traps were left out, and use a table to do the same thing.
#Is there any variation in duration that we will need to account for in a model? 
summary(poll$duration)
dur_plot <- ggplot(data = poll, aes(site, duration, color = island)) +
  geom_boxplot()
dur_plot #no variation

dur_table <- with(poll, ftable(site, transect, duration))
dur_table #no variation, not the best table representation -- shows tally of 3 day duration
summary(poll$duration) #another way to see there is no variation, not exactly a table?

#5) Figure out the top 6 most abundant orders 
#(hint, use the top_n() function; google it for more info).
poll %>%
  group_by(insectorder) %>%
  tally(numinsects) %>%
  top_n(6)
#Then, filter the original dataset so you only have those orders.
names(poll)
class(poll$insectorder)
topordr <- poll %>%
  filter(insectorder == "Apoidea" | insectorder == "Crabronidae" | insectorder == "Diptera" |
           insectorder == "Formicidae" | insectorder == "Hemiptera" | insectorder == "Lepidoptera")


#Then, create boxplots of the number of insects per order with different colors indicating the island.
ggplot(topordr, aes(insectorder, numinsects, color = island)) +
  geom_boxplot()

#Do you notice any general trends in insect abundances by islands? 
#besides order Formicidae, Saipain traps appear to collect more insects

#6) Use the top 6 orders dataset you created above,
#along with the facet_grid argument to create a set of graphs with insect order on the x axis,
#number of insects on the y,
#and rows of graphs for each topcolor and columns of graphs for the island. 
plot <- ggplot(topordr, aes(insectorder, numinsects)) +
  geom_boxplot()
plot + facet_grid(vars(topordr$topcolor), vars(topordr$island))
#done








