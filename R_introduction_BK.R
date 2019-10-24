# Intro to R Day 2

######## Topics #######
# Getting help
# Working directories
# Using R as a calculator
# Functions
# Downloading new packages
# Creating data and variables
# Preparing and loading data files into R
# Getting to know your data
# Square brackets - like an address for a dataset
# Manipulating parts of data tables

# ----- Getting help --------
# If you know a function name, you can use
#   the question mark ? to open a help file

?mean #opens help file 
?t.test

# Help files tell possible arguments
#   and give examples at the end

help.start()  # opens the links to online manuals and miscellaneous materials

# Or, open help tab (at right) and type name in
# Or, google it! (There are great R forums)

## YOUR TURN ##
#find the help file for the function that finds the maximum of the values within a given vector or matrix

?max


# ----- Working Directories ---------
# Determine your working directory

getwd()

# Set your working directory using setwd()
#   or by using "Set as working director" in the "More"
#   option in the "Files" tab on the right

setwd('/Users/Haldre/Desktop') #Why doesn't this work on your laptop?

# Working Directories & RStudio Projects
#when you are using projects, the working directory will automatically be set to the same folder as the Project. 
#If you want to access something within the project, you need to add the path to that file. 


## YOUR TURN ##
#Figure out where your working directory is currently located
getwd()
#set the working directory to your desktop (hint: can use the Session - Set working directory GUI to do this, and then see what the code is)
setwd("~/Desktop")
#set the working directory back to the project directory
setwd("/Users/brettkelly/Course-Materials")


# ----- Using R as a calculator ------
1 + 2
cos(pi)

# Arithmetic
#  +  add
#  -  subtract
#  *  multiply
#  /  divide
#  ^  exponent

# Relational
#  >   greater, less than
#  >=  greater than or equal to
#  !=  not equal to

# Logical
#  !  not
#  &  and
#  |  or


## YOUR TURN ##
# Do some math! 
#1) There is a group of 10 people who are ordering pizza. If each person gets 2 slices and each pizza has 4 slices, how many pizzas should they order?
20 / 4 # = 5, they should order 5 pizzas (20 slices), so that everyone can eat 2
#2) Ask R whether 3.1415932 is greater than pi
3.1415932>pi
#TRUE!

#3) What is your age cubed? 
24^3
#13824

# ----- Creating data and variables ------


# Make a vector with concatenate, c()

c(5, 7, 3, 14)

# Or save this as something

dogAges <- c(5, 7, 3, 14)

# Type the name to see it

dogAges

# Perform functions on vectors

mean(dogAges)

dogAges2 <- dogAges*2
dogAges2

# Combine vectors

dogAges <- cbind(dogAges, dogAges2)
dogAges


## YOUR TURN ##
#create a vector of the heights (in inches) of everyone at your table

pplHeights <- c(67, 70, 62.75, 70, 63)

# ----- Functions ------

# Have form: function(argument,argument,argument,...)

# Here, curve is the function and it can interpret
#   the 2*x as the function I want to graph and
#   "from" and "to" as arguments to specify x axis length

?curve #see what the help file says

curve(2*x, from=0,to=8)


## YOUR TURN ##

#use the mean function to take the average of the heights at your table
mean(pplHeights)
# mean is 66.55

#what are the default arguments for the mean function? Include them in your code. 
?mean

mean(pplHeights, trim = 0, na.rm = FALSE)
#Use the "trim" argument to remove 0.25 from each end of your height vector before calcluating the mean. what did this do?  
mean(pplHeights, trim = 0.25, na.rm = TRUE)

# ----- Downloading new packages ------

# If the function you want is in a
#   different package, use install.packages() (or use Packages tab in RStudio)

install.packages("lme4")

# To load this so R can use it, use library() (or check box in Packages tab on RStudio)

library(lme4)


## YOUR TURN ##
#install the tidyverse package (or really, suite of packages)
install.packages("tidyverse")
#load tidyverse
library(tidyverse)
# ----- Preparing and loading data files into R ------

#   Can use .csv or .txt files or excel files

# Read file using read.csv, naming it something. Note that this must be in your 
# working directory

spider <- read.csv("1. Intro to R/data/spider.csv", header = TRUE)

# You can also use use file.choose()
spider <- read.csv(file.choose())

# Reading in excel files is easy too, with readxl package
library(readxl)

spiderprey <- read_excel("1. Intro to R/data/Prey_Capture_Final_Do_Not_Touch.xlsx", sheet = 2)

## Your Turn ##

# 1) Read in the first sheet of the Prey_Capture_Final excel file. Name it something appropriate for the content. 
spiderMetaData <- read_excel("1. Intro to R/data/Prey_Capture_Final_Do_Not_Touch.xlsx", sheet = 1)
# 2) Use the Import Dataset tool to bring in the dataset and change the classes of columns to something else (e.g. date, character etc.). Then look at the code that ran in the console below. 


# ----- Getting to know your data ------

# What does R interpret this as? use class()

class(spider)

# Good! R interprets it as a data frame

# Look at the dimensions - rows by cols

dim(spider)

# Look at the first rows with head()

head(spider)

# What are the column names? Row names? 

colnames(spider)
rownames(spider)

# How are the rows, columns labeled?

labels(spider)

# Summarize your data

summary(spider)

# R describes data as numerical, factors, and integers
# Use str(data) to see what it is describing your data

str(spider)

# Change class using as.factor(), as.numeric(), as.integer(), as.character()

spider$survey <- as.factor(spider$survey)

str(spider)


## YOUR TURN ## 

#Using the Spider Prey dataset, do the following: 

#1) Find out what R interprets this object as. 

#2) Determine the dimensions of the dataframe.

#3) Look at the head of the dataset. How many rows does that show?

#4) Look at the column names and row names - are they logical? 

#5) Look at the summary & the structure of your dataframe. 

#6) change the site from a character to a factor class. 



# ----- Discussing your data with your computer ------

# To describe cells in your data frame,
#   R uses the form data[i,j]
#   where i is row, j is column
#   Or, data$column to describe columns

# Specific cells
spider[2,5]

# Specific row
spider[2,]

# Specific column
spider[,5]

# OR, data$column

spider$island

# OR, data[['column']]

spider[['island']]

## YOUR TURN ### 

#With the spiderprey dataset, answer the following: 

#1) What is the name of the web in the 12th row of data? 

#2) Pull out all of the values in the totalprey column using 3 different approaches. 


# ----- Manipulating parts of data tables ------

# Create a vector by calculating
# This isnt automatically attached to the "spider" data frame
webs50m <- (spider$tot_webs/spider$length)*50

# To attach, use cbind() to add "disolved" to "flow"
spider <- cbind(spider,webs50m)

# Make sure the new column is there
head(spider)

## YOUR TURN ###
#using the spiderprey dataset...

#1) Make a new column that sums values from obs1 to obs8. Check to see if this matches the values in totalprey column. 
