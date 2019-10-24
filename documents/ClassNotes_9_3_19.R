#-------------------------------------------------
# Introduction to RStudio - Class Notes 09/03/2019
#-------------------------------------------------

# what/where is my working directory? Let's Check!
getwd()
# Same PLace as where your Project File (BKelly_Fishes) is housed

# R Data Structures

# a 1-D data set is classified as a vector (ex. list)
# a 2-D data set is a matrix (data frame/tibble)
# N-D is an array 

#data can have classes; use function class() to check type of data 
Vector1 <- 1:20 # create a vector 
Vector1 # make sure it was created properly
class(Vector1) # check data class

# concatenate function with c()
Vector2 <- c("red","yellow","blue")
Vector2 #see that it was created as intented
class(Vector2) #check class

# How can I "call" or pull a cell from a df or vector?? 
# Using [,] - Data Frame with 2-D structure
# Using [] with vector data
# Let's try it out
Vector2[2] # it returns "yellow" which is the second color in the list of Vector2!


