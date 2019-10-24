
##############################################

# RMark tutorials - from Appendix C in book ##

##############################################

install.packages("RMark")
library(RMark)

#load dipper data, it is actually contained within RMark
data(dipper)

#explore data
summary(dipper)
head(dipper)

#simple analysis run
myexample <- mark(dipper)

list.files()

#assigned "myexample" to a list
#check class of list; get a description of type of model used in MARK
class(myexample)

#look at the PIM structure 
PIMS(myexample, "Phi", simplified = F)
PIMS(myexample, 'p', simplified = F)

#creating design matrices for models for p (recap probability in these examples)

#p~time model
model.matrix(~time, myexample$design.data$p[1:10,])

#p~Time model
model.matrix(~Time, myexample$design.data$p[1:10,])

#p~Time+age
model.matrix(~Time+age, myexample$design.data$p[1:10,])
