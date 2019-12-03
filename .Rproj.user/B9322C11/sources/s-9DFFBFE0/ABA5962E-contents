##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## Learing Occupancy Analysis in unmarked ##
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

##-----------------------------------------------------------------##
## Based off of the book: Applied Hierarchical Modeling in Ecology ##
##          Volume 1. Marc Kery and J. Andrew Royle                ##
##-----------------------------------------------------------------##

##Chapter 10: Modeling Static Occurrence
##10.3 Simulation and Analysis of the simplest possible site-occupancy model

#install.packages("unmarked")
library(unmarked)

#choose sample sizes and prepare observed data aray (y)
set.seed(24)
M <- 100 #number of sites
J <- 2 #number of occasions
y <- matrix(NA, nrow = M, ncol = J) #an encounter history matrix

#set parameter values
psi <- 0.8 #occupancy probability
p <- 0.5 #detection probability

#generate presence/absence data
z <- rbinom(n=M, size = 1, prob = psi) #simulated binomial distribution or aka Bernoulli with 100 observations and probability of occupancy = 0.8

#generate detection/non-detection data
for(j in 1:J) {
  y[,j] <- rbinom(n=M, size =1, prob = z*p)
}

#inspect data
sum(z) #true number of occupied sites

sum(apply(y,1,max)) #observed number of occupied sites

#can look at true state of occupancy alongside detection/non-detection data -- here we just inspect the first few rows as an exmaple
head(cbind(z=z,y1=y[,1],y2=y[,2])) #truth=z, and status of sampling occasions 1 and 2 are in y1 and y2

##-----
## now analyze the data using package "unmarked" and funtion "occu" where the linear model for detection is specified b4 occupancy
##-----

umf <- unmarkedFrameOccu(y = y) #create unmarked data frame
summary(umf) #awesome summary capabilities - very straightforward and helpful

(fm1 <- occu(~1~1, data = umf)) #fit model - intercept model for both parameters ~p,~psi
##output:
#Call:
  #occu(formula = ~1 ~ 1, data = umf)

#Occupancy:
 # Estimate    SE    z     P(>|z|)
#    1.04   0.394   2.65   0.00807

#Detection:
 # Estimate   SE    z   P(>|z|)
#0.329      0.26  1.26   0.207

#AIC: 270.2257
##----------------------------------

#get estimates on probability scale
backTransform(fm1, "state") #psi estimate
backTransform(fm1, "det") #p estimate


##-----------------------------------##
## Now do an example with a covarite effects on psi and p (1 covariate for each parameter in this example)
##-----------------------------------##

#choose sample sizes and preare obs. data array y
set.seed(1)
M <- 100 #number of sites
J <- 3 #number of occasions
y <- matrix(NA, nrow = M, ncol = J) #to contain observation data

#create covariate of vegetation height (vegHt)
vegHt <- sort(runif(M,-1,1)) #"sort for graphical convenience" 
  # ^ it appears this is basically a simulated covariate that they have standardized to mean = 0. 

#choose parameter values for occupancy model and compute occupancy
beta0 <- 0 #logit-scale intercept
beta1 <- 3 #logit-scale slope for veg height
psi <- plogis(beta0+beta1*vegHt) #occupancy probability
plot(vegHt, psi, ylim = c(0,1), type="l", lwd=3) #plot psi relationship

#now visit each site and observe presence/absence perfectly
z <- rbinom(M,1,psi) #true presence/absence

#look at data so far
table(z)

#plot true system state
par(mfrow=c(1,3), mar=c(5,5,2,2), cex.axis=1.5, cex.lab=1.5)
plot(vegHt, z, xlab="Veg Height", ylab="True pres/abs (z)", frame=F, cex=1.5)
plot(function(x)plogis(beta0+beta1*x), -1,1, add=T, lwd=3, col="red")


































