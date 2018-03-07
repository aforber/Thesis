#-----------------------------
### COT THESIS
### AUTHOR: ALYSSA FORBER
### DATE: JANUARY 16, 2018
### DATE MODIFIED: 
#-----------------------------

rm(list=ls())

sim1 <- read.csv("/Users/alyssaforber/Documents/Denver/Thesis/Results/Simulation/Sim1_20180120.csv")
sim3 <- read.csv("/Users/alyssaforber/Documents/Denver/Thesis/Results/Simulation/Sim3_20180120.csv")
sim5 <- read.csv("/Users/alyssaforber/Documents/Denver/Thesis/Results/Simulation/Sim5_20180120.csv")
sim10 <- read.csv("/Users/alyssaforber/Documents/Denver/Thesis/Results/Simulation/Sim10_20180122.csv")
sim25 <- read.csv("/Users/alyssaforber/Documents/Denver/Thesis/Results/Simulation/Sim25_20180122.csv")
sim50 <- read.csv("/Users/alyssaforber/Documents/Denver/Thesis/Results/Simulation/Sim50_20180123.csv")


# putting output into correct format function
library(stringr)

myformat <- function(sim){
  sim <- sim[1:8,]
  sim <- cbind(sim, type = str_split_fixed(sim$X, " ", 2))
  sim$Specificity <- as.numeric(as.character(sim$Specificity))
  sim
}

sim1 <- myformat(sim1)
sim3 <- myformat(sim3)
sim5 <- myformat(sim5)
sim10 <- myformat(sim10)
sim25 <- myformat(sim25)
sim50 <- myformat(sim50)


#---------------------------
# MAKE ONE RESULTS TABLE
#---------------------------

# We want threshold, then sens, then spec, then accuracy (which we don't have yet)
# Want in order, and just for 3, 5, 10, and 50%
# Need it have .5 first and then youden-- switch all to match other tables

library(data.table)

myswitch <- function(sim){
  # re-order rows to match other tables in paper
  sim <- rbind(sim[2,], sim[1,], sim[4,], sim[3,], sim[6,], sim[5,], sim[8,], sim[7,])
  # re-order columns to match
  sim <- setcolorder(sim, c(1,5,3,2,4,6,7))
  sim
}

sim3S <- myswitch(sim3)
sim5S <- myswitch(sim5)
sim10S <- myswitch(sim10)
sim50S <- myswitch(sim50)

fill <- c(NA,NA, NA, NA,NA, NA, NA)
simTab <- rbind(fill,sim3S[,c(1:5)], 
                fill,sim5S[,c(1:5)],
                fill,sim10S[,c(1:5)],
                fill,sim50S[,c(1:5)])

# 1, 10, 19, 28 row first column need the prevalence



#--------------------------
# PLOT
#--------------------------


x1 <- rep(1, 8)
x2 <- rep(2, 8)
x3 <- rep(3, 8)
pts <- rep(c(16, 17),4)
cls <- rep(c("purple", "blue", "red", "green4"), each=2) # make youden dot, 0.5 triangle


# TURN PLOT INTO FUNCTION
plotsim <- function(sim, percent){
  
  plot(x=x1, sim[,2], pch=pts,  ylab="", xlab="", xaxt = "n", main = paste("Outcome = ", percent, "%"), xlim=c(0.5,3.5),
     ylim=c(0,1),col=cls)
  lines(x=x2, sim[,3], pch=pts, type="p", col=cls)
  lines(x=x3, sim[,4], pch=pts,type="p",col=cls)
  axis(1, at=1:3, labels=c("Specificity", "Sensitivity", "AUC"))
  legend(x=0.5, y=.4, legend=c("Full", "Down", "Up", "SMOTE"), col=c("purple", "blue", "red", "green4"), 
       cex=.8, bty="n", pch=16)
  legend(x=2.8, y=.28, legend=c("Youden", "0.5 cutoff"), col="black", pch=c(16,17), bty="n", cex=0.8)
}

plotsim(sim10, 10)
# they all pretty much look identical

#----------
# PLOT OF SENSITIVITY AGAINST PREVALENCE
#----------

# the sensitivities are 0 for ones less than 25

sensYoud <- cbind(c(1, 3, 5, 10, 25, 50), c(sim1[1,3], sim3[1,3], sim5[1,3], sim10[1,3], sim25[1,3], sim50[1,3]))

sens5 <- cbind(c(1, 3, 5, 10, 25, 50), c(sim1[2,3], sim3[2,3], sim5[2,3], sim10[2,3], sim25[2,3], sim50[2,3]))

# PLOT WITH POINTS
plot(sens5, pch=16, ylim=c(0,1), xlab="Prevalence", ylab="Sensitivity", main="Sensitivity vs Prevalence")
lines(sensYoud, type="p", pch=16, col="red")
lines(sensYoud, col="red", lwd=2)
lines(sens5, lwd=2)
legend(x=38, y=.27, legend=c("Youden", "0.5"), bty="n", col=c("red", "black"), pch=16, ncol=1, cex=.9)

#----------
# PLOT OF PREVALENCE BY THRESHOLD 
#----------

thresh <- cbind(c(1, 3, 5, 10, 25, 50), c(sim1[1,5], sim3[1,5], sim5[1,5], sim10[1,5], sim25[1,5], sim50[1,5]))
plot(thresh, pch=16, xlab = "Prevalence %", ylab ="Youden Threshold", main="Threshold vs Prevalence")
lines(thresh, lwd=2)


