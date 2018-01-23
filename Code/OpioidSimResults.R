#-----------------------------
### COT THESIS
### AUTHOR: ALYSSA FORBER
### DATE: JANUARY 16, 2018
### DATE MODIFIED: 
#-----------------------------

rm(list=ls())

sim1 <- read.csv("/Users/alyssaforber/Documents/Denver/Thesis/Results/Sim1_20180120.csv")
sim3 <- read.csv("/Users/alyssaforber/Documents/Denver/Thesis/Results/Sim3_20180120.csv")
sim5 <- read.csv("/Users/alyssaforber/Documents/Denver/Thesis/Results/Sim5_20180120.csv")
sim10 <- read.csv("/Users/alyssaforber/Documents/Denver/Thesis/Results/Sim10_20180122.csv")
sim25 <- read.csv("/Users/alyssaforber/Documents/Denver/Thesis/Results/Sim25_20180122.csv")
sim50 <- read.csv("/Users/alyssaforber/Documents/Denver/Thesis/Results/Sim50_20180123.csv")

# remove last row of each with the outcome percent
sim1<- sim1[1:8,]
sim3<- sim3[1:8,]
sim5<- sim5[1:8,]
sim10<- sim10[1:8,]
sim25<- sim25[1:8,]
sim50<- sim50[1:8,]

# separate the type column into two columsn (one with youden or 0.5, and one with sampling type)
library(stringr)
sim1 <- cbind(sim1, type = str_split_fixed(sim1$X, " ", 2))
sim3 <- cbind(sim3, type = str_split_fixed(sim3$X, " ", 2))
sim5 <- cbind(sim5, type = str_split_fixed(sim5$X, " ", 2))
sim10 <- cbind(sim10, type = str_split_fixed(sim10$X, " ", 2))
sim25 <- cbind(sim25, type = str_split_fixed(sim25$X, " ", 2))
sim50 <- cbind(sim50, type = str_split_fixed(sim50$X, " ", 2))

# makes sure specificity is treaded as a numeric column
sim1$Specificity <- as.numeric(as.character(sim1$Specificity))
sim3$Specificity <- as.numeric(as.character(sim3$Specificity))
sim5$Specificity <- as.numeric(as.character(sim5$Specificity))
sim10$Specificity <- as.numeric(as.character(sim10$Specificity))
sim25$Specificity <- as.numeric(as.character(sim25$Specificity))
sim50$Specificity <- as.numeric(as.character(sim50$Specificity))

# could look at just full youden and 0.5 for each sim and so on
# or could look at just each sim in total 

# JUST FOR SIMULATION 1, ALL THE COMPONENTS
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

