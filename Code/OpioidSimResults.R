#-----------------------------
### COT THESIS
### AUTHOR: ALYSSA FORBER
### DATE: JANUARY 16, 2018
### DATE MODIFIED: 
#-----------------------------

rm(list=ls())

sim1 <- read.csv("/Users/alyssaforber/Documents/Denver/Thesis/Results/Sim1_20180116.csv")
sim3 <-
sim5 <-
sim10 <- 



plot(colMeans(fullY), pch=16, xaxt = "n", ylab="", xlab="", main = "Outcome = 1.0%")
lines(colMeans(downY), pch=16, col="blue", type="p")
lines(colMeans(upY), pch=16, col="red", type="p")
lines(colMeans(smoteY), pch=16, col="green", type="p")
axis(1, at=1:3, labels=c("Specificity", "Sensitivity", "AUC"))
