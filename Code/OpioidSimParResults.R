#-----------------------------
### COT THESIS
### AUTHOR: ALYSSA FORBER
### DATE: MARCH 31
### DATE MODIFIED: 
#-----------------------------
# Putting together results for simulation after changed
# to include more variables and then run in parallel

rm(list=ls())

# LOAD DATA
sim3 <- read.csv("/Users/alyssaforber/Documents/Denver/Thesis/Results/Simulation5/Sim3_Mean_20180502.csv")
sim5 <- read.csv("/Users/alyssaforber/Documents/Denver/Thesis/Results/Simulation5/Sim5_Mean_20180502.csv")
sim10 <- read.csv("/Users/alyssaforber/Documents/Denver/Thesis/Results/Simulation5/Sim10_Mean_20180503.csv")
sim20 <- read.csv("/Users/alyssaforber/Documents/Denver/Thesis/Results/Simulation5/Sim20_Mean_20180503.csv")
sim40 <- read.csv("/Users/alyssaforber/Documents/Denver/Thesis/Results/Simulation5/Sim40_Mean_20180506.csv")
sim50 <- read.csv("/Users/alyssaforber/Documents/Denver/Thesis/Results/Simulation5/Sim50_Mean_20180507.csv")

sim3_med <- read.csv("/Users/alyssaforber/Documents/Denver/Thesis/Results/Simulation5/Sim3_Median_20180502.csv")
sim5_med <- read.csv("/Users/alyssaforber/Documents/Denver/Thesis/Results/Simulation5/Sim5_Median_20180502.csv")
sim10_med <- read.csv("/Users/alyssaforber/Documents/Denver/Thesis/Results/Simulation5/Sim10_Median_20180503.csv")
sim20_med <- read.csv("/Users/alyssaforber/Documents/Denver/Thesis/Results/Simulation5/Sim20_Median_20180503.csv")
sim40_med <- read.csv("/Users/alyssaforber/Documents/Denver/Thesis/Results/Simulation5/Sim40_Median_20180506.csv")
sim50_med <- read.csv("/Users/alyssaforber/Documents/Denver/Thesis/Results/Simulation5/Sim50_Median_20180507.csv")

sim3_quan <- read.csv("/Users/alyssaforber/Documents/Denver/Thesis/Results/Simulation5/Sim3_Quantile_20180502.csv")
sim5_quan  <- read.csv("/Users/alyssaforber/Documents/Denver/Thesis/Results/Simulation5/Sim5_Quantile_20180502.csv")
sim10_quan  <- read.csv("/Users/alyssaforber/Documents/Denver/Thesis/Results/Simulation5/Sim10_Quantile_20180503.csv")
sim20_quan  <- read.csv("/Users/alyssaforber/Documents/Denver/Thesis/Results/Simulation5/Sim20_Quantile_20180503.csv")
sim40_quan  <- read.csv("/Users/alyssaforber/Documents/Denver/Thesis/Results/Simulation5/Sim40_Quantile_20180506.csv")
sim50_quan  <- read.csv("/Users/alyssaforber/Documents/Denver/Thesis/Results/Simulation5/Sim50_Quantile_20180507.csv")


# Make plot of threshold vs prevalence

thresh <- cbind(c(3, 5, 10, 20, 40, 50), c(sim3[1,5], sim5[1,5], sim10[1,5], 
                                           sim20[1,5], sim40[1,5], sim50[1,5]))
plot(thresh, pch=16, xlab = "Prevalence %", ylab ="Youden Threshold", main="Threshold vs Prevalence")
lines(thresh, lwd=2)

# Make plot of sensitiviy and specificity along prevalence for unsampled youden & .5

sensYoud <- cbind(c(3, 5, 10, 20, 40, 50), c(sim3[1,3], sim5[1,3], sim10[1,3], 
                                             sim20[1,3], sim40[1,3], sim50[1,3]))
sens5 <- cbind(c(3, 5, 10, 20, 40, 50), c(sim3[2,3], sim5[2,3], sim10[2,3], 
                                          sim20[2,3], sim40[2,3], sim50[2,3]))

specYoud <- cbind(c(3, 5, 10, 20, 40, 50), c(sim3[1,2], sim5[1,2], sim10[1,2], 
                                             sim20[1,2], sim40[1,2], sim50[1,2]))
spec5 <- cbind(c(3, 5, 10, 20, 40, 50), c(sim3[2,2], sim5[2,2], sim10[2,2], 
                                          sim20[2,2], sim40[2,2], sim50[2,2]))

plot(sens5, pch=20, ylim=c(0,1), xlab="Prevalence", ylab="", 
     main="Sensitivity & Specificity vs Prevalence", col="blue")
lines(sensYoud, type="p", pch=20, col="blue")
lines(sensYoud, col="blue", lwd=2, lty=2)
lines(sens5, col="blue", lwd=2)
lines(specYoud, col="red", lwd=2, lty=2)
lines(specYoud, col="red", type="p", pch=20)
lines(spec5, col="red", lwd=2)
lines(spec5, col="red", type="p", pch=20)
#x=35, y=.3
legend("bottomright", legend=c("Specificity", "Sensitiviy", "0.5", "Youden"), 
       bty="n", col=c("red", "blue", "black", "black"), pch=c(20,20, 45, 45), 
       ncol=1, lty=c(0,0,1,2), lwd=c(1,1,2,2))
legend("bottomright", legend=c("",""), bty="n", cex=1)


#### clean tables
library(data.table)
myformat <- function(sim){
  sim <- sim[,1:7]
  sim[,c(2:4,6)] <- round(sim[,c(2:4,6)]*100)
  sim[,5] <- round(sim[,5], digits=2)
  sim[,7] <- round(sim[,7])
  sim <- setcolorder(sim, c(1,5,3,2,4,6,7))
  # flip to put 0.5 first
  sim <- rbind(sim[2,], sim[1,], sim[3:5,])
  sim
}

sim3 <- myformat(sim3)
sim5 <- myformat(sim5)
sim10 <- myformat(sim10)
sim20 <- myformat(sim20)
sim40 <- myformat(sim40)
sim50 <- myformat(sim50)

sim3_med <- myformat(sim3_med)
sim5_med <- myformat(sim5_med)
sim10_med <- myformat(sim10_med)
sim20_med <- myformat(sim20_med)
sim40_med <- myformat(sim40_med)
sim50_med <- myformat(sim50_med)

# quantile formatting
quantformat <- function(sim){
  sim <- cbind(sim[,2:7], sim[,9:14])
  sim <- rbind(sim[2,], sim[1,], sim[3:5,])
  sim[,c(1:5,7:11)] <- round(sim[,c(1:5,7:11)]*100)
}

sim3_quan <- quantformat(sim3_quan)
sim5_quan <- quantformat(sim5_quan)
sim10_quan <- quantformat(sim10_quan)
sim20_quan <- quantformat(sim20_quan)
sim40_quan <- quantformat(sim40_quan)
sim50_quan <- quantformat(sim50_quan)



