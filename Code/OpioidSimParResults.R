#-----------------------------
### COT THESIS
### AUTHOR: ALYSSA FORBER
### DATE: MARCH 31
### DATE MODIFIED: 
#-----------------------------
# Putting together results for simulation after changed
# to include more variables and then run in parallel

# LOAD DATA
sim3 <- read.csv("/Users/alyssaforber/Documents/Denver/Thesis/Results/Simulation4/Sim3_20180402.csv")
sim5 <- read.csv("/Users/alyssaforber/Documents/Denver/Thesis/Results/Simulation4/Sim5_20180402.csv")
sim10 <- read.csv("/Users/alyssaforber/Documents/Denver/Thesis/Results/Simulation4/Sim10_20180403.csv")
sim20 <- read.csv("/Users/alyssaforber/Documents/Denver/Thesis/Results/Simulation4/Sim20_20180404.csv")
sim40 <- read.csv("/Users/alyssaforber/Documents/Denver/Thesis/Results/Simulation4/Sim40_20180405.csv")
sim50 <- read.csv("/Users/alyssaforber/Documents/Denver/Thesis/Results/Simulation4/Sim50_20180404.csv")

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

plot(sens5, pch=16, ylim=c(0,1), xlab="Prevalence", ylab="", 
     main="Sensitivity & Specificity vs Prevalence", col="blue")
lines(sensYoud, type="p", pch=16, col="blue")
lines(sensYoud, col="blue", lwd=2)
lines(sens5, col="blue", lwd=2)
lines(specYoud, col="red", lwd=2)
lines(specYoud, col="red", type="p", pch=16)
lines(spec5, col="red", lwd=2)
lines(spec5, col="red", type="p", pch=16)

legend(x=38, y=.27, legend=c("Youden", "0.5"), bty="n", col=c("red", "black"), 
       pch=16, ncol=1, cex=.9)








