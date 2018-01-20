#-----------------------------
### COT THESIS
### AUTHOR: ALYSSA FORBER
### DATE: JANUARY 9, 2018
### DATE MODIFIED: JAN 10, 11, 12
#-----------------------------


rm(list=ls())

library(DMwR)
library(caret)
library(glmnet)
library(pROC)

# 1. Pick three or four most important vars to use:
# age (age), chronic pain at discharge (ChronicPDcDx), 
# past year number of opioid prescr/Number of opioid prescriptions filled in 
# year following hospital discharge (OP_Post_nm) -> THIS DOESN'T EXIST IN OUR DATASET,
# receipt of opioid at discharge (OP_Receipt)

# 2. Run logistic regression with them to get coefficients

opd = read.csv('/Users/alyssaforber/Box Sync/AlyssaKatieResearch/Opioids/Data/ropdata5_red.csv')

opd = opd[c("Op_Chronic" , "age" , "OP_Receipt" , "ChronicPDcDx" ,"visit_year")]

# splitting 08-11 and 12-14 to make 2/3 split
train <- subset(opd, visit_year < 2012)
train$visit_year <- NULL

glm <- glm(Op_Chronic ~ age + OP_Receipt + ChronicPDcDx, 
           data = train, family = "binomial")
summary(glm)


# COEFFICIENTS
b.int = -0.965  ## intercept
b.age =  0.007875  ## age
b.chronic = 0.789668 ## chronic pain
#b.num =  ## past year number of opioid
b.receipt = 1.232307 ## receipt of opioid at discharge

niterations <- 1000

# EMPTY MATRICES
fullY <- matrix(data=NA, nrow = niterations, ncol = 4)
full5 <- matrix(data=NA, nrow = niterations, ncol = 3)
upY <- matrix(data=NA, nrow = niterations, ncol = 4)
up5 <- matrix(data=NA, nrow = niterations, ncol = 3)
downY <- matrix(data=NA, nrow = niterations, ncol = 4)
down5 <- matrix(data=NA, nrow = niterations, ncol = 3)
smoteY <- matrix(data=NA, nrow = niterations, ncol = 4)
smote5 <- matrix(data=NA, nrow = niterations, ncol = 3)

ysim <- vector()

#-----------------------------------
# experimenting with parallel cores
#install.packages("parallel")
#library(parallel)

# Calculate the number of cores
#no_cores <- detectCores() - 1
# I have 2 physical cores, but 4 logical ones

# Initiate cluster
#cl <- makeCluster(no_cores)
#stopCluster(cl)
#-----------------------------------



Sys.time()
for (i in 1:niterations){
  
  #---------------------------
  # CREATE SIMULATED OUTCOMES
  #---------------------------
  all.opd <- as.data.frame(transform(opd, Op_Chronic_Sim=rbinom(27705, 1, 
                                                         plogis(b.int + b.age*opd$age + 
                                                                b.chronic*opd$ChronicPDcDx + 
                                                                #b.num*opd$PriorOp5 + 
                                                                b.receipt*opd$OP_Receipt))))
  # save the outcome percentage
  ysim[i] <- mean(all.opd$Op_Chronic_Sim)
  
  # split into train and test set
  full_train <- subset(all.opd, visit_year < 2012)
  full_test <- subset(all.opd, visit_year >= 2012)
  
  # remove visit_year
  full_train$visit_year <- NULL
  full_test$visit_year <- NULL
  
  #----------------------
  # SAMPLE DATA
  #----------------------
  
  #-------------
  # DOWN SAMPLE 
  #-------------

  #get data with only predictors
  predictors <- full_train
  predictors$Op_Chronic_Sim <- NULL
  full_train$Op_Chronic_Sim <- as.factor(full_train$Op_Chronic_Sim)
  
  down_train <- downSample(x = predictors,
                           y = full_train$Op_Chronic_Sim)

  #-----------
  # UP SAMPLE
  #-----------

  up_train <- upSample(x = predictors,
                       y = full_train$Op_Chronic_Sim)  

  #--------------
  # SMOTE SAMPLE
  #--------------

  smote_train <- SMOTE(Op_Chronic_Sim ~ ., data  = full_train) 
  
  #--------------------------
  # RUN MODELS
  #--------------------------
  
  # model.matrix for test data for all datasets
  # full test and new test are the same, needed for all, just different format
  newtest <- model.matrix(full_test$Op_Chronic_Sim ~ age + OP_Receipt + ChronicPDcDx, 
                          data = full_test)
  
  #----------------
  ## ORIGINAL DATA
  #----------------
  
  # run model.matrix for train data
  newtrain <- model.matrix(Op_Chronic_Sim ~ age + OP_Receipt + ChronicPDcDx, data=full_train) 
  
  # run cv.glmnet with matrix
  cvlasso <- cv.glmnet(newtrain, y = as.factor(full_train$Op_Chronic_Sim), family = "binomial")

  # predict with matrix
  predict_lass <- predict(cvlasso, newtest, type = "response", s = "lambda.min")
  
  ###### pROC PACKAGE
  roc_lass <- roc(full_test$Op_Chronic_Sim, as.numeric(predict_lass))
 
  #### calculate with youden and save
  results <- coords(roc_lass, x = "best", best.method = "youden", 
                        ret = c("specificity", "sensitivity", "accuracy", "threshold"))
  fullY[i,1] <- results[1]
  fullY[i,2] <- results[2]
  fullY[i,3] <- results[3]
  fullY[i,4] <- results[4]
  
  #### calculate with 0.5 cutoff and save
  results <- coords(roc_lass, x = 0.5, input = "threshold",
                         ret = c("specificity", "sensitivity", "accuracy"))
  full5[i,1] <- results[1]
  full5[i,2] <- results[2]
  full5[i,3] <- results[3]
  
  #-------------
  # DOWN SAMPLE
  #-------------
  
  # run model.matrix for train data
  newtrain <- model.matrix(Class ~ age + OP_Receipt + ChronicPDcDx, data=down_train)
  
  # run cv.glmnet with matrix
  cvlasso <- cv.glmnet(newtrain, y = as.factor(down_train$Class), family = "binomial")
  
  # predict with matrix
  predict_down <- predict(cvlasso, newtest, type = "response", s = "lambda.min")
  
  ###### pROC PACKAGE
  roc_down <- roc(full_test$Op_Chronic_Sim, as.numeric(predict_down))
  
  #### calculate with youden
  results <- coords(roc_down, x = "best", best.method = "youden", 
                        ret = c("specificity", "sensitivity", "accuracy", "threshold"))
  downY[i,1] <- results[1]
  downY[i,2] <- results[2]
  downY[i,3] <- results[3]
  downY[i,4] <- results[4]
  
  #### calculate with 0.5 cutoff
  results <- coords(roc_down, x = 0.5, input = "threshold",
                         ret = c("specificity", "sensitivity", "accuracy"))
  down5[i,1] <- results[1]
  down5[i,2] <- results[2]
  down5[i,3] <- results[3]
  
  #--------------------
  # UP SAMPLE
  #--------------------
  
  # run model.matrix for train data
  newtrain <- model.matrix(Class~ age + OP_Receipt + ChronicPDcDx, data=up_train)
  
  # run cv.glmnet with matrix
  cvlasso <- cv.glmnet(newtrain, y = as.factor(up_train$Class), family = "binomial")

  # predict with matrix
  predict_up <- predict(cvlasso, newtest, type = "response", s = "lambda.min")
  
  ###### pROC PACKAGE
  roc_up <- roc(full_test$Op_Chronic_Sim, as.numeric(predict_up))

  #### calculate with youden
  results <- coords(roc_up, x = "best", best.method = "youden", 
                      ret = c("specificity", "sensitivity", "accuracy", "threshold"))
  upY[i,1] <- results[1]
  upY[i,2] <- results[2]
  upY[i,3] <- results[3]
  upY[i,4] <- results[4]
  
  #### calculate with 0.5 cutoff
  results <- coords(roc_up, x = 0.5, input = "threshold",
                       ret = c("specificity", "sensitivity", "accuracy"))
  up5[i,1] <- results[1]
  up5[i,2] <- results[2]
  up5[i,3] <- results[3]
  
  
  #--------------------
  # SMOTE SAMPLE
  #--------------------
  
  # run model.matrix for train data
  newtrain <- model.matrix(Op_Chronic_Sim ~ age + OP_Receipt + ChronicPDcDx, data=smote_train)
  
  # run cv.glmnet with matrix
  cvlasso <- cv.glmnet(newtrain, y = as.factor(smote_train$Op_Chronic_Sim), family = "binomial")

  # predict with matrix
  predict_smote <- predict(cvlasso, newtest, type = "response", s = "lambda.min")
  
  ###### pROC PACKAGE
  roc_smote <- roc(full_test$Op_Chronic_Sim, as.numeric(predict_smote))
  
  #### calculate with youden 
  results <- coords(roc_smote, x = "best", best.method = "youden", 
                         ret = c("specificity", "sensitivity", "accuracy", "threshold"))
  smoteY[i,1] <- results[1]
  smoteY[i,2] <- results[2]
  smoteY[i,3] <- results[3]
  smoteY[i,4] <- results[4]
  
  #### calculate with 0.5 cutoff
  results <- coords(roc_smote, x = 0.5, input = "threshold",
                          ret = c("specificity", "sensitivity", "accuracy"))
  smote5[i,1] <- results[1]
  smote5[i,2] <- results[2]
  smote5[i,3] <- results[3]

}

# LOOK AT RESULTS

mean(ysim)
Sys.time()

# started 20:37:46
# ended 22:03:23

# started 20:47:36
# ended 22:26:07

# started 00:09:50
# ended 01:47:49

# started 02:14:30
# ended 04:18:23

# 25 % prevalance took 4 hours!!!
# started 17:30:20
# ended 21:38:14

# 50% prevalance took 11 hours!!!!!!!!!!!!
# started 2018-01-19 21:54:02
# ended 2018-01-20 09:02:37


# int= -3.970231, %= 0.05899296
# int= -4, %= 0.05730013
# int= -4.5, %= 0.03676232
# int= -4.2 , %= 0.04892258
# int= -4.15, %= 0.05007255
# int= -4.1, %= 0.05314203

# int= -4.7, %= 0.02981772

# int= -3, %= 0.1353366
# int= -3.2, %= 0.1147591
# int= -3.3, %= 0.1058437, 0.1058293, 0.1062083

# int= -5.82, %= 0.0100379

# int= -1, %= 0.4905468
# int= -0.97, %= 0.4966973
# int= -0.965, %= 0.499408

# int= -2.2, %= 0.2469157
# int= -2.17, %= 0.2527666
# int= -2.19, %= 0.2488973
# int= -2.18, %= 0.249695


total_results <- rbind(colMeans(fullY), colMeans(full5), colMeans(downY), colMeans(down5),
                       colMeans(upY), colMeans(up5), colMeans(smoteY), colMeans(smote5))
colnames(total_results) <- c("Specificity", "Sensitivity", "AUC")
rownames(total_results) <- c("Full Youden", "Full 0.5", "Down Youden", "Down 0.5",
                             "Up Youden", "Up 0.5", "SMOTE Youden", "SMOTE 0.5")
total_results <- rbind(total_results, c("percent", mean(ysim)*100, ""))

write.csv(total_results, "/Users/alyssaforber/Documents/Denver/Thesis/Results/Sim50_20180120.csv")


plot(colMeans(fullY), pch=16, xaxt = "n", ylab="", xlab="", main = "Outcome = 1.0%")
lines(colMeans(downY), pch=16, col="blue", type="p")
lines(colMeans(upY), pch=16, col="red", type="p")
lines(colMeans(smoteY), pch=16, col="green", type="p")
axis(1, at=1:3, labels=c("Specificity", "Sensitivity", "AUC"))
