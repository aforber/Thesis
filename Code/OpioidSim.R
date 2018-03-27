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

# USE ALL 35 VARIALBES NOW
#opd = opd[c("Op_Chronic" , "age" , "OP_Receipt" , "ChronicPDcDx" ,"visit_year")]
opd = opd[c("age" , "agesq" , "genderm" , "racehisp" , "raceaa" , "Op_Chronic" ,
            "raceoth" , "genm_racehisp" , "genm_raceaa" , "genm_raceoth" ,
            "inscat5" , "los_ln" , "mmecat5" , "prior12" , "OP_Receipt" , "PriorOp5" ,
            "NOP_past" , "Benzo_past" , "SUHxDx_alch" , "SUHxDx_Stml" ,
            "SUHxDx_tabc" , "MHHxDx" , "ChronicPHxDx" , "AcutePHxDx" ,
            "ChronicPDcDx" , "AcutePDcDx" , "Surg_any" , "CHARLSON" , 
            "NeoplasmDcDx" , "NeoplasmHxDx" , "post_index_hospitalizations" ,         
            "char_surg" , "oprec_surg" , "chpdc_surg" , "chphx_surg" , "visit_year")]


# splitting 08-11 and 12-14 to make 2/3 split
train <- subset(opd, visit_year < 2012)
train$visit_year <- NULL

# ADD MORE VARIABLES TO THIS
glm <- glm(Op_Chronic ~ age + OP_Receipt + ChronicPDcDx + 
             post_index_hospitalizations + ChronicPHxDx + CHARLSON + 
             oprec_surg + SUHxDx_tabc + NeoplasmDcDx + NOP_past, 
           data = train, family = "binomial")
summary(glm)

#----------------------
# OLD INFO FOR 3 VARS
#----------------------
# COEFFICIENTS

# age 0.007875
# chronic pain 0.789668
# receipt of opioid 1.232307

# INTERCEPTS FOR PERCENTAGES
# int= -5.82, %= 0.0100379
# int= -4.7, %= 0.02981772
# int= -4.15, %= 0.05007255
# int= -3.6, %= 0.1003321 
# int= -3.36, %= 0.1008085 **use this one
# int= -2.18, %= 0.249695
# int= -0.965, %= 0.499408

#b.int = -3.36  ## intercept
#b.age =  0.007875 ## age
#b.chronicD = 0.789668 ## chronic pain
#b.receipt = 1.232307 ## receipt of opioid at discharge


#----------------------
# NEW INFO W/MORE VARS
#----------------------

# NEW COEFFICIENTS WITH 10 VARS
# age                         -0.002113    
# OP_Receipt                   1.514161 
# ChronicPDcDx                 0.615101
# post_index_hospitalizations  0.301034  
# ChronicPHxDx                 0.496509 
# CHARLSON                     0.038409 
# oprec_surg                  -0.487705 
# SUHxDx_tabc                  0.421039 
# NeoplasmDcDx                 0.539857   
# NOP_past                     0.993827 

# INTERCEPTS
# -4.562189 for 5%

b.int = -4.562189 # for 5%
b.age = -0.002113
b.receipt = 1.514161
b.chronicD = 0.615101
b.post = 0.301034
b.chronicH = 0.496509
b.charlson = 0.038409
b.surg = -0.487705
b.SUH = 0.421039
b.neo = 0.539857
b.NOP = 0.993827

niterations <- 10

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

start <- Sys.time()
for (i in 1:niterations){
  
  #---------------------------
  # CREATE SIMULATED OUTCOMES
  #---------------------------
  all.opd <- as.data.frame(transform(opd, Op_Chronic_Sim=rbinom(27705, 1, 
                                                         plogis(b.int + b.age*opd$age + 
                                                                  b.receipt*opd$OP_Receipt +
                                                                  b.chronicD*opd$ChronicPDcDx +
                                                                  b.post*post_index_hospitalizations +
                                                                  b.chronicH*ChronicPHxDx+
                                                                  b.charlson*CHARLSON +
                                                                  b.surg*oprec_surg +
                                                                  b.SUH*SUHxDx_tabc +
                                                                  b.neo*NeoplasmDcDx +
                                                                  b.NOP*NOP_past))))
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
end <- Sys.time()
end-start

# original coefficients
# 1% was 1.44 hours
# 3% was 1.6 hours
# 5% was 1.6 hours
# 10% was 2.1 hours
# 25% was 4.1 hours
# 50% was 10.2 hours

# 4*coefficients
# 3% was 2.4 hours
# 5% was 2.6 hours
# 10% was 3.05 hours
# 50% was 13.9 hours


# INTERCEPTS FOR PERCENTAGES
# int= -5.82, %= 0.0100379
# int= -4.7, %= 0.02981772
# int= -4.15, %= 0.05007255
# int= -3.6, %= 0.1003321
# int= -2.18, %= 0.249695
# int= -0.965, %= 0.499408

# INTERCEPTS FOR PERCENTAGES COEFFS * 4!
# int= -10.68, %= 0.02997654
# int= -9.91, %= 0.04997654
# int= -8.52, %= 0.1000108
# int= -3.51, %=  0.4995957



total_results <- rbind(colMeans(fullY), c(colMeans(full5), .5), colMeans(downY), c(colMeans(down5), .5),
                       colMeans(upY), c(colMeans(up5), .5), colMeans(smoteY), c(colMeans(smote5), .5))


# add AUC to the table
auc <- c(roc_lass$auc, roc_lass$auc, roc_down$auc, roc_down$auc, 
         roc_up$auc, roc_up$auc, roc_smote$auc, roc_smote$auc)
total_results <- cbind(total_results, auc)

colnames(total_results) <- c("Specificity", "Sensitivity", "Accuracy", "Threshold", "AUC")
rownames(total_results) <- c("Full Youden", "Full 0.5", "Down Youden", "Down 0.5",
                             "Up Youden", "Up 0.5", "SMOTE Youden", "SMOTE 0.5")

total_results <- rbind(total_results, c("percent", mean(ysim)*100, "", "", ""))

# check the sim percent before writing
#write.csv(total_results, "/Users/alyssaforber/Documents/Denver/Thesis/Results/Simulation3/Sim10_20180326.csv")


plot(colMeans(fullY), pch=16, xaxt = "n", ylab="", xlab="", main = "Outcome = 1.0%")
lines(colMeans(downY), pch=16, col="blue", type="p")
lines(colMeans(upY), pch=16, col="red", type="p")
lines(colMeans(smoteY), pch=16, col="green", type="p")
axis(1, at=1:3, labels=c("Specificity", "Sensitivity", "AUC"))
