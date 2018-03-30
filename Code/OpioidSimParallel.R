#-----------------------------
### COT THESIS
### AUTHOR: ALYSSA FORBER
### DATE: JANUARY 9, 2018
### DATE MODIFIED: JAN 10, 11, 12, MARCH
#-----------------------------
# RUN SIM PARALLEL

rm(list=ls())


library(doParallel)
library(foreach)


opd = read.csv('/Users/alyssaforber/Box Sync/AlyssaKatieResearch/Opioids/Data/ropdata5_red.csv')

# USE ALL 35 VARIALBES NOW
#opd = opd[c("Op_Chronic" , "age" , "OP_Receipt" , "ChronicPDcDx" ,"visit_year")]
opd = opd[c("age" , "agesq" , "genderm" , "racehisp" , "raceaa" , "Op_Chronic" ,
            "inscat5" , "los_ln" , "mmecat5" , "prior12" , "OP_Receipt" , "PriorOp5" ,
            "NOP_past" , "Benzo_past" , "SUHxDx_alch" , "SUHxDx_Stml" ,
            "SUHxDx_tabc" , "MHHxDx" , "ChronicPHxDx" , "AcutePHxDx" ,
            "ChronicPDcDx" , "AcutePDcDx" , "Surg_any" , "CHARLSON" , 
            "NeoplasmDcDx" , "NeoplasmHxDx" , "post_index_hospitalizations" ,         
            "char_surg" , "oprec_surg" , "chpdc_surg" , "chphx_surg" , "visit_year")]


# splitting 08-11 and 12-14 to make 2/3 split
train <- subset(opd, visit_year < 2012)
train$visit_year <- NULL

# ADDED MORE VARIABLES TO THIS
#glm <- glm(Op_Chronic ~ age + OP_Receipt + ChronicPDcDx + 
#             post_index_hospitalizations + ChronicPHxDx + CHARLSON + 
#             oprec_surg + SUHxDx_tabc + NeoplasmDcDx + NOP_past, 
#           data = train, family = "binomial")
#summary(glm)


b.int = -1.3
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


#ysim <- vector()

# Have to run this or it won't run in parallel
#getDoParWorkers()
#options(cores=2)
getDoParWorkers()
#registerDoMC(2)
cl<-makeCluster(8)
registerDoParallel(cl)

start <- Sys.time()
myresults <- foreach(i=1:niterations) %dopar% {
  
  library(DMwR)
  library(caret)
  library(glmnet)
  library(pROC)
  library(doMC)

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
  #ysim <- mean(all.opd$Op_Chronic_Sim)
  
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
  # NOW USING ALL THE VARS IN THE MODELS 
  newtest <- model.matrix(full_test$Op_Chronic_Sim ~ ., 
                          data = full_test)
  
  #----------------
  ## ORIGINAL DATA
  #----------------
  # NOW USING ALL VARS
  newtrain <- model.matrix(Op_Chronic_Sim ~ ., data=full_train) 
  
  # run cv.glmnet with matrix
  cvlasso <- cv.glmnet(newtrain, y = as.factor(full_train$Op_Chronic_Sim), family = "binomial")
  
  
  # GET NUMBER OF COVARIATES CHOSEN BY MODEL 
  coefs <- length(coef(cvlasso)@x) - 1 #subtracting the intercept

  
  # predict with matrix
  predict_lass <- predict(cvlasso, newtest, type = "response", s = "lambda.min")
  
  ###### pROC PACKAGE
  roc_lass <- roc(full_test$Op_Chronic_Sim, as.numeric(predict_lass))
  
  #### calculate with youden and save
  results <- coords(roc_lass, x = "best", best.method = "youden", 
                    ret = c("specificity", "sensitivity", "accuracy", "threshold"))
  
  # SAVE THE OUTPUT 
  Output <- cbind(as.data.frame.list(results), roc_lass$auc, coefs)
  names(Output)[5] <- "AUC"
  
  
  #-------------
  # DOWN SAMPLE
  #-------------
  
  # run model.matrix for train data
  #newtrain <- model.matrix(Class ~ age + OP_Receipt + ChronicPDcDx, data=down_train)
  # ALL VARS NOW
  newtrain <- model.matrix(Class ~ ., data=down_train)
  
  # run cv.glmnet with matrix
  cvlasso <- cv.glmnet(newtrain, y = as.factor(down_train$Class), family = "binomial")
  
  
  # GET NUMBER OF COVARIATES CHOSEN BY MODEL 
  coefs <- length(coef(cvlasso)@x) - 1 #subtracting the intercept
  
  
  # predict with matrix
  predict_down <- predict(cvlasso, newtest, type = "response", s = "lambda.min")
  
  ###### pROC PACKAGE
  roc_down <- roc(full_test$Op_Chronic_Sim, as.numeric(predict_down))
  
  #### calculate with youden
  results <- coords(roc_down, x = "best", best.method = "youden", 
                    ret = c("specificity", "sensitivity", "accuracy", "threshold"))
  
  Output2 <- cbind(as.data.frame.list(results), roc_down$auc, coefs)
  names(Output2)[5] <- "AUC"
  Output <- rbind(Output, Output2)
  
  #--------------------
  # UP SAMPLE
  #--------------------
  
  # run model.matrix for train data
  #newtrain <- model.matrix(Class~ age + OP_Receipt + ChronicPDcDx, data=up_train)
  # ALL VARS
  newtrain <- model.matrix(Class~ ., data=up_train)
  
  # run cv.glmnet with matrix
  cvlasso <- cv.glmnet(newtrain, y = as.factor(up_train$Class), family = "binomial")
  
  
  # GET NUMBER OF COVARIATES CHOSEN BY MODEL 
  coefs <- length(coef(cvlasso)@x) - 1 #subtracting the intercept
  
  
  # predict with matrix
  predict_up <- predict(cvlasso, newtest, type = "response", s = "lambda.min")
  
  ###### pROC PACKAGE
  roc_up <- roc(full_test$Op_Chronic_Sim, as.numeric(predict_up))
  
  #### calculate with youden
  results <- coords(roc_up, x = "best", best.method = "youden", 
                    ret = c("specificity", "sensitivity", "accuracy", "threshold"))
  
  Output2 <- cbind(as.data.frame.list(results), roc_up$auc, coefs)
  names(Output2)[5] <- "AUC"
  Output <- rbind(Output, Output2)
  
  
  #--------------------
  # SMOTE SAMPLE
  #--------------------
  
  # run model.matrix for train data
  #newtrain <- model.matrix(Op_Chronic_Sim ~ age + OP_Receipt + ChronicPDcDx, data=smote_train)
  # ALL VARS
  newtrain <- model.matrix(Op_Chronic_Sim ~ ., data=smote_train)
  
  # run cv.glmnet with matrix
  cvlasso <- cv.glmnet(newtrain, y = as.factor(smote_train$Op_Chronic_Sim), family = "binomial")
  
  
  # GET NUMBER OF COVARIATES CHOSEN BY MODEL 
  coefs <- length(coef(cvlasso)@x) - 1 #subtracting the intercept

  
  # predict with matrix
  predict_smote <- predict(cvlasso, newtest, type = "response", s = "lambda.min")
  
  ###### pROC PACKAGE
  roc_smote <- roc(full_test$Op_Chronic_Sim, as.numeric(predict_smote))
  
  #### calculate with youden 
  results <- coords(roc_smote, x = "best", best.method = "youden", 
                    ret = c("specificity", "sensitivity", "accuracy", "threshold"))
  
  Output2 <- cbind(as.data.frame.list(results), roc_smote$auc, coefs)
  names(Output2)[5] <- "AUC"
  Output <- rbind(Output, Output2)
  
  Output
  
  
}

# LOOK AT RESULTS

#mean(ysim)
end <- Sys.time()
end-start

#   5%
# 0.05847681 FOR INT -4.562189 (this is the actual intercept for our 5%)
# 0.05795344 for int -4.5622
# 0.05585634 for int -4.6
# 0.05200866 for int -4.7
# 0.04774589 for int -4.8
# 0.05016784 for int 14.73 ** 5%
#   3%
# 0.02971305 for int -5.34
# 0.03013175 for int -5.335 ** 3%
# 0.02984299 for int -5.336 ** or 3%
# 0.02963725 for int -5.3355
#  10%
# 0.1023281 for int -3.86
# 0.1020754 for int -3.88
# 0.09953438 for int -3.9 ** 10%
#  50%
# 0.5488612 for int -1 TOOK 35 MINUTES TO RUN 10 ITERATIONS (MEANS 2.5 DAYS FOR 1000!)
# 0.4890742 for int -1.3 TOOK 40 MINUTES 


total_results <- rbind(colMeans(fullY), c(colMeans(full5), .5), colMeans(downY), c(colMeans(down5), .5),
                       colMeans(upY), c(colMeans(up5), .5), colMeans(smoteY), c(colMeans(smote5), .5))


# add AUC to the table
# this is actually only from the last round, not an average...
auc <- c(roc_lass$auc, roc_lass$auc, roc_down$auc, roc_down$auc, 
         roc_up$auc, roc_up$auc, roc_smote$auc, roc_smote$auc)
total_results <- cbind(total_results, auc)

colnames(total_results) <- c("Covariates", "Specificity", "Sensitivity", "Accuracy", "Threshold", "AUC")
rownames(total_results) <- c("Full Youden", "Full 0.5", "Down Youden", "Down 0.5",
                             "Up Youden", "Up 0.5", "SMOTE Youden", "SMOTE 0.5")

total_results <- rbind(total_results, c("percent", mean(ysim)*100, "", "", "", ""))

# check the sim percent and date before writing
#write.csv(total_results, "/Users/alyssaforber/Documents/Denver/Thesis/Results/Simulation4/Sim5_20180328_test.csv")


