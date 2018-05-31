#---------------------
# OPIOID FINAL CODE
# ALYSSA FORBER
# MAY 22, 2018
# MODIFIED 
#-------------------



rm(list=ls())

library(pROC)
library(glmnet)


opd = read.csv('/Users/alyssaforber/Box Sync/AlyssaKatieResearch/Opioids/Data/ropdata5_red.csv')

print(table(opd$Op_Chronic))

opd = opd[c("age" , "agesq" , "genderm" , "racehisp" , "raceaa" , "Op_Chronic" ,
            "raceoth" , "genm_racehisp" , "genm_raceaa" , "genm_raceoth" ,
            "inscat5" , "los_ln" , "mmecat5" , "prior12" , "OP_Receipt" , "PriorOp5" ,
            "NOP_past" , "Benzo_past" , "SUHxDx_alch" , "SUHxDx_Stml" ,
            "SUHxDx_tabc" , "MHHxDx" , "ChronicPHxDx" , "AcutePHxDx" ,
            "ChronicPDcDx" , "AcutePDcDx" , "Surg_any" , "CHARLSON" , 
            "NeoplasmDcDx" , "NeoplasmHxDx" , "post_index_hospitalizations" ,         
            "char_surg" , "oprec_surg" , "chpdc_surg" , "chphx_surg" , "visit_year")]


# splitting 08-11 and 12-14 to make 2/3 split (64.7% train, 35.3% test)
full_train <- subset(opd, visit_year < 2012)
full_test <- subset(opd, visit_year >= 2012)

# looking at outcome percents (0.059% outcome in train, 0.040% outcome in test)
prop.table(table(full_train$Op_Chronic))
prop.table(table(full_test$Op_Chronic))

# Only neede visit year to split data
full_train$visit_year <- NULL
full_test$visit_year <- NULL

prop.table(table(opd$Op_Chronic))
table(opd$Op_Chronic)


#--------------------------------------------------------
#### ORIGINAL, DOWN, UP, SMOTE SAMPLING FOR LASSO #####
#--------------------------------------------------------


#------------------
### DOWN SAMPLE ###
#------------------

set.seed(747)
#install.packages("caret")
library(caret)

#get data with only predictors
predictors <- full_train
predictors$Op_Chronic <- NULL
full_train$Op_Chronic <- as.factor(full_train$Op_Chronic)


down_train <- downSample(x = predictors,
                         y = full_train$Op_Chronic)

table(down_train$Class) 

#------------------
### UP SAMPLE ###
#------------------

set.seed(747)

up_train <- upSample(x = predictors,
                     y = full_train$Op_Chronic)  

table(up_train$Class) 


#--------------------
### SMOTE SAMPLE ###
#--------------------

## using default settings on SMOTE
#install.packages("DMwR")
library(DMwR)
set.seed(747)

full_train$Op_Chronic <- as.factor(full_train$Op_Chronic)

# let smote use indicators but make sure they are rounded at the end
full_train_smote <- full_train

cols <- c("genderm", "racehisp", "raceaa", "raceoth", "genm_racehisp", 
          "genm_raceaa", "chphx_surg", "genm_raceoth", "prior12", "OP_Receipt",
          "NOP_past", "Benzo_past", "SUHxDx_alch", "SUHxDx_Stml", "SUHxDx_tabc",
          "MHHxDx", "ChronicPHxDx", "AcutePHxDx", "ChronicPDcDx", "AcutePDcDx",
          "Surg_any", "NeoplasmDcDx", "NeoplasmHxDx", "oprec_surg", "chpdc_surg")

smote_train <- SMOTE(Op_Chronic ~ ., data  = full_train_smote)                         
table(smote_train$Op_Chronic)
# Round indicators
smote_train[,cols] <- round(smote_train[,cols]) 


#-------------------------------------------------
# RUN LASSO ON ALL SAMPLES
# ORIGINAL, DOWN, UP, SMOTE
#-------------------------------------------------

#--------------------
## ORIGINAL DATA
#--------------------

# run model.matrix for train data
newtrain <- model.matrix(Op_Chronic~.,data=full_train) 

# run cv.glmnet with matrix
cvlasso <- cv.glmnet(newtrain, y = as.factor(full_train$Op_Chronic), family = "binomial")
min <- cvlasso$lambda.min
coefs_lass <- coef(cvlasso, s= "lambda.min")

# model.matrix for test data
newtest <- model.matrix(full_test$Op_Chronic ~ . , data = full_test)

# predict with matrix
predict_lass <- predict(cvlasso, newtest, type = "response", s = "lambda.min")


hist(predict_lass, main = "Histogram of Unsampled Predicted Probabilites",
     xlab = "Predicted Probability")
confmat_lass <- as.data.frame(cbind(full_test$Op_Chronic, predict_lass))
colnames(confmat_lass) <- c("Op_Chronic", "Pred")


###### pROC PACKAGE
roc_lass <- roc(confmat_lass$Op_Chronic, confmat_lass$Pred)

#### calculate with youden 
coords_lass <- coords(roc_lass, x = "best", best.method = "youden", 
                      ret = c("threshold", "specificity", "sensitivity", 
                              "npv", "ppv", "accuracy"))

#### calculate with 0.5 cutoff
coords_lass5 <- coords(roc_lass, x = 0.5, input = "threshold",
                       ret = c("threshold", "specificity", "sensitivity", 
                               "npv", "ppv", "accuracy"))

#### calculate with prevalence
coords_lass5 <- coords(roc_lass, x = 0.05, input = "threshold",
                       ret = c("threshold", "specificity", "sensitivity", 
                               "npv", "ppv", "accuracy"))

### FIND THRESHOLD WITH TRAINING DATA AND USE
predict_lass <- predict(cvlasso, newtrain, type = "response", s = "lambda.min")
confmat_lass <- as.data.frame(cbind(full_train$Op_Chronic, predict_lass))
colnames(confmat_lass) <- c("Op_Chronic", "Pred")
roc_lass_train <- roc(confmat_lass$Op_Chronic, confmat_lass$Pred)
coords_lass_train <- coords(roc_lass_train, x = "best", best.method = "youden")
# save the threshold chosen in the training
train_thresh <- coords_lass_train[1]

# use this threshold in test data
coords_lass_train <- coords(roc_lass_train, x = train_thresh, input = "threshold",
                       ret = c("threshold", "specificity", "sensitivity", 
                               "npv", "ppv", "accuracy"))

#--------------------
## DOWN SAMPLE
#--------------------

# run model.matrix for train data
newtrain <- model.matrix(Class~.,data=down_train) 

# run cv.glmnet with matrix
cvlasso <- cv.glmnet(newtrain, y = as.factor(down_train$Class), family = "binomial")
min <- cvlasso$lambda.min
coefs_down <- coef(cvlasso, s= "lambda.min")


newtest <- model.matrix(full_test$Op_Chronic ~ . , data = full_test)

# predict with matrix
predict_down <- predict(cvlasso, newtest, type = "response", s = "lambda.min")



hist(predict_down, main = "Histogram of Down Sampled Predicted Probabilites",
     xlab = "Predicted Probability")
confmat_down <- as.data.frame(cbind(full_test$Op_Chronic, predict_down))
colnames(confmat_down) <- c("Op_Chronic", "Pred")


###### pROC PACKAGE
roc_down <- roc(confmat_down$Op_Chronic, confmat_down$Pred)
# Area under the curve: 


#### calculate with youden 
coords_down <- coords(roc_down, x = "best", best.method = "youden", 
                      ret = c("threshold", "specificity", "sensitivity", 
                              "npv", "ppv", "accuracy"))

#### calculate with prevalence
coords_down5 <- coords(roc_down, x = 0.5, input = "threshold", 
                      ret = c("threshold", "specificity", "sensitivity", 
                              "npv", "ppv", "accuracy"))


### FIND THRESHOLD WITH TRAINING DATA AND USE
predict_down <- predict(cvlasso, newtrain, type = "response", s = "lambda.min")
confmat_down <- as.data.frame(cbind(down_train$Class, predict_down))
colnames(confmat_down) <- c("Op_Chronic", "Pred")
roc_down_train <- roc(confmat_down$Op_Chronic, confmat_down$Pred)
coords_down_train <- coords(roc_down_train, x = "best", best.method = "youden")
# save the threshold chosen in the training
train_thresh <- coords_down_train[1]

# use this threshold in test data
coords_down_train <- coords(roc_down_train, x = train_thresh, input = "threshold",
                            ret = c("threshold", "specificity", "sensitivity", 
                                    "npv", "ppv", "accuracy"))


#--------------------
## UP SAMPLE
#--------------------

# run model.matrix for train data
newtrain <- model.matrix(Class~.,data=up_train) 

# run cv.glmnet with matrix
cvlasso <- cv.glmnet(newtrain, y = as.factor(up_train$Class), family = "binomial")
min <- cvlasso$lambda.min
coefs_up <- coef(cvlasso, s= "lambda.min")


newtest <- model.matrix(full_test$Op_Chronic ~ . , data = full_test)

# predict with matrix of testing data
predict_up <- predict(cvlasso, newtest, type = "response", s = "lambda.min")


hist(predict_up)
confmat_up <- as.data.frame(cbind(full_test$Op_Chronic, predict_up))
colnames(confmat_up) <- c("Op_Chronic", "Pred")


###### pROC PACKAGE
roc_up <- roc(confmat_up$Op_Chronic, confmat_up$Pred)
# Area under the curve: 


#### calculate with youden 
coords_up <- coords(roc_up, x = "best", best.method = "youden", 
                    ret = c("threshold", "specificity", "sensitivity", 
                            "npv", "ppv", "accuracy"))

#### calculate with prevalence 
coords_up5 <- coords(roc_up, x = 0.5, input = "threshold", 
                    ret = c("threshold", "specificity", "sensitivity", 
                            "npv", "ppv", "accuracy"))


### FIND THRESHOLD WITH TRAINING DATA AND USE
predict_up <- predict(cvlasso, newtrain, type = "response", s = "lambda.min")
confmat_up <- as.data.frame(cbind(up_train$Class, predict_up))
colnames(confmat_up) <- c("Op_Chronic", "Pred")
roc_up_train <- roc(confmat_up$Op_Chronic, confmat_up$Pred)
coords_up_train <- coords(roc_up_train, x = "best", best.method = "youden")
# save the threshold chosen in the training
train_thresh <- coords_up_train[1]

# use this threshold in test data
coords_up_train <- coords(roc_up_train, x = train_thresh, input = "threshold",
                            ret = c("threshold", "specificity", "sensitivity", 
                                    "npv", "ppv", "accuracy"))


#--------------------
## SMOTE 
#--------------------

# run model.matrix for train data
newtrain <- model.matrix(Op_Chronic~.,data=smote_train) 

# run cv.glmnet with matrix
cvlasso <- cv.glmnet(newtrain, y = as.factor(smote_train$Op_Chronic), family = "binomial")
min <- cvlasso$lambda.min
coefs_smote <- coef(cvlasso, s= "lambda.min")


newtest <- model.matrix(full_test$Op_Chronic ~ . , data = full_test)

# predict with matrix
predict_smote <- predict(cvlasso, newtest, type = "response", s = "lambda.min")
hist(predict_smote)
confmat_smote <- as.data.frame(cbind(full_test$Op_Chronic, predict_smote))
colnames(confmat_smote) <- c("Op_Chronic", "Pred")


###### pROC PACKAGE
roc_smote <- roc(confmat_smote$Op_Chronic, confmat_smote$Pred)
# Area under the curve: 


#### calculate with youden
coords_smote <- coords(roc_smote, x = "best", best.method = "youden", 
                        ret = c("threshold", "specificity", "sensitivity", 
                                "npv", "ppv", "accuracy"))

#### calculate with prevalence
coords_smote5 <- coords(roc_smote, x = 0.5, input = "threshold", 
                       ret = c("threshold", "specificity", "sensitivity", 
                               "npv", "ppv", "accuracy"))


### FIND THRESHOLD WITH TRAINING DATA AND USE
predict_smote <- predict(cvlasso, newtrain, type = "response", s = "lambda.min")
confmat_smote <- as.data.frame(cbind(smote_train$Op_Chronic, predict_smote))
colnames(confmat_smote) <- c("Op_Chronic", "Pred")
roc_smote_train <- roc(confmat_smote$Op_Chronic, confmat_smote$Pred)
coords_smote_train <- coords(roc_smote_train, x = "best", best.method = "youden")
# save the threshold chosen in the training
train_thresh <- coords_smote_train[1]

# use this threshold in test data
coords_smote_train <- coords(roc_smote_train, x = train_thresh, input = "threshold",
                          ret = c("threshold", "specificity", "sensitivity", 
                                  "npv", "ppv", "accuracy"))


#-----------------------------------
# RESULTS and PLOTS
#-----------------------------------

# Table for youden index and sampling combined
rocTable <- round(rbind(coords_lass5, coords_lass, coords_lass_train, coords_lass5,
                        coords_down, coords_down_train, coords_down5, 
                        coords_up, coords_up_train, coords_up5,
                        coords_smote, coords_smote_train, coords_smote5), digits=2)

# Add AUC
auc <- c(roc_lass$auc, roc_lass$auc, roc_lass_train$auc, roc_lass$auc, 
         roc_down$auc, roc_down_train$auc, roc_down$auc,
         roc_up$auc, roc_up_train$auc, roc_up$auc,
         roc_smote$auc, roc_smote_train$auc, roc_smote$auc)

# Add # variables from coeffs
vars <- c(34, 34, 34, 34, 30, 30, 30, 34, 34, 34, 34, 34, 34)
rocTable <- cbind(rocTable, auc, vars)
rocTable[,7] <- round(rocTable[,7], digits=2)
rocTable[,2:7] <- rocTable[,2:7]*100
rocTable <- cbind(rocTable[,1], rocTable[,3], rocTable[,2], rocTable[,c(4:8)])
colnames(rocTable) <- c("Threshold", "Sensitivity", "Specificity", "NPV", "PPV", 
                        "Accuracy", "AUC", "Number of Covariates Selected")
rownames(rocTable) <- c("Full Training 0.5", "Full Training", "Full Training*", "Full Training 0.05",
                        "Under-Sampled", "Under-Sampled*", "Under-Sampled 0.5",
                        "Over-Sampled", "Over-Sampled*", "Over-Sampled 0.5",
                        "SMOTE", "SMOTE*", "SMOTE 0.5")

write.csv(rocTable, '/Users/alyssaforber/Documents/Denver/Thesis/Results/LassRocTable2018524.csv')

