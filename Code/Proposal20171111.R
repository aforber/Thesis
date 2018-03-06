#---------------------
# OPIOID SAMPLING
# ALYSSA FORBER
# NOVEMBER 11 2017
# MODIFIED NOV 14, 30 2017
#-------------------
# Got lasso prediction to work in 1103.R and 
# now to clean it up and do for all samples

rm(list=ls())


#install.packages("pROC")
library(pROC)


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
#defaults are 200 up and 200 down
smote_train <- SMOTE(Op_Chronic ~ ., data  = full_train)                         
table(smote_train$Op_Chronic)



#-------------------------------------------------
# RUN LASSO ON ALL SAMPLES
# ORIGINAL, DOWN, UP, SMOTE
#-------------------------------------------------


#install.packages("glmnet")
library(glmnet)


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
# Area under the curve: 


#### calculate with youden (cutoff is 0.04)
coords_lass <- coords(roc_lass, x = "best", best.method = "youden", 
                      ret = c("threshold", "specificity", "sensitivity", 
                              "npv", "ppv", "accuracy"))

#### calculate with 0.5 cutoff
coords_lass5 <- coords(roc_lass, x = 0.5, input = "threshold",
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

# model.matrix for test data
# Right now, using the full test data
# Once we impliment bagging, then we will
# have a subset with just the vars used
# in the chosen model
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


#### calculate with youden (cutoff is 0.4)
coords_down <- coords(roc_down, x = "best", best.method = "youden", 
                      ret = c("threshold", "specificity", "sensitivity", 
                              "npv", "ppv", "accuracy"))

#### calculate with 0.5 cutoff
coords_down5 <- coords(roc_down, x = 0.5, input = "threshold",
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

# model.matrix for test data
# Right now, using the full test data
# Once we impliment bagging, then we will
# have a subset with just the vars used
# in the chosen model
newtest <- model.matrix(full_test$Op_Chronic ~ . , data = full_test)

# predict with matrix
predict_up <- predict(cvlasso, newtest, type = "response", s = "lambda.min")



hist(predict_up)
confmat_up <- as.data.frame(cbind(full_test$Op_Chronic, predict_up))
colnames(confmat_up) <- c("Op_Chronic", "Pred")


###### pROC PACKAGE
roc_up <- roc(confmat_up$Op_Chronic, confmat_up$Pred)
# Area under the curve: 


#### calculate with youden (cutoff is ~0.4)
coords_up <- coords(roc_up, x = "best", best.method = "youden", 
                    ret = c("threshold", "specificity", "sensitivity", 
                            "npv", "ppv", "accuracy"))

#### calculate with 0.5 cutoff
coords_up5 <- coords(roc_up, x = 0.5, input = "threshold",
                       ret = c("threshold", "specificity", "sensitivity", 
                               "npv", "ppv", "accuracy"))


#--------------------
## SMOTE SAMPLE
#--------------------

# run model.matrix for train data
newtrain <- model.matrix(Op_Chronic~.,data=smote_train) 

# run cv.glmnet with matrix
cvlasso <- cv.glmnet(newtrain, y = as.factor(smote_train$Op_Chronic), family = "binomial")
min <- cvlasso$lambda.min
coefs_smote <- coef(cvlasso, s= "lambda.min")


# model.matrix for test data
# Right now, using the full test data
# Once we impliment bagging, then we will
# have a subset with just the vars used
# in the chosen model
newtest <- model.matrix(full_test$Op_Chronic ~ . , data = full_test)

# predict with matrix
predict_smote <- predict(cvlasso, newtest, type = "response", s = "lambda.min")



hist(predict_smote)
confmat_smote <- as.data.frame(cbind(full_test$Op_Chronic, predict_smote))
colnames(confmat_smote) <- c("Op_Chronic", "Pred")


###### pROC PACKAGE
roc_smote <- roc(confmat_smote$Op_Chronic, confmat_smote$Pred)
# Area under the curve: 


#### calculate with youden (cutoff is 0.47)
coords_smote <- coords(roc_smote, x = "best", best.method = "youden", 
                       ret = c("threshold", "specificity", "sensitivity", 
                                "npv", "ppv", "accuracy"))

#### calculate with 0.5 cutoff
coords_smote5 <- coords(roc_smote, x = 0.5, input = "threshold",
                       ret = c("threshold", "specificity", "sensitivity", 
                               "npv", "ppv", "accuracy"))


#-----------------------------------
# RESULTS and PLOTS
#-----------------------------------

# Table for youden index and sampling combined
rocTable <- round(rbind(coords_lass5, coords_lass, coords_down, coords_up, coords_smote), digits=3)
# add AUC to the table
roc_lass$auc
roc_down$auc
roc_up$auc
roc_smote$auc
auc <- c(0.864, 0.864, 0.864, 0.865, 0.864)
rocTable <- cbind(rocTable, auc)
rocTable[,2:7] <- rocTable[,2:7]*100

write.csv(rocTable, '/Users/alyssaforber/Box Sync/AlyssaKatieResearch/Opioids/Results/LassRocTable.csv')


# Table with all results, sampling, youden, and samplig youden combined
rocTable <- round(rbind(coords_lass5, coords_lass, coords_down5, coords_down, 
                        coords_up5, coords_up, coords_smote5, coords_smote), digits=3)
# Add AUC
auc <- c(0.864, 0.864, 0.864, 0.864, 0.865, 0.865, 0.864, 0.864)
# Add # variables 
# (counting genm_raceoth, genm_racehisp, genm_raceaa as separate and 
# racehisp, raceaa, raceoth as separates)
# down= 34
# up= 34
# smote= 33
# original= 31
vars <- c(31, 31, 34, 34, 34, 34, 33, 33)
rocTable <- cbind(rocTable, auc, vars)
rocTable[,2:7] <- rocTable[,2:7]*100

write.csv(rocTable, '/Users/alyssaforber/Box Sync/AlyssaKatieResearch/Opioids/Results/LassRocTable11302017.csv')



plot.roc(smooth(roc_lass), asp = NA)

plot.roc(roc_lass, print.thres = coords_lass["threshold"], identity = T, main = "Unsampled ROC Curve")
plot.roc(roc_lass, print.thres = 0.5, identity = T)
plot.roc(roc_down, print.thres = coords_down["threshold"], identity = T)
plot.roc(roc_up, print.thres = coords_up["threshold"], identity = T)
plot.roc(roc_smote, print.thres = coords_smote["threshold"], identity = T)

plot.roc(roc_lass, print.thres = c(coords_lass["threshold"], 0.5), identity = T, main = "Unsampled ROC Curve", asp = NA)

# plot two at the same time
plot.roc(roc_lass, print.thres = coords_lass["threshold"], identity = T, print.thres.col="black")
plot.roc(roc_down, print.thres = coords_down["threshold"], col = "blue", add = T)
legend(legend = c("Original Data", "Down Sampled Data") ,lty = "solid", col = c("black", "blue"), "bottomright", cex = 0.8, bty = "n")
