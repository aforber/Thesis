#---------------------
# OPIOID SAMPLING
# ALYSSA FORBER
# APRIL 24, 2018
# MODIFIED 
#-------------------
# How to handle indicators in smote
# do sensitivity analysis for options
# 1. don't let smote touch indicators
# 2. let it create new number and then round to 0 or 1

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

# make sure char_surg is treated as factor for either method (it's a numerical factor, ints 1-17)
#full_train$char_surg <- as.factor(full_train$char_surg)
# actually since the model has treated it numeric it is fine to leave unrounded 

# GOING TO TRY TWO SMOTE METHODS

# 1.---------
# make all indicators factors so smote does not touch them
full_train_smote1 <- full_train

cols <- c("genderm", "racehisp", "raceaa", "raceoth", "genm_racehisp", 
          "genm_raceaa", "chphx_surg", "genm_raceoth", "prior12", "OP_Receipt",
          "NOP_past", "Benzo_past", "SUHxDx_alch", "SUHxDx_Stml", "SUHxDx_tabc",
          "MHHxDx", "ChronicPHxDx", "AcutePHxDx", "ChronicPDcDx", "AcutePDcDx",
          "Surg_any", "NeoplasmDcDx", "NeoplasmHxDx", "oprec_surg", "chpdc_surg")
full_train_smote1[cols] <- lapply(full_train_smote1[cols], factor)

# defaults are 200 up and 200 down
smote_train1 <- SMOTE(Op_Chronic ~ ., data  = full_train_smote1)                         
table(smote_train1$Op_Chronic)

# 2.---------
# let smote use indicators but make sure they are rounded at the end
full_train_smote2 <- full_train

smote_train2 <- SMOTE(Op_Chronic ~ ., data  = full_train_smote2)                         
table(smote_train2$Op_Chronic)

smote_train2[,cols] <- round(smote_train2[,cols]) 

# put char_surg back as numerical for the analysis
#full_train$char_surg <- as.numeric(full_train$char_surg)
#smote_train1$char_surg <- as.numeric(smote_train1$char_surg)
#smote_train2$char_surg <- as.numeric(smote_train2$char_surg)
# Not worried about this because it can just go in smote and stay numerical the whole time


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


#--------------------
## SMOTE SAMPLE 1
#--------------------

# run model.matrix for train data
newtrain <- model.matrix(Op_Chronic~.,data=smote_train1) 

# run cv.glmnet with matrix
cvlasso <- cv.glmnet(newtrain, y = as.factor(smote_train1$Op_Chronic), family = "binomial")
min <- cvlasso$lambda.min
coefs_smote1 <- coef(cvlasso, s= "lambda.min")


newtest <- model.matrix(full_test$Op_Chronic ~ . , data = full_test)

# predict with matrix
predict_smote <- predict(cvlasso, newtest, type = "response", s = "lambda.min")


hist(predict_smote)
confmat_smote <- as.data.frame(cbind(full_test$Op_Chronic, predict_smote))
colnames(confmat_smote) <- c("Op_Chronic", "Pred")


###### pROC PACKAGE
roc_smote1 <- roc(confmat_smote$Op_Chronic, confmat_smote$Pred)
# Area under the curve: 


#### calculate with youden (cutoff is 0.47)
coords_smote1 <- coords(roc_smote1, x = "best", best.method = "youden", 
                       ret = c("threshold", "specificity", "sensitivity", 
                               "npv", "ppv", "accuracy"))


#--------------------
## SMOTE SAMPLE 2
#--------------------

# run model.matrix for train data
newtrain <- model.matrix(Op_Chronic~.,data=smote_train2) 

# run cv.glmnet with matrix
cvlasso <- cv.glmnet(newtrain, y = as.factor(smote_train2$Op_Chronic), family = "binomial")
min <- cvlasso$lambda.min
coefs_smote2 <- coef(cvlasso, s= "lambda.min")


newtest <- model.matrix(full_test$Op_Chronic ~ . , data = full_test)

# predict with matrix
predict_smote <- predict(cvlasso, newtest, type = "response", s = "lambda.min")


hist(predict_smote)
confmat_smote <- as.data.frame(cbind(full_test$Op_Chronic, predict_smote))
colnames(confmat_smote) <- c("Op_Chronic", "Pred")


###### pROC PACKAGE
roc_smote2 <- roc(confmat_smote$Op_Chronic, confmat_smote$Pred)
# Area under the curve: 


#### calculate with youden (cutoff is 0.47)
coords_smote2 <- coords(roc_smote2, x = "best", best.method = "youden", 
                       ret = c("threshold", "specificity", "sensitivity", 
                               "npv", "ppv", "accuracy"))



#-----------------------------------
# RESULTS and PLOTS
#-----------------------------------

# Table for youden index and sampling combined
rocTable <- round(rbind(coords_lass5, coords_lass, coords_down, coords_up, 
                        coords_smote1, coords_smote2), digits=2)

# Add AUC
auc <- c(roc_lass$auc, roc_lass$auc, roc_down$auc, roc_up$auc, roc_smote1$auc, roc_smote2$auc)

# Add # variables 
# (counting genm_raceoth, genm_racehisp, genm_raceaa as separate and 
# racehisp, raceaa, raceoth as separates)

vars <- c(32, 32, 33, 34, 33, 34)
rocTable <- cbind(rocTable, auc, vars)
rocTable[,7] <- round(rocTable[,7], digits=2)
rocTable[,2:7] <- rocTable[,2:7]*100

rownames(rocTable) <- c("Full training 0.5", "Full training", "Down sampled", "Up Sampled",
                        "SMOTE ignore", "SMOTE round")
rocTable

#write.csv(rocTable, '/Users/alyssaforber/Documents/Denver/Thesis/Results/LassRocTable4262018.csv')



plot.roc(smooth(roc_lass), asp = NA)

plot.roc(roc_lass, print.thres = coords_lass["threshold"], identity = T, main = "Unsampled ROC Curve")
plot.roc(roc_lass, print.thres = 0.5, identity = T)
plot.roc(roc_down, print.thres = coords_down["threshold"], identity = T)
plot.roc(roc_up, print.thres = coords_up["threshold"], identity = T)
plot.roc(roc_smote, print.thres = coords_smote["threshold"], identity = T)

plot.roc(roc_lass, print.thres = c(coords_lass["threshold"], 0.5), identity = T, 
         main = "Unsampled ROC Curve", asp = NA)

# plot two at the same time
plot.roc(roc_lass, print.thres = coords_lass["threshold"], identity = T, print.thres.col="black")
plot.roc(roc_down, print.thres = coords_down["threshold"], col = "blue", add = T)
legend(legend = c("Original Data", "Down Sampled Data") ,lty = "solid", col = c("black", "blue"), 
       "bottomright", cex = 0.8, bty = "n")
