#-----------------------------
### COT THESIS
### AUTHOR: ALYSSA FORBER
### DATE: JANUARY 8, 2018
### DATE MODIFIED: 
#-----------------------------



rm(list=ls())


#install.packages("BDgraph")
library(SparseLearner)


opd = read.csv('/Users/alyssaforber/Box Sync/AlyssaKatieResearch/Opioids/Data/ropdata5_red.csv')

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


## NOW TRY BAGGING
# Not entirely sure how all the details work

bag_full <- Bagging.lasso(newtrain, y = as.factor(full_train$Op_Chronic), family = "binomial")
# this takes a very long time to run!! 
# a couple minutes for each iteration and it defaults 100 iterations!

# started at 11am


pred_bag_full <- Predict.bagging(bag, newx= newtest)
hist(pred_bag_full)


