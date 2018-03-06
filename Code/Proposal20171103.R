#---------------------
# OPIOID SAMPLING
# ALYSSA FORBER
# NOVEMBER 3 2017
# MODIFIED NOV 9,10,11 2017
#-------------------
# Re-creating what was done on 20170816 with lasso instead of logistic reg
# Not yet bootrapping/bagging but at least running basic lasso for proposal

rm(list=ls())

# from 20170816 (most recent)

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

# from Opioid20170730

#install.packages("glmnet")
library(glmnet)

# ORIGINAL - MAKE ALL FULL TRAIN 
lasso <- glmnet(cbind(full_train$age, full_train$agesq, full_train$genderm, full_train$inscat5, full_train$los_ln, 
                      full_train$mmecat5, full_train$OP_Receipt, full_train$PriorOp5, full_train$NOP_past,
                      full_train$Benzo_past, full_train$SUHxDx_tabc, full_train$ChronicPHxDx, 
                      full_train$ChronicPDcDx, full_train$Surg_any, full_train$CHARLSON), 
                y=as.factor(full_train$Op_Chronic), family="binomial")


# DOWN SAMPLE
lasso_down <- glmnet(cbind(down_train$age, down_train$agesq, down_train$genderm, down_train$inscat5, down_train$los_ln, 
                           down_train$mmecat5, down_train$OP_Receipt, down_train$PriorOp5, down_train$NOP_past,
                           down_train$Benzo_past, down_train$SUHxDx_tabc, down_train$ChronicPHxDx, 
                           down_train$ChronicPDcDx, down_train$Surg_any, down_train$CHARLSON), 
                     y=as.factor(down_train$Class), family="binomial")

# UP SAMPLE
lasso_up <- glmnet(cbind(up_train$age, up_train$agesq, up_train$genderm, up_train$inscat5, up_train$los_ln, 
                         up_train$mmecat5, up_train$OP_Receipt, up_train$PriorOp5, up_train$NOP_past,
                         up_train$Benzo_past, up_train$SUHxDx_tabc, up_train$ChronicPHxDx, 
                         up_train$ChronicPDcDx, up_train$Surg_any, up_train$CHARLSON), 
                   y=as.factor(up_train$Class), family="binomial")

# SMOTE SAMPLE
lasso_smote <- glmnet(cbind(smote_train$age, smote_train$agesq, smote_train$genderm, smote_train$inscat5, smote_train$los_ln, 
                            smote_train$mmecat5, smote_train$OP_Receipt, smote_train$PriorOp5, smote_train$NOP_past,
                            smote_train$Benzo_past, smote_train$SUHxDx_tabc, smote_train$ChronicPHxDx, 
                            smote_train$ChronicPDcDx, smote_train$Surg_any, smote_train$CHARLSON), 
                      y=as.factor(smote_train$Op_Chronic), family="binomial")

#-------------------------------------------------
# PREDICT ON ALL SAMPLES
# ORIGINAL, DOWN, UP, SMOTE
#-------------------------------------------------


#--------------------
## ORIGINAL DATA
#--------------------
set.seed(44)

# test dataset might need to be in a matrix form..


x_test <- full_test[, -6]
full_test2 <- model.matrix(~ . -as.factor(full_test$Op_Chronic), data=x_test)
predict_lass <- predict(lasso, full_test2, type = "response")

predDown <- predict(downLasso, cbind(test$age, test$agesq, test$genderm, test$racehisp, test$raceaa, 
                                     test$raceoth, test$genm_racehisp, test$genm_raceaa, test$genm_raceoth,
                                     test$inscat5, test$los_ln, test$mmecat5, test$prior12, test$OP_Receipt, test$PriorOp5,
                                     test$NOP_past, test$Benzo_past, test$SUHxDx_alch, test$SUHxDx_Stml,
                                     test$SUHxDx_tabc, test$MHHxDx, test$ChronicPHxDx, test$AcutePHxDx,
                                     test$ChronicPDcDx, test$AcutePDcDx, test$Surg_any, test$CHARLSON, 
                                     test$char_surg, test$oprec_surg, test$chpdc_surg, test$chphx_surg),
                    s="lambda.min", type="response")


predLass <- predict(lasso, cbind(full_test$age, full_test$agesq, full_test$genderm, full_test$racehisp, full_test$raceaa,
                                 full_test$raceoth, full_test$genm_racehisp, full_test$genm_raceaa, full_test$genm_raceoth, 
                                 full_test$inscat5, full_test$los_ln, full_test$mmecat5, full_test$prior12, full_test$OP_Receipt, full_test$PriorOp5,
                                 full_test$NOP_past, full_test$Benzo_past, full_test$SUHxDx_alch, full_test$SUHxDx_Stml,
                                 full_test$SUHxDx_tabc, full_test$MHHxDx, full_test$chronicPHxDx, full_test$AcutePHxDx,
                                 full_test$ChronicPDcDx, full_test$AcutePDcDx, full_test$Surg_any, full_test$CHARLSON, 
                                 full_test$NeoplasmDcDx, full_test$NeoplasmHxDx, full_test$post_index_hospitalizations,         
                                 full_test$char_surg, full_test$oprec_surg, full_test$chpdc_surg, full_test$chphx_surg),
                    s="lambda.min", type="response")


predLass <- predict(lasso, full_test2, type="response")


hist(predict_lass)
confmat_lass <- as.data.frame(cbind(full_test$Op_Chronic, predict_lass))
colnames(confmat_lass) <- c("Op_Chronic", "Pred")


###### pROC PACKAGE
roc_lass <- roc(confmat_lass$Op_Chronic, confmat_lass$Pred)
# Area under the curve: 


#### calculate with youden
coords_lass <- coords(roc_lass, x = "best", best.method = "youden", 
                      ret = c("threshold", "specificity", "sensitivity", 
                              "accuracy", "npv", "ppv"))



#--------------------
## DOWN DATA
#--------------------


#--------------------
## UP DATA
#--------------------


#--------------------
## SMOTE DATA
#--------------------






#--------------------
## ORIGINAL DATA
#--------------------

# run model.matrix for train data
newtrain <- model.matrix(Op_Chronic~.,data=full_train) 


# run cv.glmnet with matrix
cvlasso <- cv.glmnet(newtrain, y = as.factor(full_train$Op_Chronic), family = "binomial")

min <- cvlasso$lambda.min


# run glmnet with matrix
lasso <- glmnet(newtrain, y=as.factor(full_train$Op_Chronic), family="binomial", lambda = min)


# model.matrix for test data
newtest4 <- model.matrix(full_test$Op_Chronic ~ . , data = full_test)


# predict with matrix
pred <- predict(lasso, newtest3, type = "response", s = "lambda.min")











#--------------------
## DOWN DATA
#--------------------

cvlasso_down <- cv.glmnet(cbind(down_train$age, down_train$agesq, down_train$genderm, down_train$inscat5, down_train$los_ln, 
                              down_train$mmecat5, down_train$OP_Receipt, down_train$PriorOp5, down_train$NOP_past,
                              down_train$Benzo_past, down_train$SUHxDx_tabc, down_train$ChronicPHxDx, 
                              down_train$ChronicPDcDx, down_train$Surg_any, down_train$CHARLSON), 
                        y = as.factor(down_train$Class), family = "binomial")


#--------------------
## UP DATA
#--------------------

cvlasso_up <- cv.glmnet(cbind(up_train$age, up_train$agesq, up_train$genderm, up_train$inscat5, up_train$los_ln, 
                            up_train$mmecat5, up_train$OP_Receipt, up_train$PriorOp5, up_train$NOP_past,
                            up_train$Benzo_past, up_train$SUHxDx_tabc, up_train$ChronicPHxDx, 
                            up_train$ChronicPDcDx, up_train$Surg_any, up_train$CHARLSON), 
                      y = as.factor(up_train$Class), family = "binomial")


#--------------------
## SMOTE DATA
#--------------------
cvlasso_smote <- cv.glmnet(cbind(smote_train$age, smote_train$agesq, smote_train$genderm, smote_train$inscat5, smote_train$los_ln, 
                               smote_train$mmecat5, smote_train$OP_Receipt, smote_train$PriorOp5, smote_train$NOP_past,
                               smote_train$Benzo_past, smote_train$SUHxDx_tabc, smote_train$ChronicPHxDx, 
                               smote_train$ChronicPDcDx, smote_train$Surg_any, smote_train$CHARLSON), 
                         y = as.factor(smote_train$Op_Chronic), family = "binomial")



