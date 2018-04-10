#-----------------------------
### COT THESIS
### AUTHOR: ALYSSA FORBER
### DATE: MARCH 28, 2018
### DATE MODIFIED: 
#-----------------------------
# Try boosting
# GBM package and SVM 

rm(list=ls())

#install.packages("gbm")
library(gbm)
library(pROC)

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
full_train$visit_year <- NULL
full_test$visit_year <- NULL


# stump #
#gbm1 <- gbm(as.factor(chd) ~ sbp + tobacco + ldl + adiposity + family + typea +
#              obesity + alcohol + age, distribution = "bernoulli", data = SAheart,
#              n.trees = 400, cv=5, interaction.depth=1)
#summary(gbm1)
#print(gbm1)


#----------------
# FULL TRAINING
#----------------
newtrain <- as.data.frame(model.matrix(Op_Chronic~.,data=full_train))
newtrain$`(Intercept)` <- NULL
gbm1 <- gbm(full_train$Op_Chronic ~ ., distribution = "bernoulli", 
            data = newtrain, n.trees = 400, cv=5, interaction.depth=1)
summary(gbm1)
print(gbm1)

newtest <- as.data.frame(model.matrix(full_test$Op_Chronic ~ . , data = full_test))
pred <- predict.gbm(gbm1, newtest, type = "response")

hist(pred, main = "Histogram of Unampled Predicted Probabilites",
     xlab = "Predicted Probability")
confmat <- as.data.frame(cbind(full_test$Op_Chronic, pred))
colnames(confmat) <- c("Op_Chronic", "Pred")
roc_full <- roc(confmat$Op_Chronic, confmat$Pred)
 
# calculate with youden 
coords_full <- coords(roc_full, x = "best", best.method = "youden", 
                      ret = c("threshold",  "sensitivity", "specificity",
                              "npv", "ppv", "accuracy"))

# calculate with 0.5 cutoff
coords_full5 <- coords(roc_full, x = 0.5, input = "threshold",
                       ret = c("threshold",  "sensitivity", "specificity",
                               "npv", "ppv", "accuracy"))
# PLOT
plot.roc(roc_full, print.thres = coords_full["threshold"], identity = T, main = "Unsampled ROC Curve")


#--------------
# DOWN SAMPLE
#--------------

set.seed(747)
library(caret)

#get data with only predictors
predictors <- full_train
predictors$Op_Chronic <- NULL

down_train <- downSample(x = predictors,
                         y = as.factor(full_train$Op_Chronic))

newdowntrain <- as.data.frame(model.matrix(Class~.,data=down_train))
newdowntrain$`(Intercept)` <- NULL
# getting some error
gbm2 <- gbm(full_train$Op_Chronic ~ ., distribution = "bernoulli", 
            data = newdowntrain, n.trees = 400, cv=5, interaction.depth=1)
summary(gbm2)
print(gbm2)

newtest <- as.data.frame(model.matrix(full_test$Op_Chronic ~ . , data = full_test))
pred <- predict.gbm(gbm1, newtest, type = "response")
