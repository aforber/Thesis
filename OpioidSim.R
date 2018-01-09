#-----------------------------
### COT THESIS
### AUTHOR: ALYSSA FORBER
### DATE: JANUARY 9, 2018
### DATE MODIFIED: 
#-----------------------------


rm(list=ls())

#install.packages("DMwR")
library(DMwR)
#install.packages("caret")
library(caret)
#install.packages("glmnet")
library(glmnet)
#install.packages("pROC")
library(pROC)


# 1. Pick three or four most important vars to use:
# age, chronic pain, past year number of opioid, 
# receipt of opioid at discharge

# 2. Run logistic regression with them to get coefficients

opd = read.csv('/Users/alyssaforber/Box Sync/AlyssaKatieResearch/Opioids/Data/ropdata5_red.csv')

opd = opd[c("age" ,  "Op_Chronic" , "OP_Receipt" , "PriorOp5" , "ChronicPHxDx" ,
            "ChronicPDcDx" , "visit_year", "Op_PastChronic")]

# splitting 08-11 and 12-14 to make 2/3 split
train <- subset(opd, visit_year < 2012)
train$visit_year <- NULL

glm <- glm(Op_Chronic ~ "" + "" + "", data = train, family = "binomial")
summary(glm)


# COEFFICIENTS
b.int = -2.8  ## intercept
b.age =  -.05  ## age
b.chronic = -0.5 ## chronic pain
b.num = -.7 ## past year number of opioid
b.receipt = 0 ## receipt of opioid at discharge

niterations <- 1

# EMPTY MATRICES
fullY <- matrix(NA, niterations)
full5 <- matrix(NA, niterations)
upY <- matrix(NA, niterations)
up5 <- matrix(NA, niterations)
downY <- matrix(NA, niterations)
down5 <- matrix(NA, niterations)
smoteY <- matrix(NA, niterations)
smote5 <- matrix(NA, niterations)
ysim <- vector()


for (i in 1:niterations){
  
  #---------------------------
  # CREATE SIMULATED OUTCOMES
  #---------------------------
  all.opd <- as.data.frame(transform(fulltrain, Op_Chronic_Sim=rbinom(27705, 1, 
                                                         plogis(b.int + b.age*opd$age + 
                                                                b.chronic*opd$chronic + 
                                                                b.num*opd$PriorOp5 + 
                                                                b.receipt*opd$OP_Receipt))))
  # save the outcome percentage
  ysim[i] <- mean(Op_Chronic_Sim)
  
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

  #defaults are 200 up and 200 down
  smote_train <- SMOTE(Op_Chronic_Sim ~ ., data  = full_train) 
  
  #--------------------------
  # RUN MODELS
  #--------------------------
  
  #----------------
  ## ORIGINAL DATA
  #----------------
  
  # run model.matrix for train data
  newtrain <- model.matrix(Op_Chronic_Sim~.,data=full_train) 
  
  # run cv.glmnet with matrix
  cvlasso <- cv.glmnet(newtrain, y = as.factor(full_train$Op_Chronic_Sim), family = "binomial")
  
  # model.matrix for test data
  newtest <- model.matrix(full_test$Op_Chronic_Sim ~ . , data = full_test)
  
  # predict with matrix
  predict_lass <- predict(cvlasso, newtest, type = "response", s = "lambda.min")
  
  ###### pROC PACKAGE
  roc_lass <- roc(full_test$Op_Chronic_Sim, predict_lass)
 
  #### calculate with youden
  fullY[i] <- coords(roc_lass, x = "best", best.method = "youden", 
                        ret = c("specificity", "sensitivity", "accuracy"))
  
  #### calculate with 0.5 cutoff
  full5[i] <- coords(roc_lass, x = 0.5, input = "threshold",
                         ret = c("specificity", "sensitivity", "accuracy"))
  
  #-------------
  # DOWN SAMPLE
  #-------------
  
  # run model.matrix for train data
  newtrain <- model.matrix(Class~.,data=down_train) 
  
  # run cv.glmnet with matrix
  cvlasso <- cv.glmnet(newtrain, y = as.factor(down_train$Class), family = "binomial")
  
  newtest <- model.matrix(full_test$Op_Chronic_Sim ~ . , data = full_test)
  
  # predict with matrix
  predict_down <- predict(cvlasso, newtest, type = "response", s = "lambda.min")
  
  ###### pROC PACKAGE
  roc_down <- roc(full_test$Op_Chronic_Sim, predict_down)
  
  #### calculate with youden
  downY[i] <- coords(roc_down, x = "best", best.method = "youden", 
                        ret = c("specificity", "sensitivity", "accuracy"))
  
  #### calculate with 0.5 cutoff
  down5[i] <- coords(roc_down, x = 0.5, input = "threshold",
                         ret = c("specificity", "sensitivity", "accuracy"))
  
  #--------------------
  # UP SAMPLE
  #--------------------
  
  # run model.matrix for train data
  newtrain <- model.matrix(Class~.,data=up_train) 
  
  # run cv.glmnet with matrix
  cvlasso <- cv.glmnet(newtrain, y = as.factor(up_train$Class), family = "binomial")
  
  newtest <- model.matrix(full_test$Op_Chronic_Sim ~ . , data = full_test)
  
  # predict with matrix
  predict_up <- predict(cvlasso, newtest, type = "response", s = "lambda.min")
  
  ###### pROC PACKAGE
  roc_up <- roc(full_test$Op_Chronic_Sim, predict_up)

  #### calculate with youden
  upY[i] <- coords(roc_up, x = "best", best.method = "youden", 
                      ret = c("specificity", "sensitivity", "accuracy"))
  
  #### calculate with 0.5 cutoff
  up5[i] <- coords(roc_up, x = 0.5, input = "threshold",
                       ret = c("specificity", "sensitivity", "accuracy"))
  
  
  #--------------------
  # SMOTE SAMPLE
  #--------------------
  
  # run model.matrix for train data
  newtrain <- model.matrix(Op_Chronic_Sim~.,data=smote_train) 
  
  # run cv.glmnet with matrix
  cvlasso <- cv.glmnet(newtrain, y = as.factor(smote_train$Op_Chronic_Sim), family = "binomial")
  
  newtest <- model.matrix(full_test$Op_Chronic_Sim ~ . , data = full_test)
  
  # predict with matrix
  predict_smote <- predict(cvlasso, newtest, type = "response", s = "lambda.min")
  
  ###### pROC PACKAGE
  roc_smote <- roc(full_test$Op_Chronic_Sim, predict_smote)
  
  #### calculate with youden 
  smoteY[i] <- coords(roc_smote, x = "best", best.method = "youden", 
                         ret = c("specificity", "sensitivity", "accuracy"))
  
  #### calculate with 0.5 cutoff
  smote5[i] <- coords(roc_smote, x = 0.5, input = "threshold",
                          ret = c("specificity", "sensitivity", "accuracy"))
  
  

}







