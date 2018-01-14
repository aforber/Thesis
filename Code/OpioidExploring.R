#-----------------------------
### COT THESIS
### AUTHOR: ALYSSA FORBER
### DATE: JANUARY 8, 2018
### DATE MODIFIED: 
#-----------------------------

# SIMULATION

#- Simulation of different percentages for rare outcomes
#- which method performs the best for 5% 10% 50%
#- plogis(y, 1, int + b1 x age + b2 x op_receipt + ...) for simulating the y given covariates
#- where int <- -1.5 (vary this) b1 <- x (can use estimates from Susan's paper)
#               - we get a y (the only thing we need to generate, not the data)
#               - then use lasso with y and xmatrix and predict to get confusion matrix 
#               - table 3 will be prevelance (5, 20, 50, etc maybe 10 values), and youden vs sampling with the different sens, spec 


# notes 1.9.18

# start with x var matrix and y 
# just pick three or four variables (most important ones- have to determine with AIC)
# can look at chi squares in table from paper
# look at coefficients (could fit a logistic regression and use those)
# then look at a prop table for ysim (you can save just the mean ysim)
# can start with 10 times but do it 1000 times
# also save sens, spec, and AUC all 1000
# then you can look at the mean and sd of those

 
# BAGGING WITH LASSO

#install.packages("BDgraph")
#install.packages("SparseLearner")
library(SparseLearner)
?Bagging.lasso

# instead of 
cvlasso <- cv.glmnet(newtrain, y = as.factor(full_train$Op_Chronic), family = "binomial")

# we do..
Bagging.lasso(newtrain, y = as.factor(full_train$Op_Chronic), family = "binomial")
# is there anything else I need to add?


# STACKING

#https://machinelearningmastery.com/machine-learning-ensembles-with-r/

#
