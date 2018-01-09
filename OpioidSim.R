#-----------------------------
### COT THESIS
### AUTHOR: ALYSSA FORBER
### DATE: JANUARY 9, 2018
### DATE MODIFIED: 
#-----------------------------


rm(list=ls())

# Pick three or four most important vars to use:
# age, chronic pain, past year number of opioid, 
# receipt of opioid at discharge

# run logistic regression with them to get coefficients

opd = read.csv('/Users/alyssaforber/Box Sync/AlyssaKatieResearch/Opioids/Data/ropdata5_red.csv')


opdlog = opd[c("age" ,  "Op_Chronic" , "OP_Receipt" , "PriorOp5" , "ChronicPHxDx" ,
            "ChronicPDcDx" , "visit_year")]


# splitting 08-11 and 12-14 to make 2/3 split
train <- subset(opdlog, visit_year < 2012)
train$visit_year <- NULL

glm <- glm(Op_Chronic ~., data = train, family = "binomial")
summary(glm)

# This will give me my coefficients
b.int = -2.8  ## intercept
b.age =  -.05  ## age
b.chronic = -0.5 ## chronic pain
b.num = -.7 ## past year number of opioid
b.receipt = 0 ## receipt of opioid at discharge




niterations <- 1

full <- matrix(NA, niterations)
up <- matrix(NA, niterations)
down <- matrix(NA, niterations)
smote <- matrix(NA, niterations)










