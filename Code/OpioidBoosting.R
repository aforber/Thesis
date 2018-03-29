#-----------------------------
### COT THESIS
### AUTHOR: ALYSSA FORBER
### DATE: MARCH 28, 2018
### DATE MODIFIED: 
#-----------------------------
# Try boosting
# BGM package and SVM 

# stump #
gbm1 <- gbm(as.factor(chd) ~ sbp + tobacco + ldl + adiposity + family + typea +
              obesity + alcohol + age, distribution = "bernoulli", data = SAheart,
              n.trees = 400, cv=5, interaction.depth=1)
summary(gbm1)
print(gbm1)

