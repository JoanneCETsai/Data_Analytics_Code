library(leaps) # for best subsets

# Read in the data
frame<-read.csv("LifeExpPlus.csv", row.names=1)
frame$povinc<-frame$Inequity*frame$PovertyRate # create interaction term

####################################################
# Backward elimination
####################################################

step(lm(AvgLifeExp~logGDP+Inequity+PovertyRate+EducSpending+povinc, data=frame),
     direction="backward")

####################################################
# Forward Selection
####################################################

step(lm(AvgLifeExp~1, data=frame), direction="forward", 
     scope=~logGDP+Inequity+PovertyRate+EducSpending+povinc)

####################################################
# Stepwise  Selection
####################################################

step(lm(AvgLifeExp~1, data=frame), direction="both",
     scope=~logGDP+Inequity+PovertyRate+EducSpending+povinc)

####################################################
# Best Subsets Selection
####################################################

models<-regsubsets(AvgLifeExp~logGDP+Inequity+PovertyRate+EducSpending+povinc,
                   data=frame, nvmax=5)
choices<-summary(models)  #Data window gives BIC, Adj R2
print(choices$which)
print(choices$adjr2)
print(choices$bic)

