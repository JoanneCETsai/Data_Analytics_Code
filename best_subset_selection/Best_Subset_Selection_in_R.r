#best subset selection
library(leaps) #load the leaps library
frame<-read.csv("LifeExpPlus.csv", row.names = 1)
povinc<-frame$Inequity*frame$PovertyRate
models<-regsubsets(frame$AvgLifeExp ~frame$logGDP+frame$Inequity +frame$PovertyRate+ frame$EducSpending+povinc, data=frame, method = "exhaustive", nvmax=5) # The argument 
# method tells the regsubsets function to use best subsets selection. The argument nvmax 
# specifies the maximum number of variables you’d like to consider in a model. 
choices<-summary(models)  #Data window gives BIC, Adj R2 #Creates the object choices, 
#which contains the summary data on the models and allows you to access the information 
#more easily. 
print(choices$which) #prints a TRUE/FALSE matrix that tells you which variables are included in 
# the best model for each number of variables.
print(choices$adjr2) # prints a matrix that tells you the adjusted R^2 value for the best models 
# for each number of variables. 
print(choices$bic) # prints a matrix that tells you the BIC value for the best models 
# for each number of variables. 