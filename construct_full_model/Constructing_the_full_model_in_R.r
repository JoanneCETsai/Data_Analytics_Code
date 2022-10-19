#multiple regression
frame<-read.csv("LifeExpPlus.csv", row.names = 1) #Load the Life Expectancy Data Set
head(frame) #Examine the Life Expectancy Data Set
cor(frame, method="pearson") #Look at the correlation matrix for all variables in this data set
plot(x=frame$GDP.per.capita, y=frame$AvgLifeExp) # Plot the GDP/Capita and Avg life expectancy. Note that we’ve called these columns of data from our dataframe with the notation dataset$ColumnName. 
plot(x=frame$logGDP, y=frame$AvgLifeExp) 
plot(x=frame$Inequity, y=frame$AvgLifeExp)
plot(x=frame$EducSpending, y=frame$AvgLifeExp)
plot(x=frame$PovertyRate, y=frame$AvgLifeExp)
model<-lm(formula =frame$AvgLifeExp ~frame$logGDP+frame$Inequity + frame$EducSpending+ frame$PovertyRate, data=frame) # The lm function creates 
# a linear model with our dataframe. The argument formula specifies our model in the 
# format Y ~ X1 + X2 + X3…+Xn. The argument data tells the function lm which
# dataset contains the variables we’d like to model. This makes the notation 
# dataset$ColumnName in the formula unnecessary, but you can use either notation. Often,
# when you compare models with many variables it is easier to specify the dataset than to use
# $ notation. 
summary(model)  # This function tells you about the model, including the intercept and coefficients, their errors, and t- and p-values. It also tells you values for the R^2, adjusted R^2, and F-statistic.
anova_model<-anova(model) 
anova_model # This gives you the analysis of variance table for the model, which gives you more information about the errors around the coefficients
