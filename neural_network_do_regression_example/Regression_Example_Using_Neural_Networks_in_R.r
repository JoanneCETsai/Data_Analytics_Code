library(neuralnet)

# Read in the data
frame<-read.csv("LifeExpectancyTraining.csv", row.names = 1)
head(frame)

# Plot the data using GDP, it does NOT look linear
plot(x=frame$GDP.per.capita, y=frame$Average.Life.Expectancy, pch=16,
     xlab='GDP per Capita',ylab='Average Life Expectancy')

# Plot the data with the log base 10 of GDP in place of GDP, the data looks linear now
plot(x=frame$log.GDP.per.capita, y=frame$Average.Life.Expectancy, pch=16,
     xlab='log(GDP) per Capita',ylab='Average Life Expectancy')
# Note that the dataset included a column with the log of GDP. The function to take log base 10 in R is log10()

##########################################################
# Simple Linear Regression Model
##########################################################

# We will use log base 10 of GDP as our predictor variable
model<-lm(formula =frame$Average.Life.Expectancy ~ frame$log.GDP.per.capita, data=frame)
summary(model)

##########################################################
# Simple Neural Network Model
##########################################################

# Define input and output variables to create the trainingdata data frame
input<-frame$log.GDP.per.capita
output<-frame$Average.Life.Expectancy
trainingdata<-cbind(input,output)
head(trainingdata)

# Estimate a neural network model with no hidden layers
Net.Est<-neuralnet(formula=output~input, data=trainingdata, hidden=0, threshold=0.005, lifesign="full")

# Graphically display the model coefficients, do they match the regression coefficients? (they should!)
plot(Net.Est)

# Compute fitted values from the training data
predictions<-predict(Net.Est,trainingdata)
head(predictions)

# Compute mean absolute error between true and fitted values
mean(abs(predictions-output)) # we are wrong on average by this many years

##########################################################
# Test the neural networks out of sample performance
##########################################################

# Read in the validation data set
frame<-read.csv("LifeExpectancyValidation.csv", row.names = 1)

# Define the validation data frame in the same manner as the training data frame
input<-frame$log.GDP.per.capita
output<-frame$Average.Life.Expectancy
validationdata<-cbind(input,output)

# Compute model predictions on the validation data set
predictions<-predict(Net.Est, newdata=validationdata)

# Compute mean absolute error between true and fitted values
mean(abs(predictions-output)) # we are wrong on average by this many years
