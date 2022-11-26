library(neuralnet)

# Read in the validation data set
frame<-read.csv("LifeExpectancyValidationNoNames.csv")

# Load the model that was estimated earlier
load('Net.Est')

# Create the normalization function
normalize<-function(x) {return ((x-min(x))/(max(x)-min(x)))}

# Scale the validation data set so that each column is between 0 and 1
maxmindf<-as.data.frame(lapply(frame, normalize))

# Plot the data to make sure the scaling worked, the y-axis should be between 0 and 1
plot(x=maxmindf$GDPpercapita, y=maxmindf$AverageLifeExpectancy,pch=16,
     xlab='GDP per capita',ylab='Average Life Expectancy')

# Compute predicted values on the inputs from the validation data set
predictions<-predict(Net.Est,maxmindf)

# Convert the predicted values back into years
unscalepred<- predictions*(max(frame$AverageLifeExpectancy)-min(frame$AverageLifeExpectancy))+min(frame$AverageLifeExpectancy) 

# Compute mean absolute error between predicted and true values
mean(abs(unscalepred-frame$AverageLifeExpectancy))

# Plot the original data along with the model predictions
plot(frame$GDPpercapita,frame$AverageLifeExpectancy,pch=16,
     xlab='GDP per capita',ylab='Average Life Expectancy')
points(frame$GDPpercapita, unscalepred,pch=16,col='red') # model predictions are plotted in red

# Plot the true values vs the fitted values
plot(unscalepred, frame$AverageLifeExpectancy,pch=16,
     xlim=c(60, 80), ylim=c(60,80))
abline(0,1,lw=3,col='red') # accurate predictions will lie close the line y=x in red

# Compute the correlation between the model predictions and true values
cor(frame$AverageLifeExpectancy, unscalepred) # the closer this number is to 1 the better

