library(neuralnet)

# Read in the data
frame<-read.csv("LifeExpectancyTrainingNoNames.csv")

# Create a function to normalize the data by scaling it between 0 and 1
normalize<-function(x) {return ((x-min(x))/(max(x)-min(x)))}

# Use the normalize function to normalize each column of frame
maxmindf<-as.data.frame(lapply(frame, normalize)) # This creates a new
# dataframe by applying the normalize function to each row of the dataset ‘frame’

# Plot the scaled data
plot(x=maxmindf$GDPpercapita, y=maxmindf$AverageLifeExpectancy,pch=16,
     xlab='GDP per capita',ylab='Average Life Expectancy')

# Estimate a neural network to predict AverageLifeExpectancy from GDPpercapita
Net.Est<-neuralnet(AverageLifeExpectancy~GDPpercapita, data=maxmindf)

# Visualize the resulting network and its weights
plot(Net.Est)

# Compute fitted values
predictions<-predict(Net.Est,maxmindf)
head(predictions) # predictions are between 0 and 1, so we need to "unscale" them

# Unscale the predictions to the original data range
unscalepred<- predictions*(max(frame$AverageLifeExpectancy)-min(frame$AverageLifeExpectancy) )+min(frame$AverageLifeExpectancy) 
print(unscalepred)

# Compute mean absolute error (MAE)
mean(abs(unscalepred-frame$AverageLifeExpectancy))

# Plot original data and fitted values
plot(x=frame$GDPpercapita, y=frame$AverageLifeExpectancy,pch=16,
     xlab='GDP per capita',ylab='Average Life Expectancy')
points(frame$GDPpercapita, unscalepred,pch=16,col='red')

# Plot True values versus predicted/fitted values
plot(unscalepred, frame$AverageLifeExpectancy,pch=16,
     xlim=c(60, 80), ylim=c(60,80),xlab='Predictions',ylab='True Values')
abline(a=0,b=1,col='red',lw=3) # correct predictions lie close to the line y=x

# Correlation between predicted and true values
cor(frame$AverageLifeExpectancy, unscalepred)
