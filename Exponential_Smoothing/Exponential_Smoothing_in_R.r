library(TTR)
library(forecast)

# Read in data
frame<-read.csv("PatientArrivals.csv")
head(frame)

# Use EMA( ) to fit an exponential moving average
ema.pred<-EMA(frame$Patient.arrival, n=3)  #alpha is 1/(n+1), so we’ve set the function to 
# decay exponentially over 3 time periods

# Plot the results
plot(frame$Patient.arrival, type="l", ylim=c(450,510), xlim=c(0,30),
     xlab="days", ylab="patients") # plot original data
lines(ema.pred, col=2) # add moving average to the plot

print(ema.pred) # print EMA values, note that this begins plotting at the 3rd x value because n = 3

# Use ets( ) to fit an exponential moving average
esmodel<-ets(frame$Patient.arrival, model="ANN", alpha=0.6 ) # the ets function is a general 
#exponential smoothing function with many options. model = “ANN” specifies the model as a 
# simple exponential function with additive errors. 

# Compute fitted values
es.pred<-forecast(esmodel, h=1, level=0) #The argument h is the number of periods the 
# forecast will be for, and the argument level is the confidence level for prediction intervals
es.pred$fitted

# Plot the results
plot(es.pred, ylim = c(440, 520), ylab = "Patients", xlab = "Day",
     xlim = c(0,29)) # plot original data 
lines(es.pred$fitted, col="blue") # add moving average to the plot. Note that this function gives a 
# model output for all of our data points. 
