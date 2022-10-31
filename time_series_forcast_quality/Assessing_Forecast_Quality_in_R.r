library(forecast)

# Read in the data
frame<-read.csv("PatientArrivals.csv")

# Fit exponential smoothing model with smoothing parameter alpha = 0.4
esmodel1<-ets(frame$Patient.arrival, model="ANN", alpha=0.4 ) 
es.pred1<-forecast(esmodel1, h=1, level=0) # compute model predictions

# Fit another model with alpha = 0.6
esmodel2<-ets(frame$Patient.arrival, model="ANN", alpha=0.6 ) 
es.pred2<-forecast(esmodel2, h=1, level=0)

# Fit third model with alpha = 0.8
es.model3<-ets(frame$Patient.arrival, model="ANN", alpha=0.8 ) 
es.pred3<-forecast(es.model3, h=1, level=0)

# Plot original data
plot(frame$Patient.arrival, ylim = c(440, 520),
     ylab = "Patients", xlab = "Day",
     xlim = c(0,29),pch=16, type = 'o') # Plot the data. The argument pch means the points will be # filled-in circles, type = o means both points and connected lines will be plotted


# Add the fitted values for all three models to the plot
lines(es.pred1$fitted, col="blue") # alpha = 0.4
lines(es.pred2$fitted, col="red") # alpha = 0.6
lines(es.pred3$fitted, col="grey") # alpha = 0.8. 

# Compute Error Metrics for each model
accuracy(es.pred1) # alpha = 0.4
accuracy(es.pred2) # alpha = 0.6
accuracy(es.pred3) # alpha = 0.8
