library(ggplot2)
library(car)

# Read in data
frame<-read.csv("VMTdataRegression.csv")
head(frame,n = 12) # The argument n = 12 tells R to display 12 rows. The default is 6. 

# Create multiple regression model
model<-lm(formula =frame$VMT ~frame$TimePeriod+frame$Feb + frame$March + 
            frame$April+frame$May + frame$June + frame$July+
            frame$August + frame$September + frame$October+ 
            frame$November+ frame$December, data=frame)
summary(model) #This is a good way to get coefficient values, but based on the residuals and 
# durbin Watson test below, don’t use the p-values!

# Plot residuals versus time
# patterns on this plot indicate the presence of autocorrelation
ggplot(data = frame, aes(y = model$residuals, x = TimePeriod)) +
  xlab('Period') + ylab('Residuals') + 
  geom_point(col = 'blue') + geom_abline(slope = 0) # The function ggplot is a different way
# to plot data than the standard plotting function. The aes function indicates what should be 
# added to the plot, xlab and ylab indicate the labels, geom_point specifies the point color, and 
# geom_abline adds a line to the plot.

# DW test for autocorrelation
durbinWatsonTest(model, alternative="positive") # a significant p-value indicates presence of autocorrelation
