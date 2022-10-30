library(TTR)

# Load in data
frame<-read.csv("PatientArrivals.csv")
head(frame)

# Compute simple moving average with 3 periods
sma.pred<-SMA(frame$Patient.arrival, n=3) # SMA() calculates the simple moving average. n = 3 
# indicates that we want to calculate the average over 3 periods. Note that this moving average # weights each data point equally.
print(sma.pred) #note that the forecast is inserted at the beginning of this array

# Plot the moving average
plot(sma.pred , type="l",col=2, ylim=c(450,510), xlim=c(0,30), xlab="days", ylab="patients")
# note that type indicates the type of points or line you want on the plot, ylim and xlim indicate 
# the range of the y or axis, respectively, and ylab and xlab specify the titles of the y and x axes, respectively.

# Add the original data to the plot
lines(frame$Patient.arrival) # note that the moving average is plotted at the last of the three periods used to compute it
