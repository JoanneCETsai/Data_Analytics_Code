library(TTR)

# Read in the data
frame<-read.csv("PatientArrivals.csv")
head(frame)

# Compute a weighted moving average with weights: (1/6,2/6,3/6)
wma.pred<-WMA(frame$Patient.arrival, n=3, wts=1:3) #wts specifies the weights for 
# each point, from farthest back to nearest. wts can be specified as any vector, or as 
# shown here, where each number is divided by the sum of the vector

# Plot original data 
plot(frame$Patient.arrival, type="l", ylim=c(450,510), xlim=c(0,30),
     xlab="days", ylab="patients")

# Add moving average to the plot
lines(wma.pred, col=2)

# Print fitted values
print(wma.pred)
