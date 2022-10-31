# Part one
library(TTR)
sma.pred<-SMA(c(6,8,7,10,11,14), n=3) 
sma.pred

# Part two
# Read in the data
frame<-read.csv("PatientArrivals.csv")

# Fit exponential smoothing model with smoothing parameter alpha = 0.4
esmodel1<-ets(frame$Patient.arrival, model="ANN", alpha=0.4 ) 
es.pred1<-forecast(esmodel1, h=1, level=0) # compute model predictions
# Plot original data
plot(frame$Patient.arrival, ylim = c(440, 520),
     ylab = "Patients", xlab = "Day",
     xlim = c(0,29),pch=16, type = 'o')

lines(es.pred1$fitted, col="blue") # alpha = 0.4

accuracy(es.pred1) # alpha = 0.4

tracking_signal<-nrow(frame)*1.722602/18.34819
tracking_signal

# Part 3 (A)
library(stats)

# Read in the data
frame<-read.csv("/Users/chiaentsai/Desktop/Data_Analytics/Data_Analytics_Code/Additive_Holt_Winters/VMTdataTSonly.csv")

# Create time series object from data frame
x<-ts(frame, frequency = 12)
print(x)
#seasonal = “add” specifies that R should use a multiplicative model

#optimize parameters     
fit2<-HoltWinters(x, seasonal="add")

print(fit2$SSE)

plot(fit2,xlim=c(1.5,10.5),ylim=c(200000,300000),
     main = "Holt-Winters",lw=2)

legend('topleft',col=c('black','red'),lty=1,cex=0.8,
       legend=c('Data','Estimate Parameter HW'))

fit2$coefficients
fit2$alpha
fit2$beta
fit2$gamma
fit2$seasonal
fit2$SSE

# Part 3 (B)
frame<-read.csv("/Users/chiaentsai/Desktop/Data_Analytics/Data_Analytics_Code/hw6_part3/UnemploymentData.csv")
x<-ts(frame, frequency = 12)
print(x)

# Additive model
fit3<-HoltWinters(x, seasonal="add")

print(fit3$SSE)

plot(fit3,xlim=c(1.5,10.5),ylim=c(2,12),
     main = "Holt-Winters",lw=2)

legend('topleft',col=c('black','red'),lty=1,cex=0.8,
       legend=c('Data','Estimate Parameter HW'))

# Multiplicative model
fit4<-HoltWinters(x, seasonal="multi")

print(fit4$SSE)

plot(fit4,xlim=c(1.5,10.5),ylim=c(2,12),
     main = "Holt-Winters",lw=2)

legend('topleft',col=c('black','red'),lty=1,cex=0.8,
       legend=c('Data','Estimate Parameter HW'))
fit4$alpha
fit4$beta
fit4$gamma

# Part 4
# Read in the data
frame<-read.csv("VMTdataTSonly.csv")

print(frame) # examine the unformatted data frame

# Create time series object with a frequency of 12 seasons from the data frame
x<-ts(frame, frequency = 12)

print(x) # examine the formatted time series with 12 seasons

# Fit linear regression for time series forecasting
model<-tslm(x~trend+season) # the formatted time series allows the tslm function to identify the season of each period

# Examine the model coefficients
print(model) #note that this method doesn’t give you the p-values, which is helpful because you # don’t want to use them for time series. 
