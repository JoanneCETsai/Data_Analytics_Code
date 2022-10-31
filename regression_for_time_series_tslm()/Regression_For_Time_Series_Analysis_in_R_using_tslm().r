library(forecast)

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
