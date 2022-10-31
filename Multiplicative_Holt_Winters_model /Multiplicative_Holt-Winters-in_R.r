library(stats)
require(graphics)

# Read in the data
frame<-read.csv("VMTdataTSonly.csv")

# Create a time series object from the data frame
x<-ts(frame, frequency = 12) # monthly data so frequency=12
print(x)

# Compute multiplicative HW model with manually specified parameters
fit1<-HoltWinters(x, seasonal="mult", alpha=0.5, beta=0.5, gamma=0.5)

print(fit1) # view the model

print(fit1$SSE) # print sum of squared errors metric

# Compute multiplicative HW model and have R estimate parameters from data 
fit2<-HoltWinters(x, seasonal="mult") #seasonal = “mult” specifies that R should use a multiplicative model

print(fit2) # view the model

print(fit2$SSE) # print sum of squared errors metric


# Plot and compare results
plot(fit1, xlim=c(1.5,10.5), ylim=c(200000,300000),
     main = "Holt-Winters", lwd=2)
lines(fit2$fitted[,1],col='blue', lwd=2)
legend('topleft', col=c('black','red','blue'), lty=1, cex=0.8,
       legend=c('Data','Manual Parameter HW','Estimated Parameter HW'))
