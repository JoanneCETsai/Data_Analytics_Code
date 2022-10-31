library(stats)

# Read in the data
frame<-read.csv("VMTdataTSonly.csv")

# Create time series object from data frame
x<-ts(frame, frequency = 12)
print(x)

# Compute HW model with manually specified parameters
fit1<-HoltWinters(x, seasonal="add", alpha=0.5, beta=0.5, gamma=0.5)
#seasonal = “add” specifies that R should use a multiplicative model

print(fit1$SSE)

#optimize parameters     
fit2<-HoltWinters(x, seasonal="add")

print(fit2$SSE)


plot(fit1,xlim=c(1.5,10.5),ylim=c(200000,300000),
     main = "Holt-Winters",lw=2)
lines(fit2$fitted[,1],col='blue',lw=2)
legend('topleft',col=c('black','red','blue'),lty=1,cex=0.8,
       legend=c('Data','Manual Parameter HW','Estimate Parameter HW'))
