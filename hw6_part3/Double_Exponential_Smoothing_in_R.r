library(aTSA)

# Read in the data
frame<-read.csv("TrendEmergency.csv")

# Fit compute double exponential smoothing 
# with specified parameters alpha=0.168551 and beta=0.601583 that we found in Excel
fit1<-Holt(frame$Patients, type=c("additive"),
           alpha=0.168551, beta=0.601583, lead=1, plot=TRUE ) # The type argument specifies 
# that the model is additive, since we’re adding the average value and the trend. The argument # lead specifies the number of steps ahead the forecast is made for. The argument plot tells R 
# to make a plot of the smoothed model and actual data.

print(fit1$accurate) # error metrics
#Note that the estimates from this optimization problem may have different solutions based on 
# the starting alpha and beta: 
print(fit1$alpha)
print(fit1$beta)

# Compute double exponential smoothing
# with parameters alpha and beta optimized from the data
fit2<-Holt(frame$Patients, type=c("additive"),  plot=TRUE ) #Here, we haven’t specified the lead # so the default is 0

print(fit2$accurate) # error metrics
print(fit2$alpha)
print(fit2$beta)

# Plot and compare results
plot(frame$Patients,type='l')
lines(fit1$estimate,col='red') # plot fit1
lines(fit2$estimate,col='blue') # plot fit2
legend(x = 'topleft',col=c("black","red", "blue"),lty=1,cex=0.8,
       legend=c('Data','Excel Parameter Smoothed Model','R-optimized Parameter Smoothed Model')) # this function adds a legend to an existing plot. Here, x =  ‘topleft’ 
#tells R to put the legend in the top left of the graph and col is a vector of colors that align with 
# the vector of labels that specify the legend. lty specifies the type of line and cex specifies the 
# size of the legend.
