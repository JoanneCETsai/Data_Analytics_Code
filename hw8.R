library(MASS)
library(neuralnet)

# load the data
frame<-read.csv("/Users/chiaentsai/Desktop/Data_Analytics/Data_Analytics_Code/hw8_neural/ElectionData.csv")
ncol(frame)
nrow(frame)
head(frame)

# normalize the data set
normalize<-function(x) {return ((x-min(x))/(max(x)-min(x)))}
df = subset(frame, select = -c(countystate, fips, US.regions))
maxmindf<-as.data.frame(lapply(df, normalize)) 

# check the scatter plot of features
plot(x=maxmindf$PacificCoast, y=maxmindf$Trump, col = "red",
     xlab='PacificCoast',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$PacificCoast, y=maxmindf$Clinton, col = "blue",
      xlab='PacificCoast',ylab='Trump/Clinton fraction')
legend("top",                                    
        legend=c("Trump","Clinton"),        
        col=c("red","blue"),                 
        lty=1,lwd=2)  


plot(x=maxmindf$MountainsPlains, y=maxmindf$Trump, col = "red",
     xlab='MountainsPlains',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$MountainsPlains, y=maxmindf$Clinton, col = "blue",
     xlab='MountainsPlains',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$South, y=maxmindf$Trump, col = "red",
     xlab='South',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$South, y=maxmindf$Clinton, col = "blue",
     xlab='South',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$Midwest, y=maxmindf$Trump, col = "red",
     xlab='Midwest',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$Midwest, y=maxmindf$Clinton, col = "blue",
     xlab='Midwest',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$Northeast, y=maxmindf$Trump, col = "red",
     xlab='Northeast',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$Northeast, y=maxmindf$Clinton, col = "blue",
     xlab='Northeast',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$Population, y=maxmindf$Trump, col = "red",
     xlab='Population',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$Population, y=maxmindf$Clinton, col = "blue",
     xlab='Population',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$PercentFemale, y=maxmindf$Trump, col = "red",
     xlab='PercentFemale',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$PercentFemale, y=maxmindf$Clinton, col = "blue",
     xlab='PercentFemale',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$PercentWhite, y=maxmindf$Trump, col = "red",
     xlab='PercentWhite',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$PercentWhite, y=maxmindf$Clinton, col = "blue",
     xlab='PercentWhite',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$PercentAfricanAmerican, y=maxmindf$Trump, col = "red",
     xlab='PercentAfricanAmerican',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$PercentAfricanAmerican, y=maxmindf$Clinton, col = "blue",
     xlab='PercentAfricanAmerican',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$PercentNativeAmerican, y=maxmindf$Trump, col = "red",
     xlab='PercentNativeAmerican',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$PercentNativeAmerican, y=maxmindf$Clinton, col = "blue",
     xlab='PercentNativeAmerican',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$PrecentAsian, y=maxmindf$Trump, col = "red",
     xlab='PrecentAsian',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$PrecentAsian, y=maxmindf$Clinton, col = "blue",
     xlab='PrecentAsian',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$PercentHawaianPI, y=maxmindf$Trump, col = "red",
     xlab='PercentHawaianPI',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$PercentHawaianPI, y=maxmindf$Clinton, col = "blue",
     xlab='PercentHawaianPI',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$PercentTwoorMore, y=maxmindf$Trump, col = "red",
     xlab='PercentTwoorMore',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$PercentTwoorMore, y=maxmindf$Clinton, col = "blue",
     xlab='PercentTwoorMore',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$PercentHispanic, y=maxmindf$Trump, col = "red",
     xlab='PercentHispanic',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$PercentHispanic, y=maxmindf$Clinton, col = "blue",
     xlab='PercentHispanic',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$Percent.White.Not.Hispanic, y=maxmindf$Trump, col = "red",
     xlab='Percent.White.Not.Hispanic',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$Percent.White.Not.Hispanic, y=maxmindf$Clinton, col = "blue",
     xlab='Percent.White.Not.Hispanic',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$Percent.foreign.born, y=maxmindf$Trump, col = "red",
     xlab='Percent.foreign.born',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$Percent.foreign.born, y=maxmindf$Clinton, col = "blue",
     xlab='Percent.foreign.born',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$PercentLangDiffEnglish, y=maxmindf$Trump, col = "red",
     xlab='PercentLangDiffEnglish',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$PercentLangDiffEnglish, y=maxmindf$Clinton, col = "blue",
     xlab='PercentLangDiffEnglish',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$Bachlorsorhigher, y=maxmindf$Trump, col = "red",
     xlab='Bachlorsorhigher',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$Bachlorsorhigher, y=maxmindf$Clinton, col = "blue",
     xlab='Bachlorsorhigher',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$HighSchoolGrad, y=maxmindf$Trump, col = "red",
     xlab='HighSchoolGrad',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$HighSchoolGrad, y=maxmindf$Clinton, col = "blue",
     xlab='HighSchoolGrad',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$HomeOwnership, y=maxmindf$Trump, col = "red",
     xlab='HomeOwnership',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$HomeOwnership, y=maxmindf$Clinton, col = "blue",
     xlab='HomeOwnership',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$PercentBelowPoverty, y=maxmindf$Trump, col = "red",
     xlab='PercentBelowPoverty',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$PercentBelowPoverty, y=maxmindf$Clinton, col = "blue",
     xlab='PercentBelowPoverty',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$PopPerSqMile, y=maxmindf$Trump, col = "red",
     xlab='PopPerSqMile',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$PopPerSqMile, y=maxmindf$Clinton, col = "blue",
     xlab='PopPerSqMile',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$IncomeperCapita, y=maxmindf$Trump, col = "red",
     xlab='IncomeperCapita',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$IncomeperCapita, y=maxmindf$Clinton, col = "blue",
     xlab='IncomeperCapita',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$IncomeperCapita, y=maxmindf$Trump, col = "red",
     xlab='IncomeperCapita',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$IncomeperCapita, y=maxmindf$Clinton, col = "blue",
     xlab='IncomeperCapita',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

# Training and test data set split 
ind<-sample(1:nrow(maxmindf), 2194) # Choose 1000 points randomly for the training set
train_data<-maxmindf[ind,] # create training set
test_data<-maxmindf[-ind,] # create test set

# keep the continuous output
# Fit a neural network with 2 hidden layers with 2 nodes each

# For Trump
Net.vote<-neuralnet(Trump ~ . - Clinton, hidden=c(3,2), data=train_data, act.fct="logistic",
                    stepmax=1e6, linear.output = TRUE)
plot(Net.vote)

#Evaluate neural network on the training set
predictions<-predict(Net.vote, train_data)

#Convert the data to "unnormalized"
unscalepred<- predictions*(max(train_data$Trump)-min(train_data$Trump) )+min(train_data$Trump) 
#MAE
mean(abs(unscalepred-train_data$Trump))
#CORRELATION
cor(train_data$Trump, unscalepred)

#Run model on test dataset
predictions<-predict(Net.vote,test_data)
#Get the result and unscale it
unscalepred<- predictions*(max(test_data$Trump)-min(test_data$Trump) )+min(test_data$Trump)
#MAE
mean(abs(unscalepred-test_data$Trump))
#CORRELATION
cor(test_data$Trump, unscalepred)

# For Clinton
Net.vote<-neuralnet(Clinton ~ . -Trump, hidden=c(3,2), data=train_data, act.fct="logistic",
                    stepmax=1e6, linear.output = TRUE)
plot(Net.vote)

#Evaluate neural network on the training set
predictions<-predict(Net.vote, train_data)

#Convert the data to "unnormalized"
unscalepred<- predictions*(max(train_data$Clinton)-min(train_data$Clinton) )+min(train_data$Clinton) 
#MAE
mean(abs(unscalepred-train_data$Clinton))
#CORRELATION
cor(train_data$Clinton, unscalepred)

#Run model on test dataset
predictions<-predict(Net.vote,test_data)
#Get the result and unscale it
unscalepred<- predictions*(max(test_data$Clinton)-min(test_data$Clinton) )+min(test_data$Clinton)
#MAE
mean(abs(unscalepred-test_data$Clinton))
#CORRELATION
cor(test_data$Clinton, unscalepred)
