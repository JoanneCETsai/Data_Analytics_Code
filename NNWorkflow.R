######################NN Workflow######################
install.packages("forecast", type = "binary")
install.packages("h2o", type = "binary")
install.packages("lime", type = "binary")
install.packages("neuralnet", type = "binary")

library(MASS) 
library(neuralnet) 
library(forecast)

frame<-read.csv("~/Desktop/ElectionData.csv",row.names = 1)
#Print Correlation Matrix for all variables
cor(frame)
#Create Scatterplots 
pairs(~Clinton+Bachlorsorhigher+Population+PercentWhite+
        PercentNativeAmerican+IncomeperCapita+HomeOwnership+
        PercentFemale,data=frame, main="Scatterplots for Election Data")


#split the data
split.at <- round(nrow(frame) * 0.7)
ind<-sample(1:nrow(frame), split.at)
train_data<-frame[ind,]
test_data<-frame[-ind,]

#Normalization
#Create normalizing function
normalize<-function(x) {return ((x-min(x))/(max(x)-min(x)))}
#Normalize on training dataset
#lapply(...):Apply a Function over a List or Vector
norm_train<-as.data.frame(lapply(train_data, normalize))

#Fit an NN on training set
?neuralnet
#Parameters:
#hidden=c(,,): the structure of hidden layer
#act.fct: activation function
#threshold: neuralnet requires the model partial derivative error to change at least 0.01 otherwise it will stop changing
#stepmax: how long your neural network trains. By default, it uses 100,000 iterations.
#rep: repeat training with different starting weights (assuming you haven't defined them already).
Net.Est<-neuralnet(Clinton~PercentFemale+Population+PercentWhite+IncomeperCapita, 
                   hidden=c(2,2), data=norm_train, act.fct='logistic')

# Visualize Neural Network model coefficients
plot(Net.Est)
Net.Est$result.matrix

#!!!OPTION 1: KEEP THE CONTINUOUS OUTPUT: PERCENTAGE OF VOTING
#Evaluate neural network on the training set
predictions<-predict(Net.Est, norm_train)
#Convert the data to "unnormalized"
unscalepred<- predictions*(max(train_data$Clinton)-min(train_data$Clinton) )+min(train_data$Clinton) 
#MAE
mean(abs(unscalepred-train_data$Clinton))
#CORRELATION
cor(train_data$Clinton, unscalepred)

#Run model on test dataset
#Normalize test dataset
norm_test<-as.data.frame(lapply(test_data, normalize))
#Run predict
predictions<-predict(Net.Est,norm_test)
#Get the result and unscale it
unscalepred<- predictions*(max(test_data$Clinton)-min(test_data$Clinton) )+min(test_data$Clinton)
#MAE
mean(abs(unscalepred-test_data$Clinton))
#CORRELATION
cor(test_data$Clinton, unscalepred)


#!!!OPTION 2: CONVERT THE CONTINUOUS OUTPUT TO BINARY
#Run prediction on norm_train
predictions<-predict(Net.Est,norm_train)
#round() the unscaled prediction
unscalepred<- round(predictions*(max(train_data$Clinton)-min(train_data$Clinton))+min(train_data$Clinton),0)
#create confusion matrix with the unscalepred and the original binary created by rounding
table(round(train_data$Clinton,0),unscalepred)
#accuracy rate
mean(round(train_data$Clinton,0)==unscalepred)

#Please evaluate NN on test set

