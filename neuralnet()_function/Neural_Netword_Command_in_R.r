library(neuralnet)

# Read in the training data set
frame<-read.csv("LifeExpectancyTrainingNoNames.csv")

# Create normalization function
normalize<-function(x) {return ((x-min(x))/(max(x)-min(x)))}

# Scale the training data to range from 0 to 1
maxmindf<-as.data.frame(lapply(frame, normalize))

#############################################################
# Logistic Activation Functions
#############################################################

# Fit a neural network with 3 hidden layers and logistic activation functions
# the first layer has 2 nodes, second layer has 3 nodes, and the last layer has 1 node
Net.Est<-neuralnet(AverageLifeExpectancy~GDPpercapita, data=maxmindf, hidden=c(2,3,1), act.fct = "logistic") # Each number in the argument ‘hidden’ specifies the number of nodes in a particular layer, and the length of the vector specifies the number of nodes. The argument act.fct specifies the type of activation function you’d like to use. 

# Visualize the model and fitted coefficients. Does the structure match what you expected?
plot(Net.Est)

# Compute the fitted values
predictions<-predict(Net.Est,maxmindf)

# Unscale the model predictions
unscalepred<- predictions*(max(frame$AverageLifeExpectancy)-min(frame$AverageLifeExpectancy) )+min(frame$AverageLifeExpectancy) 

# Compute the mean absolute error between true and fitted values
mean(abs(unscalepred-frame$AverageLifeExpectancy))

# Correlation between true and fitted values
cor(frame$AverageLifeExpectancy, unscalepred)

#############################################################
# Hyperbolic Tangent Activation Functions
#############################################################

# Refit the same neural network on the same data but with tanh() activation functions
Net.Est<-neuralnet(AverageLifeExpectancy~GDPpercapita, data=maxmindf, hidden=c(2,3,1), act.fct = "tanh")

# Do you notice a significant change in the model coefficients?
plot(Net.Est)

# Compute fitted values
predictions<-predict(Net.Est,maxmindf)

# Unscale fitted values
unscalepred<- predictions*(max(frame$AverageLifeExpectancy)-min(frame$AverageLifeExpectancy) )+min(frame$AverageLifeExpectancy) 

# Compute mean absolute error between true and fitted values
mean(abs(unscalepred-frame$AverageLifeExpectancy))

# Correlation between true and fitted values
cor(frame$AverageLifeExpectancy, unscalepred)