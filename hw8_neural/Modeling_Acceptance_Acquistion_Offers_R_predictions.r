# Load the objects from the previous R script
load('maxmindtrain') # scaled training data set
load('Net.Est') # neural network model
load('test_data') # unscaled test data set
load('train_data') # unscaled training data set

# Classify the training data points using the neural network
predictions<-round(predict(Net.Est, maxmindtrain))

# Confusion matrix for evaluating the model
table(train_data$Accept,predictions)

# Compute training set accuracy ( number correct classifications / number of data points )
sum(diag(table(train_data$Accept,predictions)))/nrow(train_data)

# Create the scaling function
normalize<-function(x) {return ((x-min(x))/(max(x)-min(x)))}

# Scale the test data
maxmindtest<-as.data.frame(lapply(test_data, normalize))

# Classify the test set data points
predictions<-round(predict(Net.Est, maxmindtest))

# Test data confusion matrix for evaluating the model's out of sample performance
table(test_data$Accept,predictions)

# Compute test set accuracy
sum(diag(table(test_data$Accept,predictions)))/nrow(test_data)
# test set accuracy is usually lower than training set accuracy
# if it is significantly lower then this suggests that we have overfit our model