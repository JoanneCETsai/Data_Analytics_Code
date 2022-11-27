library(h2o)

# Initialize h2o
h2o.init()

# Read in the dataset
frame<-read.csv("aquisitionacceptance.csv")
head(frame)

# Randomly pick 1000 data points to be part of the training dataset
ind<-sample(1:nrow(frame), 1000)

train_data<-as.h2o(frame[ind,]) # create training h2o data frame
test_data<-as.h2o(frame[-ind,]) # use the remaining data for testing

# Make sure that the output variable is identified as a factor in each data set
train_data[,"Accept"] <- as.factor(train_data[,"Accept"])
test_data[,"Accept"] <- as.factor(test_data[,"Accept"])

# Fit a deep neural network with 2 hidden layers
dl_Aquisition <- h2o.deeplearning(y="Accept",
                                  x=c("Distance", "HomeTenure", "Education345",
                                      "CurMarketValue", "After", "Price100",
                                      "Price75", "Price90", "Price110", "Price125"),
                                  training_frame = train_data,
                                  validation_frame=test_data, hidden=c(2,2),
                                  standardize=TRUE, seed=33)

plot(dl_Aquisition) # training and validation loss throughout training
summary(dl_Aquisition) # model summary information

# Use the test set for prediction
predictions<-h2o.predict(dl_Aquisition, test_data)
print(predictions) #notice prediction is not the greater of p0 and p1. function uses the F1 Score. If p1 greater then f1 then assigned 1 otherwise 0

# Evaluate test set performance
h2o.confusionMatrix(dl_Aquisition, test_data) # confusion matrix
h2o.table(predictions["predict"]) # frequency of predicted outcomes
h2o.table(test_data["Accept"]) # frequency of outcomes in test data set for comparison