library(h2o)

# Initialize h2o
h2o.init()

# Read in the data
frame<-read.csv("AquisitionAcceptance.csv")
head(frame)

# Create test and training sets
ind<-sample(1:nrow(frame), 1000) # 1000 data points randomly picked for the training set
train_data<-as.h2o(frame[ind,]) # create training set as h2o data frame
test_data<-as.h2o(frame[-ind,]) # create test set as h2o data frame

# Identify the Accept variable as a factor in both data sets
train_data[,"Accept"] <- as.factor(train_data[,"Accept"])
test_data[,"Accept"] <- as.factor(test_data[,"Accept"])

# Estimate the deep neural network
dl_Aquisition <- h2o.deeplearning(y="Accept", x=c("Distance", "HomeTenure",
                                                  "Education345", "CurMarketValue",
                                                  "After", "Price100", "Price75",
                                                  "Price90", "Price110",
                                                  "Price125"),
                                  training_frame = train_data,
                                  validation_frame=test_data, hidden=c(2,2), 
                                  standardize=TRUE, seed=33)

# Visualize network coefficients
plot(dl_Aquisition)

# Print model summary information
summary(dl_Aquisition)

# Predict outputs on the test set
predictions<-h2o.predict(dl_Aquisition, test_data)
print(predictions) #notice prediction is not the greater of p0 and p1. function uses the F1 Score. If p1 greater then f1 then assigned 1 otherwise 0

# Confusion matrix for test set predictions
h2o.confusionMatrix(dl_Aquisition, test_data)

# Compare the results of the confusion matrix to the total number of predictions made for each outcome
h2o.table(predictions["predict"])

# and to the total number of true Accepts/Rejects in the test data set
h2o.table(test_data["Accept"])

