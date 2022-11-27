library(lime)
library(h2o)

# Initialize h2o
h2o.init()

# Read in data set
frame<-read.csv("aquisitionacceptance.csv")

# Randomly select 800 indices to be in the training set
ind<-sample(1:nrow(frame), 800)

# Create training and test h2o data frames
train_data<-as.h2o(frame[ind,]) 
test_data<-as.h2o(frame[-ind,])

# Make sure output variable is identified as a factor in h2o
train_data[,"Accept"] <- as.factor(train_data[,"Accept"])
test_data[,"Accept"] <- as.factor(test_data[,"Accept"])

# Create data set for analysis with LIME
for_lime<-sample(1:nrow(frame[ind,]), 5) # Pick 5 indices from the training set
data_for_lime<-frame[for_lime,]
print(data_for_lime)

# Fit deep neural network
dl_Aquisition <- h2o.deeplearning(y="Accept", x=c("Distance", "HomeTenure",
                                                  "Education345", "CurMarketValue",
                                                  "After", "Price100", "Price75",
                                                  "Price90", "Price110", "Price125"),
                                  training_frame = train_data,
                                  validation_frame=test_data, hidden=c(2,2),
                                  activation="Maxout", adaptive_rate=TRUE,
                                  l1=0.01, epoch=30, standardize=TRUE, seed=33)

plot(dl_Aquisition) # training and test loss plotted throughout training
summary(dl_Aquisition) # print out model summary information and statistics

# Convert data_for_lime into an h2o data frame
predict_data_for_lime<-as.h2o(data_for_lime)

# Compute predictions with estimated neural network for the lime dataset
predictionsforlime<-h2o.predict(dl_Aquisition, predict_data_for_lime)
print(predictionsforlime)

# Use lime to analyze the predictions 
explainer_accept <- lime(data_for_lime, dl_Aquisition)
explanation <- explain(data_for_lime, explainer_accept, n_labels = 2, n_features = 4)

print(data_for_lime)

# Visualize the lime output
plot_features(explanation, ncol=3)
plot_explanations(explanation)

print(explanation)

