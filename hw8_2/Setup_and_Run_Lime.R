#add lime
library(lime)
library(h2o)
h2o.init()
frame<-read.csv("AquisitionAcceptance.csv")
attach(frame)


ind<-sample(1:nrow(frame), 800)

trainv2<-frame[ind,]
for_lime<-sample(1:nrow(trainv2), 5) 
data_for_lime<-frame[for_lime,]
print(data_for_lime)

train_data<-as.h2o(frame[ind,]) 
test_data<-as.h2o(frame[-ind,])

train_data[,"Accept"] <- as.factor(train_data[,"Accept"])
test_data[,"Accept"] <- as.factor(test_data[,"Accept"])
dl_Aquisition <- h2o.deeplearning(y="Accept", x=c("Distance", "HomeTenure", "Education345", "CurMarketValue", "After", "Price100", "Price75", "Price90", "Price110", "Price125"), training_frame = train_data, validation_frame=test_data, hidden=c(2,2), activation="Maxout", adaptive_rate=TRUE, l1=0.01, epoch=30, standardize=TRUE, seed=33)
plot(dl_Aquisition)
summary(dl_Aquisition)

predict_data_for_lime<-as.h2o(data_for_lime)
predictionsforlime<-h2o.predict(dl_Aquisition, predict_data_for_lime)
print(predictionsforlime)

explainer_accept <- lime(data_for_lime, dl_Aquisition)
explanation <- explain(data_for_lime, explainer_accept, n_labels = 2, n_features = 4)
print(data_for_lime)
plot_features(explanation, ncol=3)
plot_explanations(explanation)
print(explanation)