#h20 for aquisition data categorical output #show variable importance, confusion matrix and predict functionality,
library(h2o)
h2o.init()
frame<-read.csv("AquisitionAcceptance.csv")
attach(frame)
head(frame)

ind<-sample(1:nrow(frame), 1000)
train_data<-as.h2o(frame[ind,])
test_data<-as.h2o(frame[-ind,])

train_data[,"Accept"] <- as.factor(train_data[,"Accept"])
test_data[,"Accept"] <- as.factor(test_data[,"Accept"])
dl_Aquisition <- h2o.deeplearning(y="Accept", x=c("Distance", "HomeTenure", "Education345", "CurMarketValue", "After", "Price100", "Price75", "Price90", "Price110", "Price125"), training_frame = train_data, validation_frame=test_data, hidden=c(2,2), standardize=TRUE, seed=33)
plot(dl_Aquisition)
summary(dl_Aquisition)
predictions<-h2o.predict(dl_Aquisition, test_data)
print(predictions) #notice prediction is not the greater of p0 and p1. function uses the F1 Score. If p1 greater then f1 then assigned 1 otherwise 0

h2o.confusionMatrix(dl_Aquisition, test_data)
h2o.table(predictions["predict"])
h2o.table(test_data["Accept"])