#Random forest  with acquisition data
library(partykit)
frame<-read.csv("aquisitionacceptanceCART.csv")
head(frame)

cfout <- cforest(Accept ~ .,ntree=20, data=frame) # ntree specifies the number of trees used in the forest
cfpred <- predict(cfout, newdata=NULL) # newdata= NULL specifies that this prediction is not being done on a new dataset
mean(cfpred == frame$Accept) # do this command several times to see the variation in predictions
#Run lines 6-8 several times to observe the variation in the prediction accuracy obtained by different random forests