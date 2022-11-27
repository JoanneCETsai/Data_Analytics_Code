#h20 for home acquisition data categorical output to show n-fold cross validation
library(h2o)
h2o.init()
frame<-read.csv("AquisitionAcceptance.csv")

frame<-as.h2o(frame)
frame[,"Accept"] <- as.factor(frame[,"Accept"])


dl_Aquisition <- h2o.deeplearning(y="Accept", x=c("Distance", "HomeTenure", "Education345", "CurMarketValue", "After", "Price100", "Price75", "Price90", "Price110", "Price125"), activation="Tanh", training_frame=frame, standardize=TRUE, epochs = 1000, nfolds=5,  hidden=c(4,4), seed=33) 
#nfolds = 5 means we will create 5 folds from our data.
plot(dl_Aquisition)
summary(dl_Aquisition)