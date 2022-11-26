# neural network aquistion data
frame<-read.csv("AquisitionAcceptance.csv")
library(neuralnet)
attach(frame)
print(nrow(frame))
head(frame)

ind<-sample(1:nrow(frame), 1000)
train_data<-frame[ind,]
test_data<-frame[-ind,]

normalize<-function(x) {return ((x-min(x))/(max(x)-min(x)))}
maxmindtrain<-as.data.frame(lapply(train_data, normalize))

Net.Est<-neuralnet(Accept~Distance+HomeTenure+Education345+CurMarketValue+After+Price100+Price75+Price90+Price110+Price125, hidden=c(2,2), data=maxmindtrain)
plot(Net.Est)