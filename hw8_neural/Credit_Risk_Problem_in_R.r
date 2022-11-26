#Loan data neural network
library(MASS)
frame<-read.csv("LoanDataMultiClass.csv")
head(frame)
plot( frame$DebttoIncome, frame$CreditBorrowsScore, col=frame$Group)
fit=lda(Group~CreditBorrowsScore+DebttoIncome, data=frame)
print(fit)
pred=predict(fit, frame)
table(frame$Group, pred$class)

library(neuralnet)
attach(frame)


ind<-sample(1:nrow(frame), 35)
print(ind)
train_data<-frame[ind,]
test_data<-frame[-ind,]

normalize<-function(x) {return ((x-min(x))/(max(x)-min(x)))}
maxmindtrain<-as.data.frame(lapply(train_data, normalize))

Net.Est<-neuralnet(Group~DebttoIncome+CreditBorrowsScore, data=maxmindtrain, rep=5)
Net.best<-plot(Net.Est, rep="best")

predictions<-predict(Net.Est, maxmindtrain, rep = which.min(Net.Est$result.matrix[1,]))



unscalepred<- round(predictions*(max(train_data$Group)-min(train_data$Group) )+min(train_data$Group),0) 
table(train_data$Group,unscalepred)


maxmindtest<-as.data.frame(lapply(test_data, normalize))
predictions<-predict(Net.Est,maxmindtest, rep = which.min(Net.Est$result.matrix[1,]))
unscalepred<- round(predictions*(max(test_data$Group)-min(test_data$Group) )+min(test_data$Group),0) 
table(test_data$Group,unscalepred)