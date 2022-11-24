library(e1071)

#load the loan data set
frame70<-read.csv("LoanDataSVM70.csv")
frame70$Group=as.factor(frame70$Group)
head(frame70)
plot(frame70$CreditBorrowsScore,frame70$DebttoIncome, col=frame70$Group)

result<-svm(Group~CreditBorrowsScore+DebttoIncome, kernel="linear", 
            cost=20, data=frame70) #cost = 20
print(result)
predWorth70<-predict(result, data=frame70)
table(predWorth70, frame70$Group)

plot(result, frame70)

#Compare with misclassifications from no-cost SVM: 
result<-svm(Group~CreditBorrowsScore+DebttoIncome, kernel="linear", data=frame70) 
#note that the default cost is 1, so this model has a cost of 1 because no cost is specified
predWorth70<-predict(result, data=frame70)
table(predWorth70, frame70$Group)



frame30<-read.csv("LoanDataSVM30.csv")
frame30$Group=as.factor(frame30$Group)
plot(frame30$CreditBorrowsScore,frame30$DebttoIncome, col=frame30$Group)

predWorth30<-predict(result, newdata=frame30)
table(predWorth30, frame30$Group)
print(predWorth30)