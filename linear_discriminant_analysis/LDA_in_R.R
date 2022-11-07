library(MASS)
frame<-read.csv("LoanData.csv")
head(frame)

#standardize
frame$stcreditBorrowsScore<-(frame$CreditBorrowsScore-mean(frame$CreditBorrowsScore))/sd(frame$CreditBorrowsScore)
frame$stDebttoIncome<-(frame$DebttoIncome-mean(frame$DebttoIncome))/sd(frame$DebttoIncome)

#create training and testing data sets
ind<-sample(1:nrow(frame), 40)
print(ind)
train_data<-frame[ind,]
test_data<-frame[-ind,]
print(test_data)

#fit lda model using training data set
fit=lda(Group~stcreditBorrowsScore+stDebttoIncome, data=train_data)
print(fit)
plot(fit)

#predict using test data set and evaluate accuracy
pred<-predict(fit, newdata=test_data)
print(pred)
print(pred$class)

CT<-table(test_data$Group, pred$class)
print(CT)
mean(test_data$Group == pred$class)

status_code<-ifelse(test_data$Group == pred$class, 1, ifelse(as.numeric(test_data$Group)>as.numeric(pred$class),2,3))
dis_data<-cbind(test_data, pred$class, status_code)
print(dis_data)

plot(dis_data$stcreditBorrowsScore, dis_data$stDebttoIncome, pch=16, col=dis_data$status_code)
legend(x="topright", legend=c("Correct","Low Cat as High", "High Cat as Low"), pch=16, col=1:3)
