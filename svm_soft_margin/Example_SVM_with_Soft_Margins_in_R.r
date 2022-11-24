library(e1071)

# Read in the entire data set
frame<-read.csv("LoanDataSVM.csv")
frame$Group=as.factor(frame$Group) # specify the Group variable as a factor
head(frame)

# Visualize the entire data set
plot(frame$CreditBorrowsScore,frame$DebttoIncome, col=frame$Group,
     pch=16,xlab='Credit Borrower Score',ylab='Debt to Income Ratio')
legend('topright',pch=16,col=c('red','black'),legend=c('Low','High'))

# Read in and plot the training data (70% of the full data set)
frame70<-read.csv("LoanDataSVM70.csv")
frame70$Group=as.factor(frame70$Group)
head(frame70)

plot(frame70$CreditBorrowsScore,frame70$DebttoIncome, col=frame70$Group,
     pch=16,xlab='Credit Borrower Score',ylab='Debt to Income Ratio')
legend('topright',pch=16,col=c('red','black'),legend=c('Low','High'))

# Construct Soft Margin SVM
result<-svm(Group~CreditBorrowsScore+DebttoIncome, kernel="linear", data=frame70)
print(result)

# Compute fitted values on the training data
predWorth70<-predict(result, data=frame70)

# Evaluate the model on the training data
table(predWorth70, frame70$Group)

# Visualize the SVM decision boundary
plot(result, frame70)

# Load in and visualize the test data set (30% of the entire data set)
frame30<-read.csv("LoanDataSVM30.csv")
frame30$Group=as.factor(frame30$Group)

plot(frame30$CreditBorrowsScore,frame30$DebttoIncome, col=frame30$Group,
     pch=16,xlab='Credit Borrower Score',ylab='Debt to Income Ratio')
legend('topright',pch=16,col=c('red','black'),legend=c('Low','High'))

# Compute predicted groups on the test data set
predWorth30<-predict(result, newdata=frame30)

# Evaluate predictions on the test data set
table(predWorth30, frame30$Group)

# Print model predictions for the test set
print(predWorth30)

