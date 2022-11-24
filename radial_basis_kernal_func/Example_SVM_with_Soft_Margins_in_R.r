library(e1071)

# Read in training data (70% of entire data set)
frame70<-read.csv("LoanDataSVM70.csv")

frame70$Group=as.factor(frame70$Group) # Identify the "Group" variable as a factor

head(frame70)

plot(frame70$CreditBorrowsScore,frame70$DebttoIncome, col=frame70$Group,
     pch=16,xlab='Credit Borrower Score',ylab='Debt to Income Ratio')
legend('topright',pch=16,col=c('red','black'),legend=c('Low','High'))

# Fit SVM margin with RBF Kernel
result<-svm(Group~CreditBorrowsScore+DebttoIncome, kernel="radial", gamma=0.5, cost=20, data=frame70)
print(result)

# Compute fitted values on training data
predWorth70<-predict(result, data=frame70)

# Evaluate model performance on training data
table(predWorth70, frame70$Group)

# Visualize the nonlinear decision boundary
plot(result, frame70)

# Read in and visualize the test data set
frame30<-read.csv("LoanDataSVM30.csv")
frame30$Group=as.factor(frame30$Group)

plot(frame30$CreditBorrowsScore,frame30$DebttoIncome, col=frame30$Group,
     pch=16,xlab='Credit Borrower Score',ylab='Debt to Income Ratio')
legend('topright',pch=16,col=c('red','black'),legend=c('Low','High'))

# Compute predicted values for the test set
predWorth30<-predict(result, newdata=frame30)
print(predWorth30)

# Evaluate model predictions on the test set
table(predWorth30, frame30$Group)

