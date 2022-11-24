library(e1071)

# Read in the data set
frame<-read.csv("LoanDataSVMultiClass.csv")
frame$Group=as.factor(frame$Group) # Convert the "Group" variable to a factor with 3 levels
print(frame)

# Compute the Multiclass SVM
result<-svm(Group~CreditBorrowsScore+DebttoIncome, kernel="linear", data=frame)
print(result)

# Compute fitted values
predictGroup<-predict(result, data=frame)

# Evaluate model performance
table(frame$Group, predictGroup) # only 1 misclassification!

#Visualize the decision boundaries for all 3 classes
plot(result, frame)