library(MASS)
library(neuralnet)

# Read in the data
frame<-read.csv("LoanDataMultiClass.csv")
head(frame)

# Plot the data colored by which group each item is in
plot( frame$DebttoIncome, frame$CreditBorrowsScore, col=frame$Group,pch=16,
      xlab='Debt to Income Ratio',ylab='Credit Borrowers Score')

#############################################################################
# 1. Classify observations using LDA
#############################################################################

# Compute linear discriminant analysis classification model
fit=lda(Group~CreditBorrowsScore+DebttoIncome, data=frame)
print(fit)

# Classify the observations
pred=predict(fit, frame)

# Print a confusion matrix to evaluate model accuracy
lda_results <- table(frame$Group, pred$class)
lda_results
#############################################################################
# 2. Classify observations using Neural Networks
#############################################################################

## Training / Test Set Split
ind<-sample(1:nrow(frame), 35) # pick 35 random observations for the training set
train_data<-frame[ind,] # create training set
test_data<-frame[-ind,] # create test set

# Create normalizing function
normalize<-function(x) {return ((x-min(x))/(max(x)-min(x)))}

# Scale the training data to be between 0 and 1
maxmindtrain<-as.data.frame(lapply(train_data, normalize))

# Fit a neural network to predict group membership
# We will fit this model 5 times  (rep=5) and choose the best repetition
Net.Est<-neuralnet(Group~DebttoIncome+CreditBorrowsScore, data=maxmindtrain, rep=5) 

# Visualize model coefficients, rep = “best” means R will plot the repetition with the smallest errors
plot(Net.Est, rep="best")

## Evaluate neural network on the training set (n=35):

# Compute fitted values on the training data set
predictions<-predict(Net.Est, maxmindtrain, rep = which.min(Net.Est$result.matrix[1,])) #Note rep = which.min(Net.Est$result.matrix[1,])) explicitly tells R to pick the replicate with the lowest error from the Net.Est results matrix, because error is the first term in the Net.Est$results.matrix column, 

# Unscale the fitted values
unscalepred<- round(predictions*(max(train_data$Group)-min(train_data$Group) )+min(train_data$Group),0) 

# Confusion matrix to evaluate training accuracy
table(train_data$Group,unscalepred)

## Evaluate neural network on the test set (n=15):

# Scale the test set
maxmindtest<-as.data.frame(lapply(test_data, normalize))

# Compute predictions for the test set
predictions<-predict(Net.Est,maxmindtest, rep = which.min(Net.Est$result.matrix[1,]))

# Unscale the test set predictions
unscalepred<- round(predictions*(max(test_data$Group)-min(test_data$Group) )+min(test_data$Group),0) 

# Create confusion matrix to evaluate test set accuracy
table(test_data$Group,unscalepred)
