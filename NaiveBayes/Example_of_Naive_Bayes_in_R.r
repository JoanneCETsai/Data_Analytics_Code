# Naive Bayes
library(e1071)
frame<-read.csv("CreditNaiveBayes.csv")
head(frame)
NBModel<-naiveBayes(Group~Credit.Borrows.Score+Debt.to.Income, data=frame)
print(NBModel)
pred<-predict(NBModel, frame, type="raw") #type = “raw” specifies that R should return the probability that a point is in each risk group. Not specifying a type would print the most likely category that each point would fall into. 
print(pred)
newdata<-read.csv("newdataNaiveBayes.csv")
print(newdata)
pred1<-predict(NBModel, newdata, type="raw")
print(pred1)
