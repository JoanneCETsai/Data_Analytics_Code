#ordered logistic regression
library(rms)

frame<-read.csv("/Users/chiaentsai/Desktop/Data_Analytics/Data_Analytics_Code/ordered_logit/LoadData3Groups.csv")
head(frame)

ologit<-lrm(Group~Credit.Borrows.Score+Debt.to.Income, data=frame)
print(ologit)

#find the probability each observation belongs to each of the three groups
fitteddata<-predict(ologit,data=frame, type = "fitted.ind") # type = “fitted.ind” indicates that you 
#want the output of this function to tell you the probability of each individual being in a each group
print(fitteddata)
frame2 <- data.frame(fitteddata)

#Gain insight into the marginal impacts of each of the variables
plot(frame$Credit.Borrows.Score, frame2$Group.1)
plot(frame$Credit.Borrows.Score, frame2$Group.2)
plot(frame$Credit.Borrows.Score, frame2$Group.3)

plot(frame$Debt.to.Income, frame2$Group.1)
plot(frame$Debt.to.Income, frame2$Group.2)
plot(frame$Debt.to.Income, frame2$Group.3)

#predit for a new potential customer
mydata=c(660, 0.35)
fitted<-predict(ologit, newdata=mydata, type="fitted.ind")
print(fitted)