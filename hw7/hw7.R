library(MASS)

frame<-read.csv("/Users/chiaentsai/Desktop/Data_Analytics/Data_Analytics_Code/hw7/HRdata2groups.csv")
head(frame)


new_level <- list()
for (i in 1:nrow(frame)) {
  
  if (frame[i, ]$PerfScoreID==1 | frame[i, ]$PerfScoreID==2) {
    new_level <- append(new_level, 0)
  } else {
    new_level <- append(new_level, 1)
  }
}

frame$score <- new_level

#standardize
frame$Termd<-(frame$Termd-mean(frame$Termd))/sd(frame$Termd)
frame$EmpStatusID<-(frame$EmpStatusID-mean(frame$EmpStatusID))/sd(frame$EmpStatusID)
frame$EmpSatisfaction<-(frame$EmpSatisfaction-mean(frame$EmpSatisfaction))/sd(frame$EmpSatisfaction)
frame$MechanicalApt<-(frame$MechanicalApt-mean(frame$MechanicalApt))/sd(frame$MechanicalApt)
frame$VerbalApt<-(frame$VerbalApt-mean(frame$VerbalApt))/sd(frame$VerbalApt)

frame$score <- as.numeric(frame$score)
head(frame)

                  
#fit lda model using whole data set
fit=lda(score ~ Termd+EmpStatusID+EmpSatisfaction+MechanicalApt+VerbalApt, data=frame)
print(fit)
plot(fit)
predict(fit, frame)


library(rms)
ologit<-lrm(score ~ Termd+EmpStatusID+EmpSatisfaction+MechanicalApt+VerbalApt+1, data=frame)
print(ologit)
predict(ologit, frame)

ologit<-lrm(PerfScoreID  ~ Termd+EmpStatusID+EmpSatisfaction+MechanicalApt+VerbalApt+1, data=frame)
print(ologit)
predict(ologit, frame)

#find the probability each observation belongs to each of the three groups
fitteddata<-predict(ologit,data=frame, type = "fitted.ind") # type = “fitted.ind” indicates that you 
#want the output of this function to tell you the probability of each individual being in a each group
print(fitteddata)


install.packages('e1071')
library(e1071)
frame<-read.csv("/Users/chiaentsai/Desktop/Data_Analytics/Data_Analytics_Code/hw7_NaiveBayes/NaiveBayesHW.csv")
head(frame)
NBModel<-naiveBayes(PerfScore~MechanicalApt, data=frame)
print(NBModel)
pred<-predict(NBModel, frame, type="raw") #type = “raw” specifies that R should return the probability that a point is in each risk group. Not specifying a type would print the most likely category that each point would fall into. 
print(pred)
newdata<-read.csv("newdataNaiveBayes.csv")
print(newdata)
pred1<-predict(NBModel, newdata, type="raw")
print(pred1)





# classification tree
install.packages("grid")
frame<-read.csv("/Users/chiaentsai/Desktop/Data_Analytics/Data_Analytics_Code/hw7_classification_trees/HRdata4groups.csv")
print(frame)
library(grid)
library(partykit)
ctout <- ctree(PerfScoreID ~ . ,data=frame)
ctpred <- predict(ctout,frame)
print(ctpred) 
result <- round(ctpred, digits = 0)

mean(result == frame$PerfScoreID)  
plot(ctout) 

# svm 
library(plotly)
library(ggplot2)
library(e1071)
frame<-read.csv("/Users/chiaentsai/Desktop/Data_Analytics/Data_Analytics_Code/hw7_svm/aquisitionacceptanceSVM.csv")
head(frame)
frame$Accept=as.factor(frame$Accept)
result<-svm(Accept~., kernel="linear", data=frame)
pred<-predict(result, data=frame)
table(pred, frame$Accept)

print(result)
