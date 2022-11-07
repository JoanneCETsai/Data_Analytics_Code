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
                  
#fit lda model using training data set
fit=lda(score ~ Termd+EmpStatusID+EmpSatisfaction+MechanicalApt+VerbalApt, data=frame)
print(fit)
plot(fit)
