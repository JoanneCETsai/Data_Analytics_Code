#The following code is similar to that used for forward selection, again using the scope argument to specify the most complicated model we’re willing to consider
frame<-read.csv("LifeExpPlus.csv", row.names = 1)
povinc<-frame$Inequity*frame$PovertyRate
step(lm(frame$AvgLifeExp ~1, data=frame),direction="both", scope= ~frame$logGDP+frame$Inequity +frame$PovertyRate+ frame$EducSpending+povinc)