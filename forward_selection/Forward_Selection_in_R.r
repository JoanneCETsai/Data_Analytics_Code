#forward selection

frame<-read.csv("LifeExpPlus.csv", row.names = 1)

povinc<-frame$Inequity*frame$PovertyRate

#The following code is similar to that used for backward selection, but the model is specified in a slightly different way because we are doing forward selection, so we have to specify the most complicated model we’d be willing to use. This is done with the scope argument
step(lm(frame$AvgLifeExp ~1, data=frame),direction="forward", scope= ~frame$logGDP+frame$Inequity +frame$PovertyRate+ frame$EducSpending+povinc)