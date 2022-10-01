#Finding the "best" model with the backwards elimination method. Note that in this step algorithm the best model is chosen with AIC and that once a parameter is eliminated it is not added back in at any point. Lower AIC scores indicate a better model. 
frame<-read.csv("LifeExpPlus.csv", row.names = 1)
povinc<-frame$Inequity*frame$PovertyRate

#backward elimination
step(lm(frame$AvgLifeExp ~frame$logGDP+frame$Inequity +frame$PovertyRate
        + frame$EducSpending+povinc, data=frame), direction="backward")
