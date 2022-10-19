#k-fold validation full model without education spending
frame<-read.csv("LifeExpPlus.csv", row.names = 1)
povinc<-frame$Inequity*frame$PovertyRate
frame$povinc=povinc

sol<-lm(AvgLifeExp ~logGDP+Inequity +PovertyRate+povinc, data=frame) #note that 
# in this function, the data is specified as coming from the object frame, so $ sign notation is not necessary
print(sol) #model output
anova_sol<-anova(sol)
anova_sol #anova table for this model

library(DAAG)
fit<-lm(AvgLifeExp ~logGDP+Inequity +PovertyRate+povinc, data=frame)
cvoutput<-cv.lm(data = frame, fit, m=5,  printit = TRUE) #perform k-fold validation
