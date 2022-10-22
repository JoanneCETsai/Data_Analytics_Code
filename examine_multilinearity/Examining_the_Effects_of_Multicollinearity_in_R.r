#rebuild the model using Dummyvar
frame<-read.csv("LifeExpPlus.csv", row.names = 1)
cor(frame, method="pearson")
plot(x=frame$Inequity, y=frame$DummyVar)
model<-lm(formula =frame$AvgLifeExp ~frame$logGDP+frame$Inequity + frame$PovertyRate+DummyVar, data=frame) 
#notice p-values for Inequality and 
#DummyVar. Remember p-values are marginal tests
summary(model)

#drop DummyVar
model<-lm(formula =frame$AvgLifeExp ~frame$logGDP+frame$Inequity + frame$PovertyRate, data=frame)
summary(model)

#drop Inequality
model<-lm(formula =frame$AvgLifeExp ~frame$logGDP+frame$PovertyRate+DummyVar, data=frame)
summary(model)

library(car) # Load the library car so you can calculate the vif
vif(lm(frame$AvgLifeExp ~frame$logGDP+frame$Inequity + frame$PovertyRate+DummyVar, data=frame)) #calculate the VIF of the model with the Dummy variable
