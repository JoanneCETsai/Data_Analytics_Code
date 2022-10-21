#logit model
library(stats)
library(pscl)
frame<-read.csv("AquisitionAcceptance.csv")
head(frame)
result<-glm(Accept~Distance+Floodplain+HomeTenure+Education345+CurMarketValue+After+Price75+Price90+Price100+Price110+Price125, family=binomial(link="logit"), data=frame)
# The function glm() offers a way to fit models with a variety of error structures. Here, we specify # that we are doing a logit regression with the argument family=binomial(link="logit") summary(result) 

result<-glm(Accept~Distance+HomeTenure+Education345+After+Price75+Price90+Price110+Price125-1, family=binomial(link="logit"), data=frame) #This model includes only the variables that # appeared significant (based on p values) in our last model. We’ve also used the term -1 in the model to indicate that we don’t want to include the intercept
summary(result)
logLik(result) #calculate the log likelihood of this model
print(exp(coef(result))) # Use the exponents of the model coefficients to look at the amount each # coefficient contributes to the odds of the homeowner accepting an offer

pR2(result) #calculate mcFadden’s R^2

AfterPrice75<-frame$After*frame$Price75
frame$AfterPrice75=AfterPrice75

result<-glm(Accept~Distance+HomeTenure+Education345+After+Price75+Price90+Price110+Price125+frame$AfterPrice75-1, family=binomial(link="logit"), data=frame)
summary(result)
logLik(result)
print(exp(coef(result)))
pR2(result)

result<-glm(Accept~+After+Price75+Price90+Price110+Price125+frame$AfterPrice75-1, family=binomial(link="logit"), data=frame)
summary(result)
logLik(result)
print(exp(coef(result)))
pR2(result)
