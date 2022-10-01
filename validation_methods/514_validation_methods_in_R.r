# simple regression
frame<-read.csv("lifeexpectancy.csv", row.names = 1)
head(frame)
plot(x=frame$GDP.per.capita, y=frame$Average.Life.Expectancy)
plot(x=frame$log.GDP.per.capita, y=frame$Average.Life.Expectancy)
model<-lm(formula =frame$Average.Life.Expectancy ~ frame$log.GDP.per.capita, data=frame)
summary(model)
anova_model<-anova(model)
anova_model
library(MPV)
PRESS(model)

#k-fold validation
library(DAAG)
cvoutput<-cv.lm(data = frame, form.lm=formula(Average.Life.Expectancy ~ log.GDP.per.capita), m=5, dots = FALSE,seed=32, printit = TRUE)
