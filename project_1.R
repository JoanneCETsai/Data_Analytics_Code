install.packages("carData")
install.packages("ggcorrplot")
library(dplyr)
library(tibble)
library(tree)
library(ISLR)
library(fastDummies)
library(DAAG)
library(ggplot2)
library(MASS)
library(factoextra)
library(carData)
library(car)
library(ggcorrplot)

# load the carseats data
carseats <- as_tibble(Carseats)
head(carseats)

# have a look of the data 
ncol(carseats)
nrow(carseats)

which(is.na(carseats))
sum(is.na(carseats)) 

summary(carseats)

# create dummy variables for categorical variables
carseats$Urban<-ifelse(carseats$Urban=="Yes",1,0) 
carseats$US<-ifelse(carseats$US=="Yes",1,0) 
carseats

frame <- dummy_cols(carseats, select_columns = "ShelveLoc")
frame <- subset(frame, select = -c(ShelveLoc) )
frame

# check multicolinearity
# remove ShelveLoc_Medium cause it is highly correlated with ShelveLoc_Good
corr <- cor(frame)
ggcorrplot(corr)
vif(lm(Sales ~ CompPrice + Income + Advertising + Population+Price+Age+Education
       + Urban + US + ShelveLoc_Bad + ShelveLoc_Good, data=frame))


# linear regression
model_linear<-lm(Sales ~., data=frame)
summary(model_linear)
anova_model<-anova(model_linear) 
anova_model

# k-fold validation for linear regression
cvoutput<-cv.lm(data = frame, model_linear, m=5,  printit = TRUE) 

pairs(Sales~ CompPrice + Income + Advertising +Population +Price+Age+Education , data = frame)

pairs(Sales~ CompPrice + Income + Advertising +Price+Education , data = frame)


# clustering
dist<-get_dist(frame, method="euclidean")
as.matrix(dist)
fviz_dist(dist) 

cluster<-kmeans(dist,centers=2,nstart=1)
plot(frame,col=cluster)
