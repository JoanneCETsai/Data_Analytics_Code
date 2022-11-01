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
library(leaps)
library(psych)
library(GPArotation)

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
corr
ggcorrplot(corr)
vif(lm(Sales ~ CompPrice + Income + Advertising + Population+Price+Age+Education
       + Urban + US + ShelveLoc_Bad + ShelveLoc_Good, data=frame))

frame <- subset(frame, select = -c(ShelveLoc_Medium) )
frame
ncol(frame)

# linear regression & k-fold validation

model_linear<-lm(Sales ~., data=frame)
cvoutput<-cv.lm(data = frame, model_linear, m=5, printit = TRUE) 
summary(model_linear)
# calculate the residaul sum of squares
sum(resid(model_linear)^2)
deviance(model_linear)

# best subset selection

models<-regsubsets(Sales ~., data=frame, method = "exhaustive", nvmax=11)
choices<-summary(models) 
print(choices$which)
print(choices$adjr2) 
frame

#anova analysis

anova_model<-anova(model_linear) 
anova_model

#PCA

Sol<-princomp(frame, cor=TRUE, scores=TRUE)
print(Sol)
print(Sol$loadings) #print eigenvectors
fviz_eig(Sol) #Scree plot
Sol$sd^2#print eigenvalues
fviz_pca_var(Sol) #correlation circle
biplot(Sol) #biplot





