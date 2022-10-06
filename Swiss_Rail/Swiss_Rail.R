#logit model

#set the folder that the computer will look into
#Please change the path compatible with your computer
#setwd("C://Users/Linda Nozick/Desktop/ENMGT 5930 slides/Unit 5/New Logit Example")

#Please change the file path compatible with your computer
frame<-read.csv("~/Desktop/swiss_data.csv")
#see the first several lines of the data
head(frame)

#glm is used to fit generalized linear models, 
#specified by giving a symbolic description of the linear predictor and a description of the error distribution.
#run glm no representation of individual specific heterogeneity
summary(result<-glm(choice~tt1+tc1+hw1+ch1+tt2+tc2+hw2+ch2+hh_inc_abs+car_availability+half_discount_ticket+full_discount_ticket+commute+shopping, data=frame))

#download and install mlogic package
install.packages("mlogit",type = "binary")
library(mlogit)
# fit model with individual specific heterogeneity and see what is significant
summary(result<-mlogit(choice~0|tt1+tc1+hw1+ch1+tt2+tc2+hw2+ch2+hh_inc_abs+car_availability+half_discount_ticket+full_discount_ticket+commute+shopping, shape="wide", id="ID", data=frame))
# simplify to create reasonable model based on deleting variables that are not signficant (no exploring interaction, etc.)
summary(result<-mlogit(choice~0|tt1+tc1+hw1+ch1+tt2+tc2+hw2+ch2+car_availability-1, shape="wide", id="ID", data=frame))

#compute average for each independent variables
avg<-colMeans(frame)
print(avg)

#The basic syntax of an R function definition is:
#function_name <- function(arg_1, arg_2, ...) {
#                 Function body 
#                 }
#plot prob of route 1 verses value of tt1 assuming all other values are at their means
probR1 <- function(X){
  1/(1+exp(result$coefficients[1]*X+result$coefficients[2]*avg[2]+result$coefficients[3]*avg[3]+result$coefficients[4]*avg[4]+result$coefficients[5]*avg[5]+result$coefficients[6]*avg[6]+
             result$coefficients[7]*avg[7] +result$coefficients[8]*avg[8]+result$coefficients[9]*avg[10] ))
}
#plot the curve of probR1
curve(probR1, from=0, to=200, xlab="tt1", ylab="prob select Route 1")

#plot prob of route 1 based on tt1, tt2 
#and others are at average values
probR1XY <- function(X,Y){
  1/(1+exp(result$coefficients[1]*X+result$coefficients[2]*avg[2]+result$coefficients[3]*avg[3]+result$coefficients[4]*avg[4]+result$coefficients[5]*Y+result$coefficients[6]*avg[6]+
             result$coefficients[7]*avg[7] +result$coefficients[8]*avg[8]+result$coefficients[9]*avg[10] ))
}
#outer() function is used to apply a function to two arrays
#Syntax: outer(x, y, FUNCTION)
X<-1:100
Y<-1:100
z=outer(X,Y,probR1XY)
#persp(...) draws perspective plots of a surface over the x–y plane.
persp(X,Y,z, xlab = "tt1", ylab="tt2", zlab="Prob Route 1", theta=30, phi = 15, ticktype = "detailed")

#compute marginal of tt1 for each value of tt1 in the dataset assuming all the other values are at their means
MprobR1 <- function(X){-result$coefficients[1]*probR1(X)*(1-probR1(X))}
Margtt1<-MprobR1(frame$tt1)
plot(frame$tt1,Margtt1, ylab = "marginal increase in prob Route 1", xlab="tt1")


#compute marginal of tt1 for each individual in the dataset
IndprobR1XY <- function(X1, X2, X3, X4, X5, X6, X7, X8, X9){
  1/(1+exp(result$coefficients[1]*X1+result$coefficients[2]*X2+result$coefficients[3]*X3+result$coefficients[4]*X4+result$coefficients[5]*X5+result$coefficients[6]*X6+
             result$coefficients[7]*X7 +result$coefficients[8]*X8+result$coefficients[9]*X9 ))
}

IndMartt1<--result$coefficients[1]*IndprobR1XY(frame$tt1, frame$tc1, frame$hw1, frame$ch1, frame$tt2, frame$tc2, frame$hw2, frame$ch2, frame$car_availability)*(1-
                                  IndprobR1XY(frame$tt1, frame$tc1, frame$hw1, frame$ch1, frame$tt2, frame$tc2, frame$hw2, frame$ch2, frame$car_availability))

plot(frame$tt1,IndMartt1, ylab = "Marginal Effect of tt1 on Prob Selecting R1", xlab="tt1")

# average the marginal effects for each person of an increase of 1 in tt1
print(mean(IndMartt1))


#compute marginals for tt1 assuming each observation in the dataset has the dataset average for all other variables
IndMartt1<--result$coefficients[1]*IndprobR1XY(frame$tt1, avg[2], avg[3], avg[4], avg[5], avg[6], avg[7], avg[8], avg[10])*(1-IndprobR1XY(frame$tt1, avg[2], avg[3], avg[4], avg[5], avg[6], avg[7], avg[8], avg[10]))

plot(frame$tt1,IndMartt1, ylab = "Marginal Effect of tt1 on Prob Selecting R1", xlab="tt1")

# average the marginal effects for each person of an increase of 1 in tt1
print(mean(IndMartt1))





install.packages("margins",type="binary")
library(margins)

#glm works with margins BUT doesn't allow us to represent variability that is person specific
result<-glm(choice~tt1+tc1+hw1+ch1+tt2+tc2+hw2+ch2+car_availability-1, data=frame, family=binomial("logit"))

#marginal for each person in the dataset with respect to each variable in the dataset and then by variable take the average
print(logitmargins<-margins(result))


#add interaction terms to the model
summary(result<-mlogit(choice~0|tt1+tc1+hw1+ch1+tt2+tc2+hw2+ch2+hh_inc_abs+car_availability+half_discount_ticket+full_discount_ticket+commute+shopping+business, shape="wide", id="ID", data=frame))
summary(result<-mlogit(choice~0|tt1+tc1+hw1+ch1+tt2+tc2+hw2+ch2+hh_inc_abs+car_availability+half_discount_ticket+full_discount_ticket+commute+shopping+tt2*business, shape="wide", id="ID", data=frame))

summary(result<-mlogit(choice~0|tt1+tc1+hw1+ch1+tt2+tc2+hw2+ch2+car_availability+tt2*business+full_discount_ticket*tc1, shape="wide", id="ID", data=frame))





