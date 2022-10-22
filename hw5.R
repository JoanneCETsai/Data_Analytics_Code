#Part one
# Define data frame with the problem variables
df <- data.frame(demand=c(217,600,511,700,552,681,872,895,1087),
                 price= c(5,4.75,4.5,4.25,4,3.75,3.5,3.25,3))
print(df)

# Run linear regression
model <- lm(demand~price,data=df) #create linear model of demand as a function of price

# Print results
summary(model)
model

# Plot data and linear fit
plot(x = df$price, y = df$demand, col = 'blue', 
     pch = 16, cex = 1.3, main = 'Demand by Price'  ) # plot demand by price
##### the argument x specifies the x variable, y specifies the y variable
# col specifies color (type colors() into the console and press enter to see options)
# pch specifies the type of point
# cex specifies the point size,
# main specifies the title

abline(model,col='red',lw=3) #plots the best linear best fit on top of an existing plot

# Compute confidence intervals with new data
new = data.frame(price=c(3,4)) #the new prices we want predictions for

# Compute prediction intervals with new data
predict(model,newdata=new,interval='prediction',level=0.9) #predict the mean values of y 
## for the new data, interval = 'prediction' specifies that we want the prediction, level specifies that we want
## a 90% level of confidence

predict(model,newdata=new,interval='confidence',level=0.9) # calculates our confidence
## level for the mean value of y. here, interval = 'confidence specifies that we want to calculate the 
## confidence interval
print(model)

#anova
anova_model<-anova(model) 
summary(anova_model)
anova_model
tss = var(df$demand)*9
tss

#PRESS
install.packages("qpcR")
library("qpcR")
library("Matrix")
press <- PRESS(model)
summary(press)
sum(press$residuals^2) # same as $stat

# k-fold cross validation
cvoutput <- cv.lm(data = df, model, m=2,  printit = TRUE) #perform k-fold validation
cvoutput

# part2
library(leaps) # for best subsets
frame<-read.csv("/Users/chiaentsai/Desktop/Data_Analytics/Data_Analytics_Code/hw5_part2/ElectionData.csv", 
                row.names=1)

# Stepwise Selection
a <- step(lm(Clinton ~ . - fips - US.regions, data=frame), direction="both")
a$coefficients

#part5
# Read in data
df <- read.csv("/Users/chiaentsai/Desktop/Data_Analytics/Data_Analytics_Code/hw5_part5/AcquisitionAcceptance.csv")
colnames(df)
# Create interaction term between After and a Price variable of your choosing
# by replacing YOURVARHERE with the desired variable name
df$Interaction <- df$After * df$Price75

# Construct logistic regression with interaction term
model <- glm(Accept~Distance+HomeTenure+Education345+After+Price75+Price90+Price110+Price125+Interaction,
             family=binomial(link='logit'),data=df)

# Examine statistical significance of model coefficients
summary(model)

# Compute McFadden Pseudo R-Squared for Model Evaluation
pR2(model)['McFadden']











