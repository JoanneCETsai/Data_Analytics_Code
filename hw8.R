library(MASS)
library(neuralnet)

# load the data
frame<-read.csv("/Users/chiaentsai/Desktop/Data_Analytics/Data_Analytics_Code/hw8_neural/ElectionData.csv")
ncol(frame)
nrow(frame)
head(frame)

# normalize the data set
normalize<-function(x) {return ((x-min(x))/(max(x)-min(x)))}
df = subset(frame, select = -c(countystate, fips, US.regions))
maxmindf<-as.data.frame(lapply(df, normalize)) 

# check the scatter plot of features
plot(x=maxmindf$PacificCoast, y=maxmindf$Trump, col = "red",
     xlab='PacificCoast',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$PacificCoast, y=maxmindf$Clinton, col = "blue",
      xlab='PacificCoast',ylab='Trump/Clinton fraction')
legend("top",                                    
        legend=c("Trump","Clinton"),        
        col=c("red","blue"),                 
        lty=1,lwd=2)  


plot(x=maxmindf$MountainsPlains, y=maxmindf$Trump, col = "red",
     xlab='MountainsPlains',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$MountainsPlains, y=maxmindf$Clinton, col = "blue",
     xlab='MountainsPlains',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$South, y=maxmindf$Trump, col = "red",
     xlab='South',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$South, y=maxmindf$Clinton, col = "blue",
     xlab='South',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$Midwest, y=maxmindf$Trump, col = "red",
     xlab='Midwest',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$Midwest, y=maxmindf$Clinton, col = "blue",
     xlab='Midwest',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$Northeast, y=maxmindf$Trump, col = "red",
     xlab='Northeast',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$Northeast, y=maxmindf$Clinton, col = "blue",
     xlab='Northeast',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$Population, y=maxmindf$Trump, col = "red",
     xlab='Population',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$Population, y=maxmindf$Clinton, col = "blue",
     xlab='Population',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$PercentFemale, y=maxmindf$Trump, col = "red",
     xlab='PercentFemale',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$PercentFemale, y=maxmindf$Clinton, col = "blue",
     xlab='PercentFemale',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$PercentWhite, y=maxmindf$Trump, col = "red",
     xlab='PercentWhite',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$PercentWhite, y=maxmindf$Clinton, col = "blue",
     xlab='PercentWhite',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$PercentAfricanAmerican, y=maxmindf$Trump, col = "red",
     xlab='PercentAfricanAmerican',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$PercentAfricanAmerican, y=maxmindf$Clinton, col = "blue",
     xlab='PercentAfricanAmerican',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$PercentNativeAmerican, y=maxmindf$Trump, col = "red",
     xlab='PercentNativeAmerican',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$PercentNativeAmerican, y=maxmindf$Clinton, col = "blue",
     xlab='PercentNativeAmerican',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$PrecentAsian, y=maxmindf$Trump, col = "red",
     xlab='PrecentAsian',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$PrecentAsian, y=maxmindf$Clinton, col = "blue",
     xlab='PrecentAsian',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$PercentHawaianPI, y=maxmindf$Trump, col = "red",
     xlab='PercentHawaianPI',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$PercentHawaianPI, y=maxmindf$Clinton, col = "blue",
     xlab='PercentHawaianPI',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$PercentTwoorMore, y=maxmindf$Trump, col = "red",
     xlab='PercentTwoorMore',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$PercentTwoorMore, y=maxmindf$Clinton, col = "blue",
     xlab='PercentTwoorMore',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$PercentHispanic, y=maxmindf$Trump, col = "red",
     xlab='PercentHispanic',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$PercentHispanic, y=maxmindf$Clinton, col = "blue",
     xlab='PercentHispanic',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$Percent.White.Not.Hispanic, y=maxmindf$Trump, col = "red",
     xlab='Percent.White.Not.Hispanic',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$Percent.White.Not.Hispanic, y=maxmindf$Clinton, col = "blue",
     xlab='Percent.White.Not.Hispanic',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$Percent.foreign.born, y=maxmindf$Trump, col = "red",
     xlab='Percent.foreign.born',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$Percent.foreign.born, y=maxmindf$Clinton, col = "blue",
     xlab='Percent.foreign.born',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$PercentLangDiffEnglish, y=maxmindf$Trump, col = "red",
     xlab='PercentLangDiffEnglish',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$PercentLangDiffEnglish, y=maxmindf$Clinton, col = "blue",
     xlab='PercentLangDiffEnglish',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$Bachlorsorhigher, y=maxmindf$Trump, col = "red",
     xlab='Bachlorsorhigher',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$Bachlorsorhigher, y=maxmindf$Clinton, col = "blue",
     xlab='Bachlorsorhigher',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$HighSchoolGrad, y=maxmindf$Trump, col = "red",
     xlab='HighSchoolGrad',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$HighSchoolGrad, y=maxmindf$Clinton, col = "blue",
     xlab='HighSchoolGrad',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$HomeOwnership, y=maxmindf$Trump, col = "red",
     xlab='HomeOwnership',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$HomeOwnership, y=maxmindf$Clinton, col = "blue",
     xlab='HomeOwnership',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$PercentBelowPoverty, y=maxmindf$Trump, col = "red",
     xlab='PercentBelowPoverty',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$PercentBelowPoverty, y=maxmindf$Clinton, col = "blue",
     xlab='PercentBelowPoverty',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$PopPerSqMile, y=maxmindf$Trump, col = "red",
     xlab='PopPerSqMile',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$PopPerSqMile, y=maxmindf$Clinton, col = "blue",
     xlab='PopPerSqMile',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$IncomeperCapita, y=maxmindf$Trump, col = "red",
     xlab='IncomeperCapita',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$IncomeperCapita, y=maxmindf$Clinton, col = "blue",
     xlab='IncomeperCapita',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

plot(x=maxmindf$IncomeperCapita, y=maxmindf$Trump, col = "red",
     xlab='IncomeperCapita',ylab='Trump/Clinton fraction')
par(new=TRUE)
plot(x=maxmindf$IncomeperCapita, y=maxmindf$Clinton, col = "blue",
     xlab='IncomeperCapita',ylab='Trump/Clinton fraction')
legend("top",                                    
       legend=c("Trump","Clinton"),        
       col=c("red","blue"),                 
       lty=1,lwd=2)

# Training and test data set split 
ind<-sample(1:nrow(maxmindf), 2194) # Choose 1000 points randomly for the training set
train_data<-maxmindf[ind,] # create training set
test_data<-maxmindf[-ind,] # create test set

# keep the continuous output
# Fit a neural network with 2 hidden layers with 2 nodes each

# For Trump
Net.vote<-neuralnet(Trump ~ . - Clinton, hidden=c(3,2), data=train_data, act.fct="logistic",
                    stepmax=1e6, linear.output = TRUE)
plot(Net.vote)

#Evaluate neural network on the training set
predictions<-predict(Net.vote, train_data)

#Convert the data to "unnormalized"
unscalepred<- predictions*(max(train_data$Trump)-min(train_data$Trump) )+min(train_data$Trump) 
#MAE
mean(abs(unscalepred-train_data$Trump))
#CORRELATION
cor(train_data$Trump, unscalepred)

#Run model on test dataset
predictions<-predict(Net.vote,test_data)
#Get the result and unscale it
unscalepred<- predictions*(max(test_data$Trump)-min(test_data$Trump) )+min(test_data$Trump)
#MAE
mean(abs(unscalepred-test_data$Trump))
#CORRELATION
cor(test_data$Trump, unscalepred)

# For Clinton
Net.vote<-neuralnet(Clinton ~ . -Trump, hidden=c(3,2), data=train_data, act.fct="logistic",
                    stepmax=1e6, linear.output = TRUE)
plot(Net.vote)

#Evaluate neural network on the training set
predictions<-predict(Net.vote, train_data)

#Convert the data to "unnormalized"
unscalepred<- predictions*(max(train_data$Clinton)-min(train_data$Clinton) )+min(train_data$Clinton) 
#MAE
mean(abs(unscalepred-train_data$Clinton))
#CORRELATION
cor(train_data$Clinton, unscalepred)

#Run model on test dataset
predictions<-predict(Net.vote,test_data)
#Get the result and unscale it
unscalepred<- predictions*(max(test_data$Clinton)-min(test_data$Clinton) )+min(test_data$Clinton)
#MAE
mean(abs(unscalepred-test_data$Clinton))
#CORRELATION
cor(test_data$Clinton, unscalepred)


# Deep learning
library(corrplot)
library(psych)
library(ggplot2)
# Dataset is loaded
frame<-read.csv("/Users/chiaentsai/Desktop/Data_Analytics/Data_Analytics_Code/hw8_2/DC_PropertieResidentialunder1mill.csv")
summary(frame) 
# check the dimension of the frame
dim(frame)

# Distribution of some of the variables (one-hot encoded ones are not represented)
multi.hist(x = frame[, 1:12], dcol= c("blue", "red"), dlty = c("dotted", "solid")  )
multi.hist(x = frame[, 13:24], dcol= c("blue", "red"), dlty = c("dotted", "solid")  )

# Some scatterplots are obtained
ggplot(frame, aes(x=ROOMS, y=PRICE)) + geom_point()
ggplot(frame, aes(x=STORIES, y=PRICE)) + geom_point()
ggplot(frame, aes(x=EYB, y=PRICE)) + geom_point()
ggplot(frame, aes(x=YearSold, y=PRICE)) + geom_point()
ggplot(frame, aes(x=GrossBuildingArea, y=PRICE)) + geom_point()
ggplot(frame, aes(x=LANDAREA, y=PRICE)) + geom_point()

# Log Price will be used to see if we obtain better insights
ggplot(frame, aes(x=ROOMS, y=logPrice)) + geom_point()
ggplot(frame, aes(x=STORIES, y=logPrice)) + geom_point()
ggplot(frame, aes(x=EYB, y=logPrice)) + geom_point()
ggplot(frame, aes(x=YearSold, y=logPrice)) + geom_point()
ggplot(frame, aes(x=GrossBuildingArea, y=logPrice)) + geom_point()
ggplot(frame, aes(x=LANDAREA, y=logPrice)) + geom_point()

#####################Cleaning dataset#####################

# Eliminating NA
dim(frame)
# remove empty rows
frame <- na.omit(frame)
dim(frame)
frame_init <- frame

# Eliminating columns where all values are the same
summary(frame_init)
frame_init$Single <-  NULL 
frame_init$Residential_0.Condo_1 <-  NULL 
frame_init$MassachusettsAvenueHeights <-  NULL 
frame_init$Woodley <-  NULL 
frame_init$Kalorama <-  NULL 
frame
# Some categorical features are already broken down into one-hot encodings. They will be eliminated as well.
frame_init$ASSESSMENT_NBHD <-  NULL 
frame_init$QUADRANT <-  NULL 

# Some other categorical features have too many classes to be interpreted with one-hot encoding. 
# Initially, they will be eliminated altough other external information could be mapped to substitute them
# A possible example is neighbourhood rent per capita or crime rate. 
# Furthermore, some of the information is doubled in the dataset so it can be assumed that a few of the features
# do not provide much additional information and can contribute to overfitting.
frame_init$ASSESSMENT_SUBNBHD <-  NULL 
frame_init$CENSUS_BLOCK <-  NULL
frame_init$SQUARE <-  NULL 

# Computing Correlation matrix.
# Only selecting columns that are numeric variables.
nums <- unlist(lapply(frame_init, is.numeric)) 
frame_numeric <- frame_init[ , nums]
summary(frame_numeric)

frame_for_corr <- frame_numeric[,c("BATHROOMS", "ROOMS", "EYB", "YearSold", "PRICE", "GrossBuildingArea", "LANDAREA")]
#Look at the correlation matrix for all variables in this data set
cor(frame_for_corr, method="pearson") 
#Draw the correlation graph
corrplot.mixed(corr=cor(frame_for_corr, 
                        method="pearson"), tl.pos="lt", tl.srt=45, 
               addCoef.col = "black")


################# NORMALIZATION AND TEST/TRAIN SPLIT #############

# Take out log.price as we will be predicting PRICE
frame_init$logPrice <-  NULL 
summary(frame_init)

# Only the numeric columns will be normalized. The original dataframe is split into two
nums <- unlist(lapply(frame_init, is.numeric)) 
frame_numeric <- frame_init[ , nums]
frame_non_numeric <- frame_init[ , !nums]
#check if number of rows of the splitted datasets remain the same
dim(frame_non_numeric)[2] + dim(frame_numeric)[2] == dim(frame_init)[2]

# Data is normalized relative to the whole dataset (before doing the train-test split)
normalize<-function(x) {return ((x-min(x))/(max(x)-min(x)))}
# Normalize data except the first column which is property ID
frame_norm<-as.data.frame(lapply(frame_numeric[,-1], normalize))
summary(frame_norm)

# Combine the normalized numeric/non-numeric datasets
# First column will be reintroduced
# First column is previously removed when normalization
frame <- cbind(X.1=frame_numeric$X.1, frame_norm, frame_non_numeric) 
# check the dimensionality remain the same
dim(frame)[2]== dim(frame_init)[2]


# Test and training extracted
ind<-sample(1:nrow(frame), 0.7*nrow(frame))

train_data_counties <- frame[ind, "X.1"]
test_data_counties <- frame[-ind, "X.1"]

train_data<- frame[ind,2:ncol(frame)]
test_data<- frame[-ind,2:ncol(frame)]


# Create data set for analysis with LIME
for_lime<-sample(1:nrow(train_data), 5) # Pick 5 indices from the training set
data_for_lime<-train_data[for_lime,]
print(data_for_lime)

########################## H2O AND LIME #########################

install.packages("h2o", type = "binary")
install.packages("lime", type = "binary")

library(lime)
library(h2o)

# Fit a deep neural network with 2 hidden layers
h2o.init()
train_data<-as.h2o(train_data) # create training h2o data frame
test_data<-as.h2o(test_data) # use the remaining data for testing
?h2o.deeplearning

all_variables <- colnames(train_data)
input_variables <- all_variables[all_variables != "PRICE"]
setdiff(all_variables, input_variables)

# This part of the code does not run reliably on my local machine
# You could try it on Codio
# ===
hyper_params <- list(
    activation=c("Maxout", "Tanh"),
    hidden = list(c(2,2), c(3,2), c(3,3)),
    epochs=c(30,100,500),
    l1=c(0,.01),
    adaptive_rate=c(TRUE,FALSE))

dl_grid <- h2o.grid(algorithm = "deeplearning", 
                    x=input_variables,
                    y = "PRICE",
                    training_frame = train_data,
                    nfolds=10,         
                    standardize=TRUE,
                    hyper_params = hyper_params)
print(dl_grid)
# ===
#It would be useful to know which parameters are better for our dataset
# For now, a few different configurations are tried and a good model is chosen as the final output.

dl_Price <- h2o.deeplearning(y="PRICE",
                             x=input_variables,
                             training_frame = train_data,
                             validation_frame=test_data, hidden=c(2,2),
                             nfolds=5,
                             l1 = 0.01,
                             epochs = 500,
                             standardize=TRUE, 
                             seed=2)

# feel free to explore the parameters eg: activation function, nfolds ...
#dl_Price2 <- h2o.deeplearning(y="PRICE", 
#                                 training_frame = train_data, 
#                                 validation_frame=test_data, hidden=c(2,2), 
#                                 nfolds=5, 
#                                 activation="Maxout", 
#                                 adaptive_rate=TRUE, l1=0.01, 
#                                 epoch=20, standardize=TRUE, seed=33)


#Visualization
#1. Plot RMSE(Root-Mean-Square Error)~EPOCHS
plot(dl_Price)
#2. Variable Importance
h2o.varimp_plot(dl_Price, 7)
#3. Cross-Validation Metrics Summary
#Change in MAE, residual_deviance, rmse across different folds
summary(dl_Price)

#Prediction on Test Data
ValidationPredictions<-h2o.predict(dl_Price, test_data)
print(ValidationPredictions) 
dim(test_data)
dim(predictions)

# Unscale ValidationPredictions
unscalepred<- ValidationPredictions*(max(frame_init$PRICE)-min(frame_init$PRICE) )+min(frame_init$PRICE) 
unscaletestprice <- test_data$PRICE*(max(frame_init$PRICE)-min(frame_init$PRICE) )+min(frame_init$PRICE) 
n<- dim(unscaletestprice)[1]

# Compute rmse
rmse <- sqrt(sum( (unscalepred - unscaletestprice)^2)/n)
mean(unscaletestprice)

# RMSE compared to mean of test_data prices
rmse/mean(unscaletestprice)


# LIME
library(lime)
# Convert data_for_lime into an h2o data frame
print(data_for_lime) #check data_for_lime
predict_data_for_lime<-as.h2o(data_for_lime)

# Compute predictions with estimated neural network for the lime dataset
predictionsforlime<-h2o.predict(dl_Price, predict_data_for_lime)
print(predictionsforlime)

# Use lime to analyze the predictions 
explainer_accept <- lime(data_for_lime, dl_Price)
explanation <- explain(data_for_lime, explainer_accept, n_labels = 2, n_features = 4)

# Visualize the lime output
plot_features(explanation, ncol=3)
plot_explanations(explanation)
print(explanation)
