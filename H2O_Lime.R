install.packages("corrplot", type = "binary")
install.packages("psych", type = "binary")

library(corrplot)
library(psych)
library(ggplot2)


# Dataset is loaded
frame<-read.csv("~/Desktop/DC_PropertieResidentialunder1mill.csv")
summary(frame) #LOG.PRICE or PRICE need to be taken out of the final model as they are the target variable
# check the dimension of the frame
dim(frame)

############# INITIAL EXPLORATION AND PLOTS ################

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

# Some categorical features are already broken down into one-hot encodings. They will be eliminated as well.
# to understand what is one-hot encodings, please refer to this link:
# https://datatricks.co.uk/one-hot-encoding-in-r-three-simple-methods

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
#hyper_params <- list(
#    activation=c("Maxout", "Tanh"),
#    hidden = list(c(2,2), c(3,2), c(3,3)),
#    epochs=c(30,100,500),
#    l1=c(0,.01),
#    adaptive_rate=c(TRUE,FALSE))

#dl_grid <- h2o.grid(algorithm = "deeplearning", 
#                    x=input_variables,
#                    y = "PRICE",
#                    training_frame = train_data,
#                    nfolds=10,         
#                    standardize=TRUE,
#                    hyper_params = hyper_params)
#print(dl_grid)
# ===
# It would be useful to know which parameters are better for our dataset
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


