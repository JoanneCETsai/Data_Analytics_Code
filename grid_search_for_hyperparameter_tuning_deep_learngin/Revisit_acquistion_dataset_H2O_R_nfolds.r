#h20 for acquisition data, categorical output  to show grid search
library(h2o)
h2o.init()
frame<-read.csv("AquisitionAcceptance.csv")


frame<-as.h2o(frame)
frame[,"Accept"] <- as.factor(frame[,"Accept"])

# Create a list of the hyperparameters you’d like to iterate through. Here, Professor Nozick 
# will be adjusting the hyperparameters activation function, network architecture, 
# iterations, tuning parameter, and whether the tuning parameter should be adaptive:
hyper_params <- list(
  activation=c("Maxout", "Tanh"),
  hidden = list(c(2,2), c(3,2), c(3,3), c(4,4)),
  epochs=c(30, 500, 1000, 3000),
  l1=c(0,.01),
  adaptive_rate=c(TRUE,FALSE))


#h2o grid search function: 

# The following command on lines 32-38 is very computationally intensive.
# It involves using 1366 observations to fit 2*4*4*2*2 = 128 models.
# Consequently, this is too much for this browser version of Rstudio to handle
# so instead we have included the results of h2o.grid() for you.
# Just run line 40 below to load the data and you will be able to then 
# print the results

# We encourage you to experiment with h2o on your personal computer!

#dl_grid <- h2o.grid(algorithm = "deeplearning", 
#                    x=c("Distance", "HomeTenure", "Education345", "CurMarketValue", "After", "Price100", "Price75", "Price90", "Price110", "Price125"),
#                    y = "Accept",
#                    training_frame = frame,
#                    nfolds=10,         
#                    standardize=TRUE,
#                    hyper_params = hyper_params)

load('~/workspace/dl_grid.RData')

print(dl_grid)