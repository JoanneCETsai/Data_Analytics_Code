library(neuralnet)

# Read in the data set
frame<-read.csv("AquisitionAcceptance.csv")
head(frame)

# Training and test data set split 
ind<-sample(1:nrow(frame), 1000) # Choose 1000 points randomly for the training set
train_data<-frame[ind,] # create training set
test_data<-frame[-ind,] # create test set

# Create the normalizing function
normalize<-function(x) {return ((x-min(x))/(max(x)-min(x)))}

# Scale the training data between 0 and 1
maxmindtrain<-as.data.frame(lapply(train_data, normalize))

# Fit a neural network with 2 hidden layers with 2 nodes each
# Due to the large number of data points this line of code will take several seconds to run
Net.Est<-neuralnet(Accept~Distance+HomeTenure+Education345+CurMarketValue+After+Price100+Price75+Price90+Price110+Price125, hidden=c(2,2), data=maxmindtrain)

# Visualize the estimated network coefficients
plot(Net.Est)

# We will want to reuse some data from this exercise in future units so we will save each one to a file 
save(maxmindtrain,file='maxmindtrain') # scaled training data
save(Net.Est,file='Net.Est') # neural network model
save(test_data,file='test_data') # test data
save(train_data,file='train_data') # training data