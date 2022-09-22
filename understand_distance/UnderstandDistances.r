# Calculating distance
library(factoextra)
library(MASS)

#The code below calculates the distance between each pair of states based on their poverty rate and education spending

#using euclidean distance
frame<-read.csv("twoattributedist.csv", row.names=1) #load the .csv file that contains the data
head(frame) #Look at the first 6 rows of data
dist.euclidean<-get_dist(frame, method="euclidean") #calculate the euclidean distances
as.matrix(dist.euclidean) # look at the matrix of euclidean distances
fviz_dist(dist.euclidean) # look at the heatmap of euclidean distances

#using manhattan distance
dist.manhattan<-get_dist(frame, method="manhattan")
as.matrix(dist.manhattan)
fviz_dist(dist.manhattan)
