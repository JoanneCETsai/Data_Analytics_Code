# Load these packages so you can do the cluster analysis:
library(MASS)
library(factoextra)

# Load the data set: 
frameClusterPowerPointExample<-read.csv("ClusterPowerPointExample.csv", row.names=1) 
print(frameClusterPowerPointExample)

# Calculate distances: 

dist.euclideanClusterPowerPointExample<-get_dist(frameClusterPowerPointExample, method="euclidean") # Calculate distances
as.matrix(dist.euclideanClusterPowerPointExample) #look at the data as a matrix

fviz_dist(dist.euclideanClusterPowerPointExample) # look at the heatmap of the data as a matrix

#Cluster analysis example: 
set.seed(35)  #The algorithm is heuristic and starts with a random assignment. 
#### Setting the seed makes it  reproducible so that whenever 
#### the seed is the same, you’ll get the same results.
ExampleCluster<-kmeans(frameClusterPowerPointExample,centers=2,nstart=1) # centers is the #### number of clusters, nstart is the number of times you want the algorithm to run. 
str(ExampleCluster) #examine your results
plot(frameClusterPowerPointExample,col=ExampleCluster$cluster) #plot your results
points(ExampleCluster$centers, col=1:2,pch=8) # add the centers to your plot
#### Note that the function points adds to an existing plot. If you tried to use the 
#### plot function, you’d create a new plot with only the clusters on it. 
