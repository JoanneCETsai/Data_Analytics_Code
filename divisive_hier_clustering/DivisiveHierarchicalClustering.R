library(MASS)
library(factoextra)
library(cluster)

#############################################################
# Read in the data
#############################################################

frameNoDC<-read.csv("FourAttributeDataNoDC.csv", row.names=1)
head(frameNoDC)

#############################################################
# Divisive Hierarchical Clustering 
#############################################################

# Compute distance matrix with euclidean distance
dist.euclideanNoDC<-get_dist(frameNoDC, method="euclidean")

fviz_dist(dist.euclideanNoDC) # visualize distance matrix with a heatmap

# Carry out divisive clustering algorithm
dc<-diana(dist.euclideanNoDC,diss=TRUE,metric="euclidean")

plot(dc) # Visualize dendrogram (folow prompts to hit return key in the console to print the plot)


clusters <- cutree(dc,k=5) # specify the desired number of clusters

#############################################################
# Examine and Analyze Clusters
#############################################################

plot(frameNoDC,col=clusters) # plot the clusters

states.coph<-cophenetic(dc) # distances from the dendrogram

fviz_dist(states.coph) # Visualize dendrogram distances with a heatmap

cor(get_dist(frameNoDC, method="euclidean"),states.coph) # correlation between dendrogram distances and euclidean distances


