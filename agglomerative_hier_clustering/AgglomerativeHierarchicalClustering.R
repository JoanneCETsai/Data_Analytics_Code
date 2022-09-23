library(MASS)
library(factoextra)

#############################################################
# Read in the data
#############################################################

frameNoDC <- read.csv("FourAttributeDataNoDC.csv", row.names=1)
head(frameNoDC)

#############################################################
## Agglomerative Hierarchical Clustering
#############################################################

# Compute distance matrix using Euclidean distance
dist.euclideanNoDC <- get_dist(frameNoDC, method="euclidean")

# Visualize distance matrix with a heatmap
fviz_dist(dist.euclideanNoDC)

# Carry out hierarchical clustering
hc <- hclust(get_dist(frameNoDC, method="euclidean"), "average")

fviz_dend(hc) # visualize cluster dendrogram

clusters <- cutree(hc,k=5) # choose the desired number of clusters and cut the tree

#############################################################
# Examine and analyze the resulting clusters
#############################################################

plot(frameNoDC,col=clusters) # plot the clusters

states.coph<-cophenetic(hc) # distances from the dendrogram

fviz_dist(states.coph) # Visualize dendrogram distances with a heatmap

cor(get_dist(frameNoDC, method="euclidean"),states.coph) # correlation between dendrogram distances and euclidean distances
