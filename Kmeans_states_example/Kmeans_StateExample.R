library(MASS)
library(factoextra)
library(maps)
library(mapdata)
library(ggplot2)
library(ggmap)

#############################################################
# Read in the data
#############################################################

frameNoDC<-read.csv("FourAttributeDataNoDC.csv", row.names=1)
head(frameNoDC)

#############################################################
# Cluster the data with Kmeans
#############################################################

dist.euclideanNoDC<-get_dist(frameNoDC, method="euclidean") # compute distance matrix

fviz_dist(dist.euclideanNoDC) # visualize distance matrix with a heatmap

set.seed(35) # set seed to guarantee reproducibility

# Run Kmeans with 2 clusters
ExampleClusterframeNoDC<-kmeans(frameNoDC,centers=2,nstart=15)
print(ExampleClusterframeNoDC)

# Visualize clusters
plot(frameNoDC,col=ExampleClusterframeNoDC$cluster)

#############################################################
# Visualize Clusters on a Map 
#############################################################

# Read in a data frame containing various clustering results and geographic info
framestates<-read.csv("states.csv")
head(framestates)
# geographic plotting information included in the framestates
# data frame can be obtained by calling this function:
# states<-map_data("state") 

# Plot cluster assignments geographically
ggplot(data=framestates)+ 
  geom_polygon(aes(x=long,y=lat,fill=factor(X3Clusters),group=group), color="white") +
  coord_fixed(1.3) + labs(fill='Cluster')

# To view the results from 2 or 4 clusters, change the fill 
# argument in geom_polygon to:
# fill = factor(X2Clusters)
# or 
# fill = factor(X4Clusters)

