#Louvain 
library(igraph)
library(network)

#Load network data
SmallLouvainex<-read.csv("Louvainsmallexample.csv",header=TRUE,row.names=1, check.names=T)
SmallL<-as.matrix(SmallLouvainex) # convert the data to a matrix
mode(SmallL)<-"numeric" # make sure the data is numeric
SmalLnet<-graph.adjacency(SmallL, mode="undirected", weighted=TRUE, diag=FALSE) #Calculate the distance matrix

Lou<-cluster_louvain(SmalLnet) #run the louvain algorithm
print(modularity(Lou, weighted=TRUE)) #calculate and print the modularity score
plot(SmalLnet, vertex.color=rainbow(3, alpha=0.6)[Lou$membership]) #visually examine
#cluster assignments
