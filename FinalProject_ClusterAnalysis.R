library(MASS)
library(factoextra)
library(maps)
library(mapdata)
library(ggplot2)
library(ggmap)

# part1.1
frame_noscale<-read.csv("/Users/chiaentsai/Desktop/Data_Analytics/hw4/hw4/TTI_dataset.csv", row.names=1)
frame <- scale(frame_noscale)
# euclidean
dist.euclidean<-get_dist(frame, method="euclidean")
dist.matrix <- as.matrix(dist.euclidean)
fviz_dist(dist.euclidean)
for (i in 1:nrow(dist.matrix)) {
  if (sum(dist.matrix[i,] > 8) > 20){
  print(row.names(dist.matrix)[i])
  }
}

for (i in 1:nrow(dist.matrix)) {
  if (sum(dist.matrix[i,] > 12) > 20){
    print(row.names(dist.matrix)[i])
  }
}
# manhattan
dist.manhattan<-get_dist(frame, method="manhattan")
dist.manhattan.matrix <- as.matrix(dist.manhattan)

fviz_dist(dist.manhattan)

for (i in 1:nrow(dist.manhattan.matrix)) {
  if (sum(dist.manhattan.matrix[i,] > 40) > 20){
    print(row.names(dist.manhattan.matrix)[i])
  }
}

for (i in 1:nrow(dist.manhattan.matrix)) {
  if (sum(dist.manhattan.matrix[i,] > 30) > 20){
    print(row.names(dist.manhattan.matrix)[i])
  }
}

frame_no_outlier <- frame[-c(51,62), ]
dist.no_outlier<-get_dist(frame_no_outlier, method="manhattan")
dist.matrix.no_outlier <- as.matrix(dist.no_outlier)


# part1.2
library(MASS)
library(factoextra)
# use Manhattan distance
hc.average <- hclust(dist.no_outlier, "average")
hc.single <- hclust(dist.no_outlier, "single")
hc.complete <- hclust(dist.no_outlier, "complete")
hc.centroid <- hclust(dist.no_outlier, "centroid")

fviz_dend(hc.average)
fviz_dend(hc.single)
fviz_dend(hc.complete)
fviz_dend(hc.centroid)

clusters_average <- cutree(hc.average,k=3)
clusters_single <- cutree(hc.single,k=3)
clusters_complete <- cutree(hc.complete,k=3)
clusters_centroid <- cutree(hc.centroid,k=3)

#pairs(frame_no_outlier,col=clusters_average)


pairs(frame_no_outlier[ , c(1,2,3)],
      col = clusters_complete)


# part2
#Load the packages you’ll use:
install.packages("sf")
install.packages("spdep")
install.packages("sp")
installed.packages("spData")
library("spData")
library("sp")
library("sf")
library("spdep") # This package contains the functions to calculate the Getis-Ord statistic. 
#### They are localG(), dnearneigh(), and nb2list().
library("ggplot2") # This package has functions to make good map images

NY_shp<-st_read("/Users/chiaentsai/Desktop/Data_Analytics/Data_Analytics_Code/hotspotanalysis/Counties.shp") 
# This loads a county map of New York state, in shape file format.

ggplot()+geom_sf(data=NY_shp, size=1, color="black", fill="white")


NCoro<-read.csv("/Users/chiaentsai/Desktop/Data_Analytics/Data_Analytics_Code/hotspotanalysis/NY_virusApril12020.csv") 
# This loads a .csv file of Coronavirus reported incidences per 10,000 as of April 1, 2020 by on the county name.

joined<-merge(NY_shp, NCoro, by.x="NAME", by.y="NAME") #This joins the Coronavirus dataset with the NY county shape file. 

ggplot()+
  geom_sf(data=joined, size=1, color="black", fill="white")+
  geom_sf(data=joined, aes(fill=joined$Population))# Creates a heatmap of Coronavirus cases by county. 

centroidscounties<-st_centroid(st_geometry(joined)) # Computes a centroid (the geographical center) for each county;


plot(coords) #UTM Zone 18, plots the centroid of each county
coords
Neigh<-dnearneigh(coords, 0, 90000) 
# identifies, for each county, the number of neighboring counties, where a county is considered a neighbor if the centroids are within about 90 miles, 
#note that the argument 90000 roughly corresponds to 90 mi, 
#and you could change this distance. 

plot.nb(Neigh, coords) # Plots coordinates with their connections to nearest neighbors. 

print(Neigh) #Print a summary of the Neigh data
card(Neigh) #List the number of neighbors each region has

ConCase<-localG(joined$Population, zero.policy=NULL, nb2listw(Neigh, style="W")) 
# The localG function returns the Getis-Ord statistics as z-scores. 
# Within this function, the nb2listw function associates a weight with each pair of counties that meet the threshold to be a neighbor using the function nb2listw(Neigh, style="W"). 
# In this example, Neigh stores which counties are considered neighbors based on the approximately 90-mile threshold. 
# The option W says that, for a given county, the weight associated each neighbor is 1 over the number of neighbors it has. 
# The function allows for the W coding scheme as well as 5 others. See the documentation for the function localG for more information about the different coding schemes.
print(ConCase)

ggplot()+
  geom_sf(data=joined)+
  geom_sf(data=joined, aes(fill=ConCase)) # Plot your z-scores as a heat map over NY counties
