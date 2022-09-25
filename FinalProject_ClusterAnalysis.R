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


