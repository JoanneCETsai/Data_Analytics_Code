# Measure distance on a more complex data set with an outlier, even after the
# data have been scaled. 
library(factoextra)
library(MASS)

#Switch R to source materials from the correct working directory
cat("Working Directory:",getwd(),"\n\n")

#Measure euclidean distance on the data set with DC 
frame<-read.csv("FourAttributeData.csv", row.names=1)
print(frame)
dist.euclidean<-get_dist(frame, method="euclidean")
as.matrix(dist.euclidean)

write.matrix(dist.euclidean, file="euclidean.txt", sep=",")
fviz_dist(dist.euclidean)

#Measure euclidean distance on the data set without DC
frameNoDC<-read.csv("/Users/chiaentsai/Desktop/Data_Analytics/Data_Analytics_Code/effects_of_outliers/FourAttributeDataNoDC.csv", row.names=1)
print(frameNoDC)
dist.euclideanNoDC<-get_dist(frameNoDC, method="euclidean")
as.matrix(dist.euclideanNoDC)
write.matrix(dist.euclideanNoDC, file="euclideanNoDC.txt", sep=",")
fviz_dist(dist.euclideanNoDC)

##### Notice the difference in the 2 distance heatmaps you created. You can 
# You can switch between plots by clicking the arrow in the top right of the Plot panel. 
# You can also zoom in on the plot or export by clicking buttons at the top of the Plot panel. 