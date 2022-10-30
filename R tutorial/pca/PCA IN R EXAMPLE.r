#Gymnastics PCA analyis Vault and Beam
install.packages("factoextra")
library(factoextra)
library(ggplot2)
frame<-read.csv("GymnasticsDataVaultFloor.csv", row.names = 1)
print(frame)
Sol<-princomp(frame, cor=TRUE, scores=TRUE) #for simplicty, this just uses the package stats 
#use correlation matrix not covariance matrix
print(Sol)
print(Sol$loadings) #print eigenvectors
fviz_eig(Sol) #Scree plot
Sol$sd^2#print eigenvalues
fviz_pca_var(Sol) #correlation circle
biplot(Sol) #biplot
